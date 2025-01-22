const std = @import("std");
const ghostty = @import("ghostty");
const xev = @import("xev");
const vaxis = @import("vaxis");

const apprt = ghostty.apprt;
const builtin = std.builtin;
const input = ghostty.input;
const renderer = ghostty.renderer;
const terminal = ghostty.terminal;
const termio = ghostty.termio;
const vxfw = vaxis.vxfw;

const Allocator = std.mem.Allocator;
const Termio = termio.Termio;

const Terminal = @This();

const log = std.log.scoped(.terminal_widget);

pub const App = struct {
    vt: *Terminal,

    pub fn wakeup(self: *App) void {
        log.debug("app wakeup", .{});
        _ = self;
    }
};
pub const Surface = struct {};

pub const Options = struct {
    command: ?[]const u8 = null,
    term: []const u8 = "ghostty",
    size: vaxis.Winsize = .{ .cols = 80, .rows = 24, .x_pixel = 800, .y_pixel = 480 },
};

const Config = struct {};

gpa: Allocator,
io: Termio,
io_thread: termio.Thread,
io_thr: std.Thread,
renderer_state: renderer.State,
renderer_mutex: std.Thread.Mutex,
renderer_mailbox: *renderer.Thread.Mailbox,
renderer_thread: std.Thread,

config: Config = .{},

// The current rendered state
screen: vaxis.AllocatingScreen,

// We need an App declaration in order to work within Ghostty
app: App,
// The mailbox to receive messages as the "core" application
mailbox: *ghostty.App.Mailbox.Queue,

// We need a ghostty.Surface for our mailbox. This is never initialized and doesn't really do
// anything
core_surface: ghostty.Surface,

size: vaxis.Winsize,

loop: xev.Loop,
wakeup: xev.Async,
wakeup_c: xev.Completion = .{},

redraw: std.atomic.Value(bool),
quit: std.atomic.Value(bool),

cursor_state: ?vxfw.CursorState = null,

/// Intrusive init. We need a stable pointer for much of our init process
pub fn init(self: *Terminal, gpa: Allocator, opts: Options) !void {
    self.gpa = gpa;
    self.size = opts.size;
    self.mailbox = try ghostty.App.Mailbox.Queue.create(gpa);
    self.app = .{
        .vt = self,
    };

    self.screen = try vaxis.AllocatingScreen.init(gpa, opts.size.cols, opts.size.rows);

    self.redraw = std.atomic.Value(bool).init(false);
    self.quit = std.atomic.Value(bool).init(false);

    // Create our event loop.
    self.loop = try xev.Loop.init(.{});
    errdefer self.loop.deinit();

    self.wakeup = try xev.Async.init();
    errdefer self.wakeup.deinit();

    self.wakeup.wait(&self.loop, &self.wakeup_c, Terminal, self, queueRender);
    // Start our renderer thread
    self.renderer_thread = try std.Thread.spawn(
        .{},
        renderThread,
        .{self},
    );
    self.renderer_thread.setName("renderer") catch {};

    const full_config: ghostty.config.Config = .{};

    // Create an initial size
    const size: renderer.Size = .{
        .screen = .{ .height = opts.size.y_pixel, .width = opts.size.x_pixel },
        .cell = .{
            .height = opts.size.y_pixel / opts.size.rows,
            .width = opts.size.x_pixel / opts.size.cols,
        },
        .padding = .{},
    };

    // Create our IO thread
    self.io_thread = try termio.Thread.init(gpa);
    errdefer self.io_thread.deinit();

    // Create our mutex and assign it into the renderer_state
    self.renderer_mutex = .{};
    self.renderer_state = .{
        .mutex = &self.renderer_mutex,
        .terminal = &self.io.terminal,
    };

    // initialize our IO backend
    var io_exec = try termio.Exec.init(gpa, .{
        .command = opts.command,
        .shell_integration = full_config.@"shell-integration",
        .shell_integration_features = full_config.@"shell-integration-features",
        .working_directory = full_config.@"working-directory",
        .resources_dir = null,
        .term = opts.term,

        // TODO:cgroup management
        // .linux_cgroup = if (comptime builtin.os.tag == .linux and
        //     @hasDecl(apprt.runtime.Surface, "cgroup"))
        //     rt_surface.cgroup()
        // else
        //     Command.linux_cgroup_default,
    });
    errdefer io_exec.deinit();

    var io_mailbox = try termio.Mailbox.initSPSC(gpa);
    errdefer io_mailbox.deinit(gpa);

    // The mailbox for messaging the renderer
    self.renderer_mailbox = try renderer.Thread.Mailbox.create(gpa);
    errdefer self.renderer_mailbox.destroy(gpa);

    const app_mailbox: ghostty.App.Mailbox = .{
        .mailbox = self.mailbox,
        .rt_app = &self.app,
    };
    const surface_mailbox: ghostty.apprt.surface.Mailbox = .{
        .surface = &self.core_surface,
        .app = app_mailbox,
    };

    const termio_opts: termio.Options = .{
        .size = size,
        .full_config = &full_config,
        .config = try termio.Termio.DerivedConfig.init(gpa, &full_config),
        .backend = .{ .exec = io_exec },
        .renderer_state = &self.renderer_state,
        .renderer_wakeup = self.wakeup,
        .renderer_mailbox = self.renderer_mailbox,
        // TODO:
        .surface_mailbox = surface_mailbox,
        .mailbox = io_mailbox,
    };

    try termio.Termio.init(&self.io, gpa, termio_opts);
    errdefer self.io.deinit();

    // Start our IO thread
    self.io_thr = try std.Thread.spawn(
        .{},
        termio.Thread.threadMain,
        .{ &self.io_thread, &self.io },
    );
    self.io_thr.setName("io") catch {};

    self.io.queueMessage(.{ .resize = self.io.size }, .unlocked);
}

pub fn deinit(self: *Terminal) void {
    // Stop our IO thread
    {
        self.io_thread.stop.notify() catch |err|
            log.err("error notifying io thread to stop, may stall err={}", .{err});
        self.io_thr.join();
    }
    self.io_thread.deinit();
    self.io.deinit();
    self.io.config.deinit();
    renderer: {
        defer self.loop.stop();
        self.quit.store(true, .unordered);
        self.wakeup.notify() catch {
            self.renderer_thread.detach();
            break :renderer;
        };
        self.renderer_thread.join();
    }
    self.gpa.destroy(self.renderer_mailbox);
    self.wakeup.deinit();
    self.loop.deinit();

    self.screen.deinit(self.gpa);

    self.gpa.destroy(self.mailbox);
}

pub fn widget(self: *Terminal) vxfw.Widget {
    return .{
        .userdata = self,
        .eventHandler = typeErasedEventHandler,
        .drawFn = typeErasedDrawFn,
    };
}

fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
    const self: *Terminal = @ptrCast(@alignCast(ptr));
    return self.handleEvent(ctx, event);
}

pub fn handleEvent(self: *Terminal, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
    switch (event) {
        .init => try ctx.tick(8, self.widget()),
        .tick => {
            try self.drainAppMailbox();
            var iter = self.renderer_mailbox.drain();
            defer iter.deinit();
            while (iter.next()) |msg| {
                // TODO: renderermailbox?
                log.debug("renderer: {s}", .{@tagName(msg)});
            }
            ctx.redraw = true;
            try ctx.tick(8, self.widget());
        },
        .key_press, .key_release => |key| {
            if (key.matches('c', .{ .ctrl = true })) {
                ctx.quit = true;
                return;
            }
            const key_event = vaxisKeyToGhosttyKey(key, event == .key_press);
            try self.handleKeyEvent(ctx, key_event);
        },
        else => {},
    }
}

fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) Allocator.Error!vxfw.Surface {
    const self: *Terminal = @ptrCast(@alignCast(ptr));
    return self.draw(ctx);
}

pub fn draw(self: *Terminal, ctx: vxfw.DrawContext) Allocator.Error!vxfw.Surface {
    const max = ctx.max.size();

    var surface = try vxfw.Surface.init(
        ctx.arena,
        self.widget(),
        max,
    );

    self.renderer_mutex.lock();
    defer self.renderer_mutex.unlock();

    if (max.width != self.size.cols or max.height != self.size.rows) {
        // trigger resize
        self.size = .{
            .rows = max.height,
            .cols = max.width,
            .x_pixel = max.width * 8,
            .y_pixel = max.width * 16,
        };
        const size: renderer.Size = .{
            .screen = .{ .height = self.size.y_pixel, .width = self.size.x_pixel },
            .cell = .{
                .height = self.size.y_pixel / self.size.rows,
                .width = self.size.x_pixel / self.size.cols,
            },
            .padding = .{},
        };
        self.io.queueMessage(.{ .resize = size }, .locked);
        self.screen.deinit(self.gpa);
        self.screen = try vaxis.AllocatingScreen.init(self.gpa, self.size.cols, self.size.rows);
        return surface;
    }

    var row: u16 = 0;
    while (row < self.size.rows) : (row += 1) {
        var col: u16 = 0;
        while (col < self.size.cols) : (col += 1) {
            const cell = self.screen.readCell(col, row) orelse continue;
            surface.writeCell(col, row, cell);
        }
    }
    surface.cursor = self.cursor_state;
    return surface;
}

fn vaxisKeyToGhosttyKey(key: vaxis.Key, press: bool) input.KeyEvent {
    log.debug("vaxis key = {}", .{key});
    const action: input.Action = if (press) .press else .release;
    const physical_key = codepointToGhosttyKey(key.codepoint);
    const mods: input.Mods = .{
        .alt = key.mods.alt,
        .caps_lock = key.mods.caps_lock,
        .ctrl = key.mods.ctrl,
        .num_lock = key.mods.num_lock,
        .shift = key.mods.shift,
        .super = key.mods.super,
    };
    const consumed_mods = blk: {
        var consumed_mods: input.Mods = .{};
        // If we have a shifted codepoint, we have consumed shift
        if (key.shifted_codepoint != null) {
            consumed_mods.shift = true;
        }
        break :blk consumed_mods;
    };
    return .{
        .action = action,
        .key = physical_key,
        .physical_key = physical_key,
        .mods = mods,
        .consumed_mods = consumed_mods,
        .composing = false,
        .utf8 = if (key.text) |text| text else "",
        .unshifted_codepoint = key.codepoint,
        // TODO: .unshifted_codepoint,
    };
}

fn codepointToGhosttyKey(cp: u21) input.Key {
    return switch (cp) {
        ' ' => .space,
        ';' => .semicolon,

        'a' => .a,
        'b' => .b,
        'c' => .c,
        'd' => .d,
        'e' => .e,
        'f' => .f,
        'g' => .g,
        'h' => .h,
        'i' => .i,
        'j' => .j,
        'k' => .k,
        'l' => .l,
        'm' => .m,
        'n' => .n,
        'o' => .o,
        'p' => .p,
        'q' => .q,
        'r' => .r,
        's' => .s,
        't' => .t,
        'u' => .u,
        'v' => .v,
        'w' => .w,
        'x' => .x,
        'y' => .y,
        'z' => .z,

        '0' => .zero,
        '1' => .one,
        '2' => .two,
        '3' => .three,
        '4' => .four,
        '5' => .five,
        '6' => .six,
        '7' => .seven,
        '8' => .eight,
        '9' => .nine,

        vaxis.Key.enter => .enter,
        vaxis.Key.backspace => .backspace,

        vaxis.Key.up => .up,
        vaxis.Key.down => .down,
        vaxis.Key.right => .right,
        vaxis.Key.left => .left,

        vaxis.Key.left_shift => .left_shift,
        vaxis.Key.right_shift => .right_shift,
        else => .invalid,
    };
}

fn queueRender(
    maybe_self: ?*Terminal,
    _: *xev.Loop,
    _: *xev.Completion,
    r: xev.Async.WaitError!void,
) xev.CallbackAction {
    _ = r catch |err| {
        log.err("error in wakeup err={}", .{err});
        return .rearm;
    };

    const self = maybe_self orelse return .rearm;

    // Check if we should quit
    if (self.quit.load(.unordered)) return .disarm;

    self.redraw.store(true, .unordered);

    self.updateScreen() catch |err| {
        log.err("couldn't update screen={}", .{err});
        return .rearm;
    };

    return .rearm;
}

fn updateScreen(self: *Terminal) !void {
    var screen: terminal.Screen = critical: {
        // Take the lock in the critical path
        self.renderer_mutex.lock();
        defer self.renderer_mutex.unlock();

        const state = &self.renderer_state;

        // If we're in a synchronized output state, we pause all rendering.
        if (state.terminal.modes.get(.synchronized_output)) {
            log.debug("synchronized output started, skipping render", .{});
            return;
        }

        if (self.size.cols != state.terminal.cols or self.size.rows != state.terminal.rows) {
            // Skip a frame if our size doesn't match
            log.debug("size mismatch, skipping render frame", .{});
            return;
        }

        var screen = try state.terminal.screen.clone(self.gpa, .{ .viewport = .{} }, null);
        errdefer screen.deinit();

        // TODO: kitty images. See ghostty repo: src/renderer/OpenGL.zig:803

        // Set our cursor visible state while we have the lock
        if (state.terminal.modes.get(.cursor_visible)) {
            const blink = state.terminal.modes.get(.cursor_blinking);
            const shape: vaxis.Cell.CursorShape = switch (state.terminal.screen.cursor.cursor_style) {
                .bar => if (blink) .beam_blink else .beam,
                .block, .block_hollow => if (blink) .block_blink else .block,
                .underline => if (blink) .underline_blink else .underline,
            };
            self.cursor_state = .{
                .col = state.terminal.screen.cursor.x,
                .row = state.terminal.screen.cursor.y,
                .shape = shape,
            };
        } else {
            self.cursor_state = null;
        }

        break :critical screen;
    };

    defer screen.deinit();

    var row_iter = screen.pages.rowIterator(.right_down, .{ .viewport = .{} }, null);
    var row: u16 = 0;
    while (row_iter.next()) |pin| {
        defer row += 1;
        if (row >= self.screen.height) break;
        var col: u16 = 0;
        const cells = pin.cells(.all);
        for (cells) |cell| {
            defer col += 1;
            if (col >= self.screen.width) break;
            const vx_style: vaxis.Style = if (cell.hasStyling())
                ghosttyStyleToVaxisStyle(pin.style(&cell))
            else
                .{};
            if (cell.hasText()) {
                var buf: [256]u8 = undefined;
                // TODO: count codepoint lengths, allocate a buffer, and then write?
                var n = std.unicode.utf8Encode(cell.codepoint(), &buf) catch return error.OutOfMemory;
                if (cell.hasGrapheme()) {
                    if (pin.grapheme(&cell)) |cps| {
                        for (cps) |cp| {
                            n += std.unicode.utf8Encode(cp, buf[n..]) catch return error.OutOfMemory;
                        }
                    }
                }
                const char = buf[0..n];
                const width: u16 = @intCast(vxfw.DrawContext.stringWidth(undefined, char));
                self.screen.writeCell(col, row, .{
                    .char = .{
                        .grapheme = char,
                        .width = @intCast(width),
                    },
                    .style = vx_style,
                });
            } else {
                self.screen.writeCell(col, row, .{ .style = vx_style });
            }
        }
    }
}

fn handleKeyEvent(self: *Terminal, ctx: *vxfw.EventContext, event: input.KeyEvent) anyerror!void {
    // Take a lock on the renderer state
    self.renderer_state.mutex.lock();
    defer self.renderer_state.mutex.unlock();

    if (self.io.terminal.modes.get(.disable_keyboard)) return;

    const write_req = try self.encodeKey(event) orelse {
        log.warn("key couldn't be encoded: {}", .{event});
        return;
    };
    errdefer write_req.deinit();
    self.io.queueMessage(switch (write_req) {
        .small => |v| .{ .write_small = v },
        .stable => |v| .{ .write_stable = v },
        .alloc => |v| .{ .write_alloc = v },
    }, .unlocked);

    // If our event is any keypress that isn't a modifier and we generated
    // some data to send to the pty, then we move the viewport down to the
    // bottom. We also clear the selection for any key other then modifiers.
    if (!event.key.modifier()) {
        try self.setSelection(null);
        try self.io.terminal.scrollViewport(.{ .bottom = {} });
        try self.wakeup.notify();
    }

    return ctx.consumeEvent();
}

fn encodeKey(self: *Terminal, event: input.KeyEvent) !?termio.Message.WriteReq {
    // Build up our encoder. Under different modes and
    // inputs there are many keybindings that result in no encoding
    // whatsoever.
    const enc: input.KeyEncoder = enc: {
        const t = &self.io.terminal;

        break :enc .{
            .event = event,
            .macos_option_as_alt = .false,
            .alt_esc_prefix = t.modes.get(.alt_esc_prefix),
            .cursor_key_application = t.modes.get(.cursor_keys),
            .keypad_key_application = t.modes.get(.keypad_keys),
            .ignore_keypad_with_numlock = t.modes.get(.ignore_keypad_with_numlock),
            .modify_other_keys_state_2 = t.flags.modify_other_keys_2,
            .kitty_flags = t.screen.kitty_keyboard.current(),
        };
    };

    log.warn("kitty flags: {}", .{enc.kitty_flags});
    const write_req: termio.Message.WriteReq = req: {
        // Try to write the input into a small array. This fits almost
        // every scenario. Larger situations can happen due to long
        // pre-edits.
        var data: termio.Message.WriteReq.Small.Array = undefined;
        if (enc.encode(&data)) |seq| {
            // Special-case: we did nothing.
            if (seq.len == 0) return null;

            break :req .{ .small = .{
                .data = data,
                .len = @intCast(seq.len),
            } };
        } else |err| switch (err) {
            // Means we need to allocate
            error.OutOfMemory => {},
            else => return err,
        }

        // We need to allocate. We allocate double the UTF-8 length
        // or double the small array size, whichever is larger. That's
        // a heuristic that should work. The only scenario I know while
        // typing this where we don't have enough space is a long preedit,
        // and in that case the size we need is exactly the UTF-8 length,
        // so the double is being safe.
        const buf = try self.gpa.alloc(u8, @max(
            event.utf8.len * 2,
            data.len * 2,
        ));
        defer self.gpa.free(buf);

        // This results in a double allocation but this is such an unlikely
        // path the performance impact is unimportant.
        const seq = try enc.encode(buf);
        break :req try termio.Message.WriteReq.init(self.gpa, seq);
    };

    return write_req;
}

fn renderThread(self: *Terminal) void {
    self.loop.run(.until_done) catch |err| {
        log.err("err = {}", .{err});
    };
}

/// Set the selection contents.
///
/// This must be called with the renderer mutex held.
fn setSelection(self: *Terminal, sel: ?terminal.Selection) !void {
    try self.io.terminal.screen.select(sel);
}

fn ghosttyStyleToVaxisStyle(style: ghostty.terminal.Style) vaxis.Style {
    const fg: vaxis.Color = switch (style.fg_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    const bg: vaxis.Color = switch (style.bg_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    const ul: vaxis.Color = switch (style.underline_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    return .{
        .fg = fg,
        .bg = bg,
        .ul = ul,
        .ul_style = @enumFromInt(@intFromEnum(style.flags.underline)),
        .bold = style.flags.bold,
        .dim = style.flags.faint,
        .italic = style.flags.italic,
        .blink = style.flags.blink,
        .reverse = style.flags.inverse,
        .invisible = style.flags.invisible,
        .strikethrough = style.flags.strikethrough,
    };
}

fn drainAppMailbox(self: *Terminal) anyerror!void {
    while (self.mailbox.pop()) |message| {
        switch (message) {
            .open_config => {},
            .new_window => {},
            .close => {},
            .surface_message => |msg| try self.handleSurfaceMessage(msg.message),
            .redraw_surface => {},
            .redraw_inspector => {},
            .quit => {},
        }
    }
}

fn handleSurfaceMessage(self: *Terminal, msg: ghostty.apprt.surface.Message) anyerror!void {
    _ = self;
    log.debug("surface message: {s}", .{@tagName(msg)});
    // switch (msg) {
    // .change_config => {},
    //
    // .set_title => |*v| {
    //     // We ignore the message in case the title was set via config.
    //     if (self.config.title != null) {
    //         log.debug("ignoring title change request since static title is set via config", .{});
    //         return;
    //     }
    //
    //     // The ptrCast just gets sliceTo to return the proper type.
    //     // We know that our title should end in 0.
    //     const slice = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(v)), 0);
    //     log.debug("changing title \"{s}\"", .{slice});
    //     try self.rt_app.performAction(
    //         .{ .surface = self },
    //         .set_title,
    //         .{ .title = slice },
    //     );
    // },
    //
    // .report_title => |style| report_title: {
    //     if (!self.config.title_report) {
    //         log.info("report_title requested, but disabled via config", .{});
    //         break :report_title;
    //     }
    //
    //     const title: ?[:0]const u8 = self.rt_surface.getTitle();
    //     const data = switch (style) {
    //         .csi_21_t => try std.fmt.allocPrint(
    //             self.alloc,
    //             "\x1b]l{s}\x1b\\",
    //             .{title orelse ""},
    //         ),
    //     };
    //
    //     // We always use an allocating message because we don't know
    //     // the length of the title and this isn't a performance critical
    //     // path.
    //     self.io.queueMessage(.{
    //         .write_alloc = .{
    //             .alloc = self.alloc,
    //             .data = data,
    //         },
    //     }, .unlocked);
    // },
    //
    // .color_change => |change| {
    //     // Notify our apprt, but don't send a mode 2031 DSR report
    //     // because VT sequences were used to change the color.
    //     try self.rt_app.performAction(
    //         .{ .surface = self },
    //         .color_change,
    //         .{
    //             .kind = switch (change.kind) {
    //                 .background => .background,
    //                 .foreground => .foreground,
    //                 .cursor => .cursor,
    //                 .palette => |v| @enumFromInt(v),
    //             },
    //             .r = change.color.r,
    //             .g = change.color.g,
    //             .b = change.color.b,
    //         },
    //     );
    // },
    //
    // .set_mouse_shape => |shape| {
    //     log.debug("changing mouse shape: {}", .{shape});
    //     try self.rt_app.performAction(
    //         .{ .surface = self },
    //         .mouse_shape,
    //         shape,
    //     );
    // },
    //
    // .clipboard_read => |clipboard| {
    //     if (self.config.clipboard_read == .deny) {
    //         log.info("application attempted to read clipboard, but 'clipboard-read' is set to deny", .{});
    //         return;
    //     }
    //
    //     try self.startClipboardRequest(.standard, .{ .osc_52_read = clipboard });
    // },
    //
    // .clipboard_write => |w| switch (w.req) {
    //     .small => |v| try self.clipboardWrite(v.data[0..v.len], w.clipboard_type),
    //     .stable => |v| try self.clipboardWrite(v, w.clipboard_type),
    //     .alloc => |v| {
    //         defer v.alloc.free(v.data);
    //         try self.clipboardWrite(v.data, w.clipboard_type);
    //     },
    // },
    //
    // .pwd_change => |w| {
    //     defer w.deinit();
    //
    //     // We always allocate for this because we need to null-terminate.
    //     const str = try self.alloc.dupeZ(u8, w.slice());
    //     defer self.alloc.free(str);
    //
    //     try self.rt_app.performAction(
    //         .{ .surface = self },
    //         .pwd,
    //         .{ .pwd = str },
    //     );
    // },
    //
    // .close => self.close(),
    //
    // // Close without confirmation.
    // .child_exited => {
    //     self.child_exited = true;
    //     self.close();
    // },
    //
    // .desktop_notification => |notification| {
    //     if (!self.config.desktop_notifications) {
    //         log.info("application attempted to display a desktop notification, but 'desktop-notifications' is disabled", .{});
    //         return;
    //     }
    //
    //     const title = std.mem.sliceTo(&notification.title, 0);
    //     const body = std.mem.sliceTo(&notification.body, 0);
    //     try self.showDesktopNotification(title, body);
    // },
    //
    // .renderer_health => |health| self.updateRendererHealth(health),
    //
    // .report_color_scheme => |force| self.reportColorScheme(force),
    //
    // .present_surface => try self.presentSurface(),
    //
    // .password_input => |v| try self.passwordInput(v),
    // }
}
