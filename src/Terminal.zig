const std = @import("std");
const builtin = @import("builtin");
const ghostty = @import("ghostty");
const xev = @import("xev");
const vaxis = @import("vaxis");

const apprt = ghostty.apprt;
const input = ghostty.input;
const renderer = ghostty.renderer;
const terminal = ghostty.terminal;
const termio = ghostty.termio;
const vxfw = vaxis.vxfw;

const Allocator = std.mem.Allocator;
const Termio = termio.Termio;

const Terminal = @This();

const log = std.log.scoped(.terminal_widget);

/// This is an empty Decl meant to satisfy libghostty
pub const App = struct {
    /// Libghostty requires this method
    pub fn wakeup(_: *App) void {}
};

/// This is an empty Decl meant to satisfy libghostty
pub const Surface = struct {};

/// This is an empty Decl meant to satisfy libghostty
pub const Renderer = struct {
    pub const DerivedConfig = struct {};
};

pub const Options = struct {
    command: ?[]const u8 = null,
    term: []const u8 = "ghostty",
    size: vaxis.Winsize = .{ .cols = 80, .rows = 24, .x_pixel = 800, .y_pixel = 480 },
    shell_integration_features: ghostty.config.ShellIntegrationFeatures = .{
        .cursor = false,
        .sudo = false,
        .title = true,
    },
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
screen_mutex: std.Thread.Mutex,

// We need an App declaration in order to work within Ghostty
app: App,
// The mailbox to receive messages as the "core" application
mailbox: *ghostty.App.Mailbox.Queue,

// We need a ghostty.Surface for our mailbox. This is never initialized and doesn't really do
// anything
core_surface: ghostty.Surface,

size: renderer.Size,

loop: xev.Loop,
wakeup: xev.Async,
wakeup_c: xev.Completion = .{},

redraw: std.atomic.Value(bool),
quit: std.atomic.Value(bool),
resize_msg_sent: std.atomic.Value(bool),

cursor_state: ?vxfw.CursorState = null,
mouse_shape: vaxis.Mouse.Shape = .default,
password_input: bool = false,

/// Initial command this terminal was started with
command: []const u8,

/// Resources directory, if we found one
resources_dir: ?[]const u8,

/// Title of the terminal
title: []const u8,

/// If the child process in the terminal has exited
child_exited: bool = false,

/// Intrusive init. We need a stable pointer for much of our init process
pub fn init(self: *Terminal, gpa: Allocator, opts: Options) !void {
    self.gpa = gpa;
    self.size = .{
        .screen = .{ .height = opts.size.y_pixel, .width = opts.size.x_pixel },
        .cell = .{
            .height = opts.size.y_pixel / opts.size.rows,
            .width = opts.size.x_pixel / opts.size.cols,
        },
        .padding = .{},
    };
    self.mailbox = try ghostty.App.Mailbox.Queue.create(gpa);
    self.app = .{};
    self.screen_mutex = .{};

    self.screen = try vaxis.AllocatingScreen.init(gpa, opts.size.cols, opts.size.rows);

    self.redraw = std.atomic.Value(bool).init(false);
    self.quit = std.atomic.Value(bool).init(false);
    self.resize_msg_sent = std.atomic.Value(bool).init(false);

    self.title = "";

    // Create our event loop.
    self.loop = try xev.Loop.init(.{});
    errdefer self.loop.deinit();

    self.wakeup = try xev.Async.init();
    errdefer self.wakeup.deinit();

    self.wakeup.wait(&self.loop, &self.wakeup_c, Terminal, self, render);
    // Start our renderer thread
    self.renderer_thread = try std.Thread.spawn(
        .{},
        renderThread,
        .{self},
    );
    self.renderer_thread.setName("renderer") catch {};

    const full_config: ghostty.config.Config = .{};

    // Create our IO thread
    self.io_thread = try termio.Thread.init(gpa);
    errdefer self.io_thread.deinit();

    // Create our mutex and assign it into the renderer_state
    self.renderer_mutex = .{};
    self.renderer_state = .{
        .mutex = &self.renderer_mutex,
        .terminal = &self.io.terminal,
    };

    self.command = if (opts.command) |cmd|
        // Dupe it so we don't have to track whether we allocated from shell or not
        try gpa.dupe(u8, cmd)
    else
        try getShell(gpa);

    self.resources_dir = try ghostty.os.resourcesDir(gpa);

    // initialize our IO backend
    var io_exec = try termio.Exec.init(gpa, .{
        .command = self.command,
        .shell_integration = full_config.@"shell-integration",
        .shell_integration_features = full_config.@"shell-integration-features",
        .working_directory = full_config.@"working-directory",
        .resources_dir = self.resources_dir,
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
        .size = self.size,
        .full_config = &full_config,
        .config = try termio.Termio.DerivedConfig.init(gpa, &full_config),
        .backend = .{ .exec = io_exec },
        .renderer_state = &self.renderer_state,
        .renderer_wakeup = self.wakeup,
        .renderer_mailbox = self.renderer_mailbox,
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

    self.renderer_mutex.lock();
    defer self.renderer_mutex.unlock();
    self.io.queueMessage(.{ .resize = self.io.size }, .locked);
}

pub fn deinit(self: *Terminal) void {
    // Stop our IO thread
    {
        self.io_thread.stop.notify() catch |err|
            log.err("error notifying io thread to stop, may stall err={}", .{err});
        self.io_thr.join();
    }
    renderer: {
        defer self.loop.stop();
        self.quit.store(true, .unordered);
        self.wakeup.notify() catch {
            self.renderer_thread.detach();
            break :renderer;
        };
        self.renderer_thread.join();
    }
    self.io_thread.deinit();
    self.io.deinit();
    self.io.config.deinit();
    self.gpa.destroy(self.renderer_mailbox);
    self.wakeup.deinit();
    self.loop.deinit();

    self.screen_mutex.lock();
    defer self.screen_mutex.unlock();
    self.screen.deinit(self.gpa);

    self.gpa.destroy(self.mailbox);
    self.gpa.free(self.command);
    if (self.resources_dir) |resources_dir| {
        self.gpa.free(resources_dir);
    }

    self.gpa.free(self.title);
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
            try self.drainAppMailbox(ctx);
            try self.drainRendererMailbox();
            // Set redraw if it was false
            ctx.redraw = ctx.redraw or self.redraw.load(.unordered);
            try ctx.tick(8, self.widget());
        },
        .key_press, .key_release => |key| {
            const key_event = vaxisKeyToGhosttyKey(key, event == .key_press);
            try self.handleKeyEvent(ctx, key_event);
        },
        .mouse => |_| try self.handleMouseEvent(ctx, event.mouse),
        .mouse_enter => try ctx.setMouseShape(self.mouse_shape),
        .mouse_leave => try ctx.setMouseShape(.default),
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
    surface.focusable = true;

    self.screen_mutex.lock();
    defer self.screen_mutex.unlock();
    self.redraw.store(false, .unordered);

    const grid = self.size.grid();
    if (max.width != grid.columns or max.height != grid.rows) {
        log.debug("size mismatch in draw: w={d}, h={d}, w_pix={d}, h_pix={d}, new: w={d}, h={d}, w_pix={d}, h_pix={d}", .{
            grid.columns,
            grid.rows,
            self.size.cell.width,
            self.size.cell.height,
            max.width,
            max.height,
            ctx.cell_size.width,
            ctx.cell_size.height,
        });
        const size: renderer.Size = .{
            .screen = .{
                .height = max.height * ctx.cell_size.height,
                .width = max.width * ctx.cell_size.width,
            },
            .cell = .{
                .height = ctx.cell_size.height,
                .width = ctx.cell_size.width,
            },
            .padding = .{},
        };
        try self.notifyResize(size);
        return surface;
    }

    var row: u16 = 0;
    while (row < grid.rows) : (row += 1) {
        var col: u16 = 0;
        while (col < grid.columns) : (col += 1) {
            var cell = self.screen.readCell(col, row) orelse continue;
            // Dupe the contents of the grapheme because the lifetime can change before it is
            // rendered
            cell.char.grapheme = try ctx.arena.dupe(u8, cell.char.grapheme);
            cell.link.uri = try ctx.arena.dupe(u8, cell.link.uri);
            cell.link.params = try ctx.arena.dupe(u8, cell.link.params);
            surface.writeCell(col, row, cell);
        }
    }
    switch (self.password_input) {
        true => if (self.cursor_state) |state| {
            surface.writeCell(state.col, state.row, .{ .char = .{ .grapheme = "" } });
            surface.writeCell(state.col + 1, state.row, .{});
        },
        false => surface.cursor = self.cursor_state,
    }
    return surface;
}

fn vaxisKeyToGhosttyKey(key: vaxis.Key, press: bool) input.KeyEvent {
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
    };
}

fn codepointToGhosttyKey(cp: u21) input.Key {
    return switch (cp) {
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

        ';' => .semicolon,
        ' ' => .space,
        '\'' => .apostrophe,
        ',' => .comma,
        '`' => .grave_accent,
        '.' => .period,
        '/' => .slash,
        '-' => .minus,
        '+' => .plus,
        '=' => .equal,
        '[' => .left_bracket,
        ']' => .left_bracket,
        '\\' => .backslash,

        vaxis.Key.up => .up,
        vaxis.Key.down => .down,
        vaxis.Key.right => .right,
        vaxis.Key.left => .left,
        vaxis.Key.home => .home,
        vaxis.Key.end => .end,
        vaxis.Key.insert => .insert,
        vaxis.Key.delete => .delete,
        vaxis.Key.caps_lock => .caps_lock,
        vaxis.Key.scroll_lock => .scroll_lock,
        vaxis.Key.num_lock => .num_lock,
        vaxis.Key.page_up => .page_up,
        vaxis.Key.page_down => .page_down,
        vaxis.Key.escape => .escape,
        vaxis.Key.enter => .enter,
        vaxis.Key.tab => .tab,
        vaxis.Key.backspace => .backspace,
        vaxis.Key.print_screen => .print_screen,
        vaxis.Key.pause => .pause,

        vaxis.Key.f1 => .f1,
        vaxis.Key.f2 => .f2,
        vaxis.Key.f3 => .f3,
        vaxis.Key.f4 => .f4,
        vaxis.Key.f5 => .f5,
        vaxis.Key.f6 => .f6,
        vaxis.Key.f7 => .f7,
        vaxis.Key.f8 => .f8,
        vaxis.Key.f9 => .f9,
        vaxis.Key.f10 => .f10,
        vaxis.Key.f11 => .f11,
        vaxis.Key.f12 => .f12,
        vaxis.Key.f13 => .f13,
        vaxis.Key.f14 => .f14,
        vaxis.Key.f15 => .f15,
        vaxis.Key.f16 => .f16,
        vaxis.Key.f17 => .f17,
        vaxis.Key.f18 => .f18,
        vaxis.Key.f19 => .f19,
        vaxis.Key.f20 => .f20,
        vaxis.Key.f21 => .f21,
        vaxis.Key.f22 => .f22,
        vaxis.Key.f23 => .f23,
        vaxis.Key.f24 => .f24,
        vaxis.Key.f25 => .f25,

        vaxis.Key.kp_0 => .kp_0,
        vaxis.Key.kp_1 => .kp_1,
        vaxis.Key.kp_2 => .kp_2,
        vaxis.Key.kp_3 => .kp_3,
        vaxis.Key.kp_4 => .kp_4,
        vaxis.Key.kp_5 => .kp_5,
        vaxis.Key.kp_6 => .kp_6,
        vaxis.Key.kp_7 => .kp_7,
        vaxis.Key.kp_8 => .kp_8,
        vaxis.Key.kp_9 => .kp_9,
        vaxis.Key.kp_decimal => .kp_decimal,
        vaxis.Key.kp_divide => .kp_divide,
        vaxis.Key.kp_multiply => .kp_multiply,
        vaxis.Key.kp_subtract => .kp_subtract,
        vaxis.Key.kp_add => .kp_add,
        vaxis.Key.kp_enter => .kp_enter,
        vaxis.Key.kp_equal => .kp_equal,
        vaxis.Key.kp_separator => .kp_separator,
        vaxis.Key.kp_left => .kp_left,
        vaxis.Key.kp_right => .kp_right,
        vaxis.Key.kp_up => .kp_up,
        vaxis.Key.kp_down => .kp_down,
        vaxis.Key.kp_page_up => .kp_page_up,
        vaxis.Key.kp_page_down => .kp_page_down,
        vaxis.Key.kp_home => .kp_home,
        vaxis.Key.kp_end => .kp_end,
        vaxis.Key.kp_insert => .kp_insert,
        vaxis.Key.kp_delete => .kp_delete,
        vaxis.Key.kp_begin => .kp_begin,

        vaxis.Key.left_shift => .left_shift,
        vaxis.Key.left_control => .left_control,
        vaxis.Key.left_alt => .left_alt,
        vaxis.Key.left_super => .left_super,
        vaxis.Key.right_shift => .right_shift,
        vaxis.Key.right_control => .right_control,
        vaxis.Key.right_alt => .right_alt,
        vaxis.Key.right_super => .right_super,

        else => .invalid,
    };
}

fn render(
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

        const grid = self.size.grid();
        if (grid.columns != state.terminal.cols or grid.rows != state.terminal.rows) {
            // Skip a frame if our size doesn't match
            log.debug(
                "size mismatch, skipping render frame old: w={d}, h={d}, w_pix={d}, h_pix={d}, new: w={d}, h={d}, w_pix={d}, h_pix={d}",
                .{
                    grid.columns,
                    grid.rows,
                    self.size.screen.width,
                    self.size.screen.height,
                    state.terminal.cols,
                    state.terminal.rows,
                    state.terminal.width_px,
                    state.terminal.height_px,
                },
            );
            return;
        }

        self.screen_mutex.lock();
        defer self.screen_mutex.unlock();

        var screen = try state.terminal.screen.clone(self.gpa, .{ .viewport = .{} }, null);
        errdefer screen.deinit();

        // TODO: kitty images. See ghostty repo: src/renderer/OpenGL.zig:803

        const top_left = state.terminal.screen.pages.getTopLeft(.viewport);
        const cursor_pin_y = state.terminal.screen.cursor.page_pin.y;
        const cursor_row: u16 = cursor_pin_y -| top_left.y;

        // Set our cursor visible state while we have the lock
        if (state.terminal.modes.get(.cursor_visible) and cursor_row < grid.rows) {
            const blink = state.terminal.modes.get(.cursor_blinking);
            const shape: vaxis.Cell.CursorShape = switch (state.terminal.screen.cursor.cursor_style) {
                .bar => if (blink) .beam_blink else .beam,
                .block, .block_hollow => if (blink) .block_blink else .block,
                .underline => if (blink) .underline_blink else .underline,
            };
            self.cursor_state = .{
                .col = screen.cursor.x,
                .row = screen.cursor.y,
                .shape = shape,
            };
        } else {
            self.cursor_state = null;
        }

        // Reset the dirty flags in the terminal and screen. We assume
        // that our rebuild will be successful since so we optimize for
        // success and reset while we hold the lock. This is much easier
        // than coordinating row by row or as changes are persisted.
        state.terminal.flags.dirty = .{};
        state.terminal.screen.dirty = .{};
        {
            var it = state.terminal.screen.pages.pageIterator(
                .right_down,
                .{ .screen = .{} },
                null,
            );
            while (it.next()) |chunk| {
                var dirty_set = chunk.node.data.dirtyBitSet();
                dirty_set.unsetAll();
            }
        }

        break :critical screen;
    };
    defer screen.deinit();

    var row_iter = screen.pages.rowIterator(.right_down, .{ .viewport = .{} }, null);
    var row: u16 = 0;
    while (row_iter.next()) |pin| {
        defer row += 1;
        if (row >= self.screen.height) {
            // We can enter this branch when a resize is not complete yet
            break;
        }
        if (pin.isDirty()) {
            self.redraw.store(true, .unordered);
        }

        var col: u16 = 0;
        const cells = pin.cells(.all);
        for (cells) |*cell| {
            defer col += 1;
            if (col >= self.screen.width) {
                // We can enter this branch when a resize is not complete yet
                break;
            }
            if (cell.isEmpty()) {
                self.screen.writeCell(col, row, .{});
                continue;
            }
            const ghostty_style = pin.style(cell);
            const vx_style: vaxis.Style = ghosttyStyleToVaxisStyle(ghostty_style, cell);
            switch (cell.content_tag) {
                .codepoint => {
                    var buf: [4]u8 = undefined;
                    const n = std.unicode.utf8Encode(cell.codepoint(), &buf) catch unreachable;
                    const char = buf[0..n];
                    self.screen.writeCell(col, row, .{
                        // Codepoints we assume are always correct for width
                        .char = .{ .grapheme = char, .width = cell.gridWidth() },
                        .style = vx_style,
                    });
                },
                .codepoint_grapheme => {
                    var len: u32 = 0;
                    const cps = pin.grapheme(cell) orelse unreachable;

                    len += std.unicode.utf8CodepointSequenceLength(cell.codepoint()) catch unreachable;
                    for (cps) |cp| {
                        len += std.unicode.utf8CodepointSequenceLength(cp) catch unreachable;
                    }

                    var buf = try self.gpa.alloc(u8, len);
                    defer self.gpa.free(buf);

                    var n = std.unicode.utf8Encode(cell.codepoint(), buf) catch unreachable;
                    for (cps) |cp| {
                        n += std.unicode.utf8Encode(cp, buf[n..]) catch unreachable;
                    }
                    std.debug.assert(buf.len == n);
                    // Graphemes we measure per vaxis
                    const width: u16 = @intCast(vxfw.DrawContext.stringWidth(undefined, buf));
                    self.screen.writeCell(col, row, .{
                        .char = .{
                            .grapheme = buf,
                            .width = @intCast(width),
                        },
                        .style = vx_style,
                    });
                },
                .bg_color_palette, .bg_color_rgb => {
                    self.screen.writeCell(col, row, .{ .style = vx_style });
                },
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

fn ghosttyStyleToVaxisStyle(
    style: ghostty.terminal.Style,
    cell: *const ghostty.terminal.Cell,
) vaxis.Style {
    const fg: vaxis.Color = switch (style.fg_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };

    const bg: vaxis.Color = switch (cell.content_tag) {
        .bg_color_palette => .{ .index = cell.content.color_palette },
        .bg_color_rgb => .{ .rgb = .{
            cell.content.color_rgb.r,
            cell.content.color_rgb.g,
            cell.content.color_rgb.b,
        } },
        else => switch (style.bg_color) {
            .none => .default,
            .palette => |v| .{ .index = v },
            .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
        },
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

fn drainAppMailbox(self: *Terminal, ctx: *vxfw.EventContext) anyerror!void {
    while (self.mailbox.pop()) |message| {
        switch (message) {
            .open_config => {},
            .new_window => {},
            .close => {},
            .surface_message => |msg| try self.handleSurfaceMessage(msg.message, ctx),
            .redraw_surface => {},
            .redraw_inspector => {},
            .quit => {},
        }
    }
}

fn drainRendererMailbox(self: *Terminal) anyerror!void {
    var iter = self.renderer_mailbox.drain();
    defer iter.deinit();
    while (iter.next()) |message| {
        switch (message) {
            .resize => |size| try self.resize(size),
            else => log.debug("unhandled renderer message: {s}", .{@tagName(message)}),
        }
    }
}

fn handleSurfaceMessage(
    self: *Terminal,
    msg: ghostty.apprt.surface.Message,
    ctx: *vxfw.EventContext,
) anyerror!void {
    switch (msg) {
        // .change_config => {},
        //
        .set_title => |*v| {
            self.gpa.free(self.title);
            // We ignore the message in case the title was set via config.
            // if (self.config.title != null) {
            //     log.debug("ignoring title change request since static title is set via config", .{});
            //     return;
            // }

            // The ptrCast just gets sliceTo to return the proper type.
            // We know that our title should end in 0.
            const slice = std.mem.sliceTo(@as([*:0]const u8, @ptrCast(v)), 0);
            log.debug("changing title \"{s}\"", .{slice});
            self.title = try self.gpa.dupe(u8, slice);
            ctx.redraw = true;
        },

        .report_title => |style| {
            // if (!self.config.title_report) {
            //     log.info("report_title requested, but disabled via config", .{});
            //     break :report_title;
            // }

            const data = switch (style) {
                .csi_21_t => try std.fmt.allocPrint(
                    self.gpa,
                    "\x1b]l{s}\x1b\\",
                    .{self.title},
                ),
            };

            // We always use an allocating message because we don't know
            // the length of the title and this isn't a performance critical
            // path.
            self.io.queueMessage(.{
                .write_alloc = .{
                    .alloc = self.gpa,
                    .data = data,
                },
            }, .unlocked);
        },
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
        .set_mouse_shape => |shape| {
            log.debug("changing mouse shape: {}", .{shape});
            self.mouse_shape = switch (shape) {
                .default => .default,
                .help => .help,
                .pointer => .pointer,
                .progress => .progress,
                .wait => .wait,
                .cell => .cell,
                .text => .text,
                .ew_resize => .@"ew-resize",
                .ns_resize => .@"ns-resize",

                else => .default,
            };

            try ctx.setMouseShape(self.mouse_shape);
        },
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
        .close => try self.close(ctx),
        // Close without confirmation.
        .child_exited => {
            self.child_exited = true;
            try self.close(ctx);
        },
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
        .password_input => |v| {
            self.password_input = v;
            ctx.redraw = true;
        },
        else => log.debug("unhandled surface message: {s}", .{@tagName(msg)}),
    }
}

// Resize the internal screen and update the internal size
fn resize(self: *Terminal, size: renderer.Size) !void {
    // Reset the resize message
    defer self.resize_msg_sent.store(false, .unordered);

    // Lock the renderer
    self.renderer_mutex.lock();
    defer self.renderer_mutex.unlock();

    // We can safely set the size now
    self.size = size;

    // Lock the screen
    self.screen_mutex.lock();
    defer self.screen_mutex.unlock();
    self.screen.deinit(self.gpa);
    const grid = size.grid();
    self.screen = try vaxis.AllocatingScreen.init(self.gpa, grid.columns, grid.rows);
    self.redraw.store(true, .unordered);
    try self.wakeup.notify();
}

fn notifyResize(self: *Terminal, size: renderer.Size) !void {
    if (!self.resize_msg_sent.load(.unordered)) {
        self.resize_msg_sent.store(true, .unordered);
        self.renderer_mutex.lock();
        defer self.renderer_mutex.unlock();
        self.io.queueMessage(.{ .resize = size }, .unlocked);
    }
}

fn handleMouseEvent(self: *Terminal, ctx: *vxfw.EventContext, mouse: vaxis.Mouse) anyerror!void {
    switch (self.io.terminal.flags.mouse_event) {
        // If we have no reporting, we will try for a wheel scroll
        .none => return self.handleWheelScroll(ctx, mouse),
        .x10 => @panic("TODO: x10 encoding"),
        // We aren't reporting motion
        .normal => if (mouse.type == .motion) return,
        // We have to have a button for this to be reported
        .button => if (mouse.button == .none) return,
        .any => {},
    }

    const button: ?input.MouseButton = switch (mouse.button) {
        .none => null,
        .left => .left,
        .middle => .middle,
        .right => .right,
        .wheel_up => .four,
        .wheel_down => .five,
        .wheel_right => .six,
        .wheel_left => .seven,
        .button_8 => .eight,
        .button_9 => .nine,
        .button_10 => .ten,
        .button_11 => .eleven,
    };

    const mods: input.Mods = .{
        .shift = mouse.mods.shift,
        .ctrl = mouse.mods.ctrl,
        .alt = mouse.mods.alt,
    };

    // Get the code we'll actually write
    const button_code: u8 = code: {
        var acc: u8 = 0;

        // Determine our initial button value
        if (button == null) {
            // Null button means motion without a button pressed
            acc = 3;
        } else if (mouse.type == .release and
            self.io.terminal.flags.mouse_format != .sgr and
            self.io.terminal.flags.mouse_format != .sgr_pixels)
        {
            // Release is 3. It is NOT 3 in SGR mode because SGR can tell
            // the application what button was released.
            acc = 3;
        } else {
            acc = switch (button.?) {
                .left => 0,
                .middle => 1,
                .right => 2,
                .four => 64,
                .five => 65,
                .six => 66,
                .seven => 67,
                else => return, // unsupported
            };
        }

        // X10 doesn't have modifiers
        if (self.io.terminal.flags.mouse_event != .x10) {
            if (mods.shift) acc += 4;
            if (mods.alt) acc += 8;
            if (mods.ctrl) acc += 16;
        }

        // Motion adds another bit
        if (mouse.type == .motion or mouse.type == .drag) acc += 32;

        break :code acc;
    };

    // From here on out, we need a redraw and we consume the event
    ctx.consumeAndRedraw();
    switch (self.io.terminal.flags.mouse_format) {
        .x10 => {
            if (mouse.col > 222 or mouse.row > 222) {
                log.info("X10 mouse format can only encode X/Y up to 223", .{});
                return;
            }

            // + 1 below is because our x/y is 0-indexed and the protocol wants 1
            var data: termio.Message.WriteReq.Small.Array = undefined;
            std.debug.assert(data.len >= 6);
            data[0] = '\x1b';
            data[1] = '[';
            data[2] = 'M';
            data[3] = 32 + button_code;
            data[4] = 32 + @as(u8, @intCast(mouse.col)) + 1;
            data[5] = 32 + @as(u8, @intCast(mouse.row)) + 1;

            // Ask our IO thread to write the data
            self.io.queueMessage(.{ .write_small = .{
                .data = data,
                .len = 6,
            } }, .locked);
        },

        .utf8 => {
            // Maximum of 12 because at most we have 2 fully UTF-8 encoded chars
            var data: termio.Message.WriteReq.Small.Array = undefined;
            std.debug.assert(data.len >= 12);
            data[0] = '\x1b';
            data[1] = '[';
            data[2] = 'M';

            // The button code will always fit in a single u8
            data[3] = 32 + button_code;

            // UTF-8 encode the x/y
            var i: usize = 4;
            i += try std.unicode.utf8Encode(@intCast(32 + mouse.col + 1), data[i..]);
            i += try std.unicode.utf8Encode(@intCast(32 + mouse.row + 1), data[i..]);

            // Ask our IO thread to write the data
            self.io.queueMessage(.{ .write_small = .{
                .data = data,
                .len = @intCast(i),
            } }, .locked);
        },

        .sgr => {
            // Final character to send in the CSI
            const final: u8 = if (mouse.type == .release) 'm' else 'M';

            // Response always is at least 4 chars, so this leaves the
            // remainder for numbers which are very large...
            var data: termio.Message.WriteReq.Small.Array = undefined;
            const resp = try std.fmt.bufPrint(&data, "\x1B[<{d};{d};{d}{c}", .{
                button_code,
                mouse.col + 1,
                mouse.row + 1,
                final,
            });

            // Ask our IO thread to write the data
            self.io.queueMessage(.{ .write_small = .{
                .data = data,
                .len = @intCast(resp.len),
            } }, .locked);
        },

        .urxvt => {
            // Response always is at least 4 chars, so this leaves the
            // remainder for numbers which are very large...
            var data: termio.Message.WriteReq.Small.Array = undefined;
            const resp = try std.fmt.bufPrint(&data, "\x1B[{d};{d};{d}M", .{
                32 + button_code,
                mouse.col + 1,
                mouse.row + 1,
            });

            // Ask our IO thread to write the data
            self.io.queueMessage(.{ .write_small = .{
                .data = data,
                .len = @intCast(resp.len),
            } }, .locked);
        },

        .sgr_pixels => {
            // Final character to send in the CSI
            const final: u8 = if (mouse.type == .release) 'm' else 'M';

            const x_pix = (self.size.cell.width * (mouse.col + 1)) + mouse.xoffset;
            const y_pix = (self.size.cell.height * (mouse.row + 1)) + mouse.yoffset;

            // Response always is at least 4 chars, so this leaves the
            // remainder for numbers which are very large...
            var data: termio.Message.WriteReq.Small.Array = undefined;
            const resp = try std.fmt.bufPrint(&data, "\x1B[<{d};{d};{d}{c}", .{
                button_code,
                x_pix,
                y_pix,
                final,
            });

            // Ask our IO thread to write the data
            self.io.queueMessage(.{ .write_small = .{
                .data = data,
                .len = @intCast(resp.len),
            } }, .locked);
        },
    }
}

fn handleWheelScroll(self: *Terminal, ctx: *vxfw.EventContext, mouse: vaxis.Mouse) anyerror!void {
    switch (mouse.button) {
        .wheel_up => {},
        .wheel_down => {},
        else => return,
    }
    self.renderer_state.mutex.lock();
    defer self.renderer_state.mutex.unlock();
    ctx.consumeAndRedraw();

    // If we have an active mouse reporting mode, clear the selection.
    // The selection can occur if the user uses the shift mod key to
    // override mouse grabbing from the window.
    if (self.io.terminal.flags.mouse_event != .none) {
        try self.setSelection(null);
    }

    // If we're in alternate screen with alternate scroll enabled, then
    // we convert to cursor keys. This only happens if we're:
    // (1) alt screen (2) no explicit mouse reporting and (3) alt
    // scroll mode enabled.
    if (self.io.terminal.active_screen == .alternate and
        self.io.terminal.flags.mouse_event == .none and
        self.io.terminal.modes.get(.mouse_alternate_scroll))
    {
        // When we send mouse events as cursor keys we always
        // clear the selection.
        try self.setSelection(null);

        const seq = if (self.io.terminal.modes.get(.cursor_keys)) seq: {
            // cursor key: application mode
            break :seq switch (mouse.button) {
                .wheel_up => "\x1bOA",
                .wheel_down => "\x1bOB",
                else => unreachable,
            };
        } else seq: {
            // cursor key: normal mode
            break :seq switch (mouse.button) {
                .wheel_up => "\x1b[A",
                .wheel_down => "\x1b[B",
                else => unreachable,
            };
        };
        self.io.queueMessage(.{ .write_stable = seq }, .locked);

        return;
    }

    const delta: isize = switch (mouse.button) {
        .wheel_up => -1,
        .wheel_down => 1,
        else => unreachable,
    };

    // Modify our viewport, this requires a lock since it affects
    // rendering. We have to switch signs here because our delta
    // is negative down but our viewport is positive down.
    try self.io.terminal.scrollViewport(.{ .delta = delta });

    try self.wakeup.notify();
}

/// Find the default shell command
fn getShell(gpa: Allocator) ![]const u8 {
    const shell_env = try std.process.getEnvVarOwned(gpa, "SHELL");
    if (shell_env.len > 0) {
        log.info("default shell source=env value={s}", .{shell_env});
        return shell_env;
    }

    switch (builtin.os.tag) {
        .windows => return "cmd.exe",
        else => {
            // We need the passwd entry for the remainder
            const pw = try ghostty.os.passwd.get(gpa);

            const sh = pw.shell orelse {
                log.warn("no default shell found, will default to using sh", .{});
                return "sh";
            };
            return sh;
        },
    }
}

pub fn close(self: *Terminal, maybe_ctx: ?*vxfw.EventContext) anyerror!void {
    if (self.child_exited) return;

    {
        self.renderer_mutex.lock();
        defer self.renderer_mutex.unlock();
        if (!self.io.terminal.cursorIsAtPrompt()) {
            log.warn("TODO: confirm close", .{});
        }
    }
    self.deinit();
    if (maybe_ctx) |ctx| {
        ctx.redraw = true;
    }
}
