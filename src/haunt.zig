const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const CoreApp = ghostty.App;
const CoreSurface = ghostty.Surface;
const apprt = ghostty.apprt;

const log = std.log.scoped(.haunt);

const Event = union(enum) {
    wakeup,
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
};

pub const App = struct {
    core: *CoreApp,
    vx: vaxis.Vaxis,
    tty: vaxis.Tty,
    config: ghostty.config.Config,
    loop: ?vaxis.Loop(Event),

    surfaces: std.ArrayList(*Surface),

    opts: Options,

    size: vaxis.Winsize,

    pub const Options = struct {};

    pub fn init(core_app: *CoreApp, opts: Options) !App {
        return .{
            .core = core_app,
            .vx = try vaxis.init(core_app.alloc, .{}),
            .tty = try vaxis.Tty.init(),
            .opts = opts,
            .config = try ghostty.config.Config.load(core_app.alloc),
            .loop = null,
            .surfaces = std.ArrayList(*Surface).init(core_app.alloc),
            .size = .{
                .rows = 0,
                .cols = 0,
                .x_pixel = 0,
                .y_pixel = 0,
            },
        };
    }

    pub fn run(self: *App) !void {
        const vx = &self.vx;
        const tty = &self.tty;
        const gpa = self.core.alloc;

        self.loop = .{
            .tty = tty,
            .vaxis = vx,
        };
        const loop = &self.loop.?;
        try loop.init();

        try loop.start();
        defer loop.stop();

        try vx.enterAltScreen(tty.anyWriter());
        try vx.queryTerminal(tty.anyWriter(), 1 * std.time.ns_per_s);

        // Queue a new window to create a launch
        _ = self.core.mailbox.push(.{
            .new_window = .{},
        }, .{ .forever = {} });
        _ = self.core.mailbox.push(.{
            .new_window = .{},
        }, .{ .forever = {} });

        var focused: u2 = 0;

        while (true) {
            loop.pollEvent();
            while (loop.tryEvent()) |event| {
                std.log.debug("event {}", .{event});
                switch (event) {
                    .key_press => |key| {
                        if (key.matches('c', .{ .ctrl = true })) {
                            return;
                        }
                        if (key.matches('n', .{ .ctrl = true })) {
                            try self.surfaces.items[focused].core_surface.focusCallback(false);
                            if (focused == 0)
                                focused = 1
                            else
                                focused = 0;
                            try self.surfaces.items[focused].core_surface.focusCallback(true);
                        }
                        const key_event = vaxisKeyToGhosttyKey(key, true);
                        // TODO: focused surface
                        const surface = self.surfaces.items[focused];
                        const effect = try surface.core_surface.keyCallback(key_event);
                        if (effect == .closed) self.surfaces.getLast().close(false);
                    },
                    .winsize => |ws| {
                        try vx.resize(gpa, tty.anyWriter(), ws);
                        self.size = ws;
                        // TODO: update surfaces
                        var col: u16 = 0;
                        for (self.surfaces.items) |surface| {
                            const size: vaxis.Winsize = .{
                                .cols = self.size.cols / 2,
                                .rows = self.size.rows,
                                .x_pixel = self.size.x_pixel / 2,
                                .y_pixel = self.size.y_pixel,
                            };
                            log.debug("size={d} x {d}", .{ size.cols, size.rows });
                            surface.origin.col = col;
                            try surface.setSize(size);
                            col = size.cols;
                        }
                    },
                    .wakeup => {},
                }
            }

            try self.core.tick(self);
        }
    }

    pub fn terminate(self: *App) void {
        for (self.surfaces.items) |surface| {
            surface.close(false);
        }
        self.surfaces.deinit();
        self.vx.deinit(self.core.alloc, self.tty.anyWriter());
        self.tty.deinit();
        self.config.deinit();
    }

    pub fn performAction(
        self: *App,
        target: apprt.Target,
        comptime action: apprt.Action.Key,
        value: apprt.Action.Value(action),
    ) !void {
        log.warn("action={s}", .{@tagName(action)});
        switch (action) {
            .new_window => {
                const parent: ?*CoreSurface = switch (target) {
                    .app => null,
                    .surface => |v| v,
                };
                // TODO: should we ignore the return?
                const surface = try self.newSurface(parent);
                const size: vaxis.Winsize = .{
                    .cols = self.size.cols / 2,
                    .rows = self.size.rows,
                    .x_pixel = self.size.x_pixel / 2,
                    .y_pixel = self.size.y_pixel,
                };
                log.debug("size={d} x {d}", .{ size.cols, size.rows });
                try surface.setSize(size);
                if (self.surfaces.items.len > 0) {
                    surface.origin.col = size.cols;
                }
                try self.surfaces.append(surface);
            },
            else => {},
        }
        _ = value; // autofix
    }

    pub fn redrawInspector(_: App, _: *Surface) void {
        log.warn("inspector not implemented in haunt", .{});
    }

    pub fn redrawSurface(self: *App, surface: *Surface) void {
        log.debug("{s}", .{@src().fn_name});
        _ = surface; // autofix
        _ = self; // autofix
        // TODO:
    }

    fn newSurface(self: *App, _: ?*CoreSurface) !*Surface {
        var surface = try self.core.alloc.create(Surface);
        errdefer self.core.alloc.destroy(surface);

        try surface.init(self);
        return surface;
    }

    pub fn wakeup(self: *App) void {
        log.debug("{s}", .{@src().fn_name});
        if (self.loop) |*loop| {
            loop.postEvent(.wakeup);
        }
    }
};

pub const Surface = struct {
    core_surface: ghostty.Surface,
    should_close: bool,
    title: [:0]const u8 = "",
    app: *App,

    size: vaxis.Winsize,

    mouse_pos: apprt.CursorPos,

    origin: struct {
        col: u16,
        row: u16,
    },

    pub fn init(self: *Surface, app: *App) !void {
        self.app = app;
        self.size = .{
            .x_pixel = 100,
            .y_pixel = 100,
            .cols = 10,
            .rows = 10,
        };
        self.mouse_pos = .{
            .x = 0,
            .y = 0,
        };
        self.origin.col = 0;
        self.origin.row = 0;

        var config = try apprt.surface.newConfig(app.core, &app.config);
        defer config.deinit();

        try app.core.addSurface(self);
        errdefer app.core.deleteSurface(self);

        try self.core_surface.init(app.core.alloc, &config, app.core, app, self);
    }

    pub fn deinit(self: *Surface) void {
        self.app.core.deleteSurface(self);
        self.core_surface.deinit();
    }

    pub fn shouldClose(self: Surface) bool {
        return self.should_close;
    }

    pub fn close(self: *Surface, process_active: bool) void {
        // TODO:
        _ = process_active;
        self.should_close = true;
        self.deinit();
        self.app.core.alloc.destroy(self);
    }

    pub fn getContentScale(_: Surface) !apprt.ContentScale {
        return .{ .x = 1, .y = 1 };
    }

    pub fn getTitle(self: Surface) [:0]const u8 {
        return self.title;
    }

    pub fn clipboardRequest(self: *Surface, _: apprt.Clipboard, state: apprt.ClipboardRequest) !void {
        const tty = &self.app.tty;
        const vx = &self.app.vx;
        // TODO: read and osc_52_write
        switch (state) {
            .paste => @panic("TODO"),
            .osc_52_read => try vx.requestSystemClipboard(tty.anyWriter()),
            .osc_52_write => @panic("TODO"),
        }
    }

    pub fn setClipboardString(
        self: *Surface,
        val: [:0]const u8,
        clipboard_type: apprt.Clipboard,
        confirm: bool,
    ) !void {
        _ = clipboard_type;
        _ = confirm;
        const tty = &self.app.tty;
        const vx = &self.app.vx;
        try vx.copyToSystemClipboard(tty.anyWriter(), std.mem.span(val.ptr), self.app.core.alloc);
    }

    /// Returns the pixel size of the screen
    pub fn getSize(self: Surface) !apprt.SurfaceSize {
        return .{
            .width = self.size.x_pixel,
            .height = self.size.y_pixel,
        };
    }

    fn setSize(self: *Surface, size: vaxis.Winsize) !void {
        if (size.cols == self.size.cols and
            size.rows == self.size.rows and
            size.x_pixel == self.size.x_pixel and
            size.y_pixel == self.size.y_pixel)
            return;

        self.size = size;

        self.core_surface.size = .{
            .cell = .{
                .height = size.y_pixel / size.rows,
                .width = size.x_pixel / size.cols,
            },
            .padding = .{},
            .screen = .{
                .height = size.y_pixel,
                .width = size.x_pixel,
            },
        };

        self.core_surface.io.queueMessage(.{ .resize = self.core_surface.size }, .unlocked);
    }

    pub fn getCursorPos(self: Surface) !apprt.CursorPos {
        return self.mouse_pos;
    }

    pub fn supportsClipboard(self: Surface, clipboard: apprt.Clipboard) bool {
        _ = clipboard; // autofix
        _ = self; // autofix
        return false;
    }
};

fn vaxisKeyToGhosttyKey(key: vaxis.Key, press: bool) ghostty.input.KeyEvent {
    const action: ghostty.input.Action = if (press) .press else .release;
    const physical_key = codepointToGhosttyKey(key.codepoint);
    const mods: ghostty.input.Mods = .{
        .alt = key.mods.alt,
        .caps_lock = key.mods.caps_lock,
        .ctrl = key.mods.ctrl,
        .num_lock = key.mods.num_lock,
        .shift = key.mods.shift,
        .super = key.mods.super,
    };
    return .{
        .action = action,
        .key = physical_key,
        .physical_key = physical_key,
        .mods = mods,
        .consumed_mods = .{},
        .composing = false,
        .utf8 = if (key.text) |text| text else "",
        // TODO: .unshifted_codepoint,
    };
}

fn codepointToGhosttyKey(cp: u21) ghostty.input.Key {
    return switch (cp) {
        ' ' => .space,

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
        else => .invalid,
    };
}
