const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const CoreApp = ghostty.App;
const apprt = ghostty.apprt;

const log = std.log.scoped(.haunt);

const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
};

pub const App = struct {
    core: *CoreApp,
    vx: vaxis.Vaxis,
    tty: vaxis.Tty,

    opts: Options,

    pub const Options = struct {};

    pub fn init(core_app: *CoreApp, opts: Options) !App {
        return .{
            .core = core_app,
            .vx = try vaxis.init(core_app.alloc, .{}),
            .tty = try vaxis.Tty.init(),
            .opts = opts,
        };
    }

    pub fn run(self: *App) !void {
        const vx = &self.vx;
        const tty = &self.tty;
        const gpa = self.core.alloc;

        var loop: vaxis.Loop(Event) = .{
            .tty = tty,
            .vaxis = vx,
        };
        try loop.init();

        try loop.start();
        defer loop.stop();

        try vx.enterAltScreen(tty.anyWriter());
        try vx.queryTerminal(tty.anyWriter(), 1 * std.time.ns_per_s);

        while (true) {
            loop.pollEvent();
            while (loop.tryEvent()) |event| {
                std.log.debug("event {}", .{event});
                switch (event) {
                    .key_press => |key| {
                        if (key.matches('c', .{ .ctrl = true })) {
                            return;
                        }
                    },
                    .winsize => |ws| {
                        try vx.resize(gpa, tty.anyWriter(), ws);
                    },
                }
            }

            try self.core.tick(self);
        }
    }

    pub fn terminate(self: *App) void {
        self.vx.deinit(self.core.alloc, self.tty.anyWriter());
        self.tty.deinit();
    }

    pub fn performAction(
        self: *App,
        target: apprt.Target,
        comptime action: apprt.Action.Key,
        value: apprt.Action.Value(action),
    ) !void {
        _ = value; // autofix
        _ = target; // autofix
        _ = self; // autofix
    }

    pub fn redrawInspector(_: App, _: *Surface) void {
        log.warn("inspector not implemented in haunt", .{});
    }

    pub fn redrawSurface(self: *App, surface: *Surface) void {
        _ = surface; // autofix
        _ = self; // autofix
        // TODO:
    }
};

pub const Surface = struct {
    core_surface: ghostty.Surface,
    should_close: bool,
    title: [:0]const u8 = "",
    app: *App,

    pub fn deinit(self: *Surface) void {
        _ = self;
    }

    pub fn shouldClose(self: Surface) bool {
        return self.should_close;
    }

    pub fn close(self: *Surface, process_active: bool) void {
        // TODO:
        _ = self;
        _ = process_active;
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
};
