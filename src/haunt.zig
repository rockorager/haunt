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
                        self.size = ws;
                        // TODO: update surfaces
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
                try surface.setSize(self.size);
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

    pixel_size: apprt.SurfaceSize,
    cell_size: struct {
        width: u16,
        height: u16,
    },

    pub fn init(self: *Surface, app: *App) !void {
        self.app = app;
        self.pixel_size = .{
            // TODO: when do set this?
            .width = 100,
            .height = 100,
        };
        self.cell_size.width = 10;
        self.cell_size.height = 10;

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
        return self.pixel_size;
    }

    fn setSize(self: *Surface, size: vaxis.Winsize) !void {
        self.pixel_size.height = size.y_pixel;
        self.pixel_size.width = size.x_pixel;
        try self.core_surface.sizeCallback(self.pixel_size);
    }
};
