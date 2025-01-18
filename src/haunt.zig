const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const CoreApp = ghostty.App;

pub const App = struct {
    core: *CoreApp,
    vx: vaxis.Vaxis,
    tty: vaxis.Tty,

    pub const Options = struct {};

    pub fn init(core_app: *CoreApp, _: Options) !App {
        return .{
            .core = core_app,
            .vx = try vaxis.init(core_app.alloc, .{}),
            .tty = try vaxis.Tty.init(),
        };
    }

    pub fn run(self: *App) !void {
        _ = self;
    }

    pub fn terminate(self: *App) void {
        self.vx.deinit(self.core.alloc, self.tty.anyWriter());
    }
};

pub const Surface = struct {
    pub fn deinit(self: *Surface) void {
        _ = self;
    }
};
