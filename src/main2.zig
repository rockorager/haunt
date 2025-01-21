const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal.zig");

const renderer = @import("renderer.zig");

const vxfw = vaxis.vxfw;

pub const ghostty_options = struct {
    pub const Renderer = renderer.Vaxis;
    pub const runtime = @import("Terminal.zig");
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var app = try vxfw.App.init(gpa.allocator());
    defer app.deinit();

    var vt: Terminal = undefined;
    try Terminal.init(&vt, gpa.allocator(), .{
        .command = "fish",
    });
    defer vt.deinit();

    try app.run(vt.widget(), .{});
    // std.time.sleep(1000000000000);
}
