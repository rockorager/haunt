const std = @import("std");
const vaxis = @import("vaxis");
const xev = @import("xev");

const server = @import("server.zig");

const Terminal = @import("Terminal");

const vxfw = vaxis.vxfw;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var loop = try xev.Loop.init(.{});
    defer loop.deinit();

    var srv: server.Server = undefined;
    try srv.init(gpa.allocator(), "/tmp/vxfw.sock", &loop);

    defer srv.deinit();

    try loop.run(.until_done);
}

test {
    _ = @import("server.zig");
}
