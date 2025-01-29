const std = @import("std");
const vaxis = @import("vaxis");
const xev = @import("xev");

const client = @import("client.zig");
const protocol = @import("protocol.zig");
const server = @import("server.zig");

const Terminal = @import("Terminal");

const vxfw = vaxis.vxfw;

pub fn main() !void {
    const sockpath = "/tmp/haunt.sock";
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var loop = try xev.Loop.init(.{});
    defer loop.deinit();

    // Try to connect as a client. If we error, we might start a server
    var cl = client.Client.init(gpa.allocator(), sockpath) catch |err| {
        switch (err) {
            error.FileNotFound => {}, // No socket found. Start the server
            error.ConnectionRefused => {
                // File exists but is not accepting connetions.
                std.log.debug(
                    "socket already exists but is refusing connection. Removing file at {s}",
                    .{sockpath},
                );
                try std.posix.unlink(sockpath);
            },
            else => {
                std.log.err("err={}", .{err});
                return err;
            },
        }
        // start the server by forking
        // const pid = try std.posix.fork();
        // if (pid == 0) {
        // reset stdin, stdout, stderr
        // const dev_null = try std.posix.open("/dev/null", .{ .ACCMODE = .WRONLY }, 0);
        // try std.posix.dup2(dev_null, std.posix.STDIN_FILENO);
        // try std.posix.dup2(dev_null, std.posix.STDOUT_FILENO);
        // // try std.posix.dup2(dev_null, std.posix.STDERR_FILENO);
        // // we are the child.
        // _ = std.os.linux.setsid();
        var srv: server.Server = undefined;
        try srv.init(gpa.allocator(), sockpath, &loop);

        defer srv.deinit();

        try loop.run(.until_done);
        return;
        // }
        //
        // return;

        // // We are the original process. Let's sleep and try to connect again
        // const retries: u8 = 50;
        // var i: u8 = 0;
        // while (i <= retries) : (i += 1) {
        //     // Sleep m
        //     std.time.sleep(@as(u64, i) * std.time.ns_per_ms);
        //     if (client.Client.init(gpa.allocator(), sockpath)) |cl|
        //         break :blk cl
        //     else |err2| {
        //         if (i == retries) {
        //             std.log.err("error trying connection after fork: {}", .{err2});
        //             return err2;
        //         }
        //     }
    };
    // return error.FailedConnection;

    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();
    // Skip the binary
    _ = args.next();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, "list-sessions", arg)) {
            return cl.listSessions();
        }
        if (std.mem.eql(u8, "attach", arg)) {
            // grab the next and attach to that session
            const session = args.next() orelse return error.MissingArg;
            try cl.attach(session);
            while (true) {
                var buf: [4096]u8 = undefined;
                const n = try cl.stream.read(&buf);
                if (n == 0) {
                    break;
                }
                std.log.debug("{s}", .{buf[0..n]});
                //
            }
            return;
        }
    }
    try cl.attach(null);
    while (true) {
        var buf: [4096]u8 = undefined;
        const n = try cl.stream.read(&buf);
        if (n == 0) {
            break;
        }
        const raw_msg = buf[0..n];
        std.log.debug("wire msg={s}", .{raw_msg});
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();
        // TODO:
        const request = try protocol.Request.decode(arena.allocator(), cl.stream.handle, raw_msg);
        switch (request.method) {
            .exit => |v| {
                if (v == 0) return;
                std.log.err("error={d}", .{v});
                return error.Bad;
            },
            else => {},
        }
        std.log.debug("{s}", .{buf[0..n]});
        //
    }
}

test {
    _ = @import("server.zig");
}
