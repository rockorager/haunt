const std = @import("std");
const builtin = @import("builtin");
const vaxis = @import("vaxis");

const json = std.json;
const net = std.net;
const posix = std.posix;

const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;

pub const Client = struct {
    gpa: Allocator,
    stream: net.Stream,

    pub fn init(gpa: Allocator, sockpath: []const u8) !Client {
        return .{
            .gpa = gpa,
            .stream = try net.connectUnixSocket(sockpath),
        };
    }

    pub fn deinit(self: *Client) void {
        const stdout = std.io.getStdOut();
        var buf_writer = std.io.bufferedWriter(stdout.writer());
        var writer = buf_writer.writer();

        const ctlseqs = vaxis.ctlseqs;

        // Pop kitty keyboard
        writer.writeAll(ctlseqs.csi_u_pop) catch {};
        // Exit alt screen
        writer.writeAll(ctlseqs.rmcup) catch {};
        // Show the cursor
        writer.writeAll(ctlseqs.show_cursor) catch {};
        // Reset bracketed paste
        writer.writeAll(ctlseqs.bp_reset) catch {};
        // Disable color scheme updates
        writer.writeAll(ctlseqs.color_scheme_reset) catch {};

        buf_writer.flush() catch {};

        // TODO: Reset ioctls

        self.stream.close();
    }

    pub fn attach(self: *Client, session: ?[]const u8) !void {
        const tty = try ttyname(self.gpa);
        defer self.gpa.free(tty);
        std.log.debug("ttyname={s}", .{tty});

        const msg: protocol.Request = .{
            .id = .{ .integer = 1 },
            .method = .{
                .attach = .{
                    .ttyname = tty,
                    .session = session,
                },
            },
        };
        try msg.stringify(self.stream.writer().any());
    }

    pub fn listSessions(self: *Client) !void {
        const request: protocol.Request = .{
            .id = .{ .integer = 1 },
            .method = .@"list-sessions",
        };
        try request.stringify(self.stream.writer().any());
        var buf: [4096]u8 = undefined;
        const stdout = std.io.getStdOut().writer();
        var arena = std.heap.ArenaAllocator.init(self.gpa);
        defer arena.deinit();
        while (true) {
            const msg = try self.stream.reader().readUntilDelimiter(&buf, '\n');
            const response = try protocol.Response.decode(arena.allocator(), msg);
            if (response.id == .integer and response.id.integer == 1) {
                const array = response.result.array;
                for (array.items) |item| {
                    try stdout.writeAll(item.string);
                    try stdout.writeAll("\n");
                }
                return;
            }
        }
    }
};

/// Returns the path of this tty device (eg /dev/pts/2, /dev/ttyS0)
fn ttyname(gpa: Allocator) ![]const u8 {
    // Check if we are a terminal
    const fd: posix.fd_t = if (posix.isatty(posix.STDIN_FILENO))
        posix.STDIN_FILENO
    else if (posix.isatty(posix.STDOUT_FILENO))
        posix.STDOUT_FILENO
    else if (posix.isatty(posix.STDERR_FILENO))
        posix.STDERR_FILENO
    else
        return error.NoTty;

    // fstat the file descriptor. We need to find the device and raw device ids
    const fstat = try posix.fstat(fd);

    switch (builtin.os.tag) {
        .windows => return error.NotSupported,
        .linux,
        .freebsd,
        .aix,
        .solaris,
        .illumos,
        .serenity,
        => return ttynameLinux(gpa, fstat),
        .macos,
        .openbsd,
        .dragonfly,
        => return ttynameBsd(gpa, fstat),
        else => {
            if (builtin.os.tag.isBSD())
                return ttynameBsd(gpa, fstat);
            if (builtin.os.tag.isDarwin())
                return ttynameBsd(gpa, fstat);
            return error.NotSupported;
        },
    }
}

/// Loops through /dev/tty* looking for a matching device
fn ttynameBsd(gpa: Allocator, fstat: posix.Stat) ![]const u8 {
    var pts = try std.fs.openDirAbsolute("/dev", .{ .iterate = true });
    defer pts.close();
    var iter = pts.iterate();
    while (try iter.next()) |entry| {
        // We only care about files starting with tty
        if (!std.mem.startsWith(u8, entry.name, "tty")) continue;

        const stat = try posix.fstatat(pts.fd, entry.name, 0);
        if (stat.dev == fstat.dev and stat.rdev == fstat.rdev) {
            return std.fmt.allocPrint(gpa, "/dev/{s}", .{entry.name});
        }
    } else return error.DeviceNotFound;
}

/// Loops through /dev/pts/* looking for a matching device
fn ttynameLinux(gpa: Allocator, fstat: posix.Stat) ![]const u8 {
    var pts = try std.fs.openDirAbsolute("/dev/pts", .{ .iterate = true });
    defer pts.close();
    var iter = pts.iterate();
    while (try iter.next()) |entry| {
        const stat = try posix.fstatat(pts.fd, entry.name, 0);
        if (stat.dev == fstat.dev and stat.rdev == fstat.rdev) {
            return std.fmt.allocPrint(gpa, "/dev/pts/{s}", .{entry.name});
        }
    } else return error.DeviceNotFound;
}
