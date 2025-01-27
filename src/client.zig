const std = @import("std");
const builtin = @import("builtin");

const posix = std.posix;

const Allocator = std.mem.Allocator;

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
