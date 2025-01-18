const std = @import("std");
const ghostty = @import("ghostty");

pub const Vaxis = struct {
    pub const DerivedConfig = struct {
        pub fn init(gpa: std.mem.Allocator, config: *const ghostty.config.Config) !DerivedConfig {
            _ = gpa;
            _ = config;
            return .{};
        }

        pub fn deinit(self: *DerivedConfig) void {
            _ = self;
        }
    };
};
