const std = @import("std");
const buildpkg = @import("ghostty").buildpkg;

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    {
        // TODO: do we need to install resources? Probably since we need a terminfo file
        var env = try std.process.getEnvMap(b.allocator);
        errdefer env.deinit();

        const config: buildpkg.Config = .{
            .optimize = optimize,
            .target = target,
            .wasm_target = .browser,
            .env = env,
        };
        const resources = try buildpkg.GhosttyResources.init(b, &config);
        resources.install();
    }

    const exe = b.addExecutable(.{
        .name = "haunt",
        .root_source_file = b.path("src/main2.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Expose as a module
    const terminal = b.addModule("Terminal", .{
        .root_source_file = b.path("src/Terminal.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("Terminal", terminal);

    const options = b.addModule("options", .{
        .root_source_file = b.path("src/options.zig"),
        .target = target,
        .optimize = optimize,
    });

    {
        // Ghostty mod
        const ghostty = b.dependency("ghostty", .{
            .target = target,
            .optimize = optimize,
            .@"app-runtime" = .none,
        });

        const ghostty_mod = ghostty.module("ghostty");
        options.addImport("ghostty", ghostty_mod);
        options.addImport("Terminal", terminal);
        terminal.addImport("ghostty", ghostty_mod);
        ghostty_mod.addImport("options", options);

        const libxev = ghostty.builder.dependency("libxev", .{
            .target = target,
            .optimize = optimize,
        });
        const libxev_mod = libxev.module("xev");
        terminal.addImport("xev", libxev_mod);
        exe_unit_tests.root_module.addImport("xev", libxev_mod);
        exe.root_module.addImport("xev", libxev_mod);
    }

    {
        // vaxis mod
        const vaxis = b.dependency("vaxis", .{
            .target = target,
            .optimize = optimize,
        });
        const vaxis_mod = vaxis.module("vaxis");
        terminal.addImport("vaxis", vaxis_mod);

        exe.root_module.addImport("vaxis", vaxis_mod);
    }
}
