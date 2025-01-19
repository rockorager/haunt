const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const haunt = @import("haunt.zig");

const apprt = ghostty.apprt;
const renderer = ghostty.renderer;
const terminal = ghostty.terminal;

const log = std.log.scoped(.vaxis_renderer);

pub const Vaxis = struct {
    gpa: std.mem.Allocator,

    surface_mailbox: apprt.surface.Mailbox,

    foreground_color: ?terminal.color.RGB,
    background_color: ?terminal.color.RGB,
    cursor_color: ?terminal.color.RGB,

    dirty: bool,

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

    pub fn init(gpa: std.mem.Allocator, options: renderer.Options) !Vaxis {
        return .{
            .gpa = gpa,
            .surface_mailbox = options.surface_mailbox,
            .foreground_color = null,
            .background_color = null,
            .cursor_color = null,
            .dirty = false,
        };
    }

    pub fn deinit(self: *Vaxis) void {
        _ = self;
    }

    pub fn surfaceInit(_: *apprt.Surface) !void {
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn finalizeSurfaceInit(self: *Vaxis, surface: *haunt.Surface) !void {
        _ = self;
        _ = surface;
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn threadEnter(self: *Vaxis, surface: *haunt.Surface) !void {
        log.debug("{s}", .{@src().fn_name});
        _ = self;
        _ = surface;
    }

    pub fn threadExit(self: *Vaxis, surface: *haunt.Surface) void {
        log.debug("{s}", .{@src().fn_name});
        _ = self;
        _ = surface;
    }

    pub fn setVisible(self: *Vaxis, visible: bool) void {
        log.debug("{s}", .{@src().fn_name});
        _ = visible; // autofix
        _ = self; // autofix
    }

    pub fn setFocus(self: *Vaxis, focused: bool) !void {
        log.debug("{s}", .{@src().fn_name});
        _ = focused; // autofix
        _ = self; // autofix
    }

    pub fn hasVsync(self: *Vaxis) bool {
        _ = self; // autofix
        log.debug("{s}", .{@src().fn_name});
        return false;
    }

    pub fn drawFrame(self: *Vaxis, surface: *haunt.Surface) !void {
        log.debug("{s}", .{@src().fn_name});
        _ = self;
        _ = surface;
    }

    pub fn setFontGrid(self: *Vaxis, grid: anytype) void {
        _ = grid; // autofix
        _ = self; // autofix
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn markDirty(self: *Vaxis) void {
        self.dirty = true;
    }

    pub fn setScreenSize(self: *Vaxis, size: renderer.Size) !void {
        _ = size; // autofix
        _ = self; // autofix
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn changeConfig(self: *Vaxis, config: *DerivedConfig) !void {
        _ = config; // autofix
        _ = self; // autofix
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn updateFrame(
        self: *Vaxis,
        surface: *apprt.Surface,
        state: *renderer.State,
        cursor_blink_visible: bool,
    ) !void {
        const vx = &surface.app.vx;
        const win = vx.window();

        var arena = std.heap.ArenaAllocator.init(self.gpa);
        defer arena.deinit();
        const allocator = arena.allocator();

        const screen = state.terminal.screen;
        var row_iter = screen.pages.rowIterator(.right_down, .{ .viewport = .{} }, null);
        var row: u16 = 0;
        while (row_iter.next()) |pin| {
            defer row += 1;
            var col: u16 = 0;
            const cells = pin.cells(.all);
            for (cells) |cell| {
                const vx_style = ghosttyStyleToVaxisStyle(pin.style(&cell));
                if (cell.hasText()) {
                    var buf: [16]u8 = undefined;
                    const n = try std.unicode.utf8Encode(cell.codepoint(), &buf);
                    const char = try allocator.dupe(u8, buf[0..n]);
                    const width = win.gwidth(char);
                    defer col += width;
                    win.writeCell(col, row, .{
                        .char = .{
                            .grapheme = char,
                            .width = @intCast(width),
                        },
                        .style = vx_style,
                    });

                    log.debug("{s}", .{buf[0..n]});
                }
            }
        }

        win.showCursor(state.terminal.screen.cursor.x, state.terminal.screen.cursor.y);

        try vx.render(surface.app.tty.anyWriter());
        _ = cursor_blink_visible; // autofix
        log.debug("{s}", .{@src().fn_name});
    }
};

fn ghosttyStyleToVaxisStyle(style: ghostty.terminal.Style) vaxis.Style {
    const fg: vaxis.Color = switch (style.fg_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    const bg: vaxis.Color = switch (style.bg_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    const ul: vaxis.Color = switch (style.underline_color) {
        .none => .default,
        .palette => |v| .{ .index = v },
        .rgb => |v| .{ .rgb = .{ v.r, v.g, v.b } },
    };
    return .{
        .fg = fg,
        .bg = bg,
        .ul = ul,
        .ul_style = @enumFromInt(@intFromEnum(style.flags.underline)),
        .bold = style.flags.bold,
        .dim = style.flags.faint,
        .italic = style.flags.italic,
        .blink = style.flags.blink,
        .reverse = style.flags.inverse,
        .invisible = style.flags.invisible,
        .strikethrough = style.flags.strikethrough,
    };
}
