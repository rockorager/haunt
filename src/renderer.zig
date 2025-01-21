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
    focused: bool,

    frame: vaxis.AllocatingScreen,

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
            .frame = try vaxis.AllocatingScreen.init(gpa, 10, 10),
            .focused = false,
        };
    }

    pub fn deinit(self: *Vaxis) void {
        self.frame.deinit(self.gpa);
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
        self.focused = focused;
        log.debug("{s}", .{@src().fn_name});
    }

    pub fn hasVsync(self: *Vaxis) bool {
        _ = self; // autofix
        return false;
    }

    pub fn drawFrame(self: *Vaxis, surface: *haunt.Surface) !void {
        const vx = &surface.app.vx;
        const win = vx.window();

        var row: u16 = 0;
        while (row < self.frame.height) : (row += 1) {
            var col: u16 = 0;
            while (col < self.frame.width) : (col += 1) {
                const cell = self.frame.readCell(col, row) orelse unreachable;
                win.writeCell(col + surface.origin.col, row + surface.origin.row, cell);
            }
        }

        const focused = if (surface.app.core.focusedSurface()) |core_surface|
            core_surface == &surface.core_surface
        else
            false;
        if (focused and self.frame.cursor_vis) {
            win.showCursor(surface.origin.col + self.frame.cursor_col, surface.origin.row + self.frame.cursor_row);
        } else {
            win.hideCursor();
        }

        var bw = std.io.bufferedWriter(surface.app.tty.anyWriter());
        try vx.render(bw.writer().any());
        try bw.flush();
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
        log.debug("{s}", .{@src().fn_name});
        const grid_size = size.grid();
        if (self.frame.height == grid_size.rows and self.frame.width == grid_size.columns)
            return;

        self.frame.deinit(self.gpa);
        self.frame = try vaxis.AllocatingScreen.init(self.gpa, grid_size.columns, grid_size.rows);
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
        _ = cursor_blink_visible; // autofix

        const vx = &surface.app.vx;
        const win = vx.window();

        const screen = state.terminal.screen;
        var row_iter = screen.pages.rowIterator(.right_down, .{ .viewport = .{} }, null);
        var row: u16 = 0;
        while (row_iter.next()) |pin| {
            defer row += 1;
            var col: u16 = 0;
            const cells = pin.cells(.all);
            for (cells) |cell| {
                const vx_style: vaxis.Style = if (cell.hasStyling())
                    ghosttyStyleToVaxisStyle(pin.style(&cell))
                else
                    .{};
                if (cell.hasText()) {
                    var buf: [256]u8 = undefined;
                    var n = try std.unicode.utf8Encode(cell.codepoint(), &buf);
                    if (cell.hasGrapheme()) {
                        if (pin.grapheme(&cell)) |cps| {
                            for (cps) |cp| {
                                n += try std.unicode.utf8Encode(cp, buf[n..]);
                            }
                        }
                    }
                    const char = buf[0..n];
                    const width = win.gwidth(char);
                    defer col += width;
                    self.frame.writeCell(col, row, .{
                        .char = .{
                            .grapheme = char,
                            .width = @intCast(width),
                        },
                        .style = vx_style,
                    });
                } else {
                    self.frame.writeCell(col, row, .{ .style = vx_style });
                    col += 1;
                }
            }
        }

        self.frame.cursor_col = state.terminal.screen.cursor.x;
        self.frame.cursor_row = state.terminal.screen.cursor.y;
        self.frame.cursor_vis = state.terminal.modes.get(.cursor_visible);
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
