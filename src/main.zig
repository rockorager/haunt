const std = @import("std");
const ghostty = @import("ghostty");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal.zig");

const vxfw = vaxis.vxfw;

pub const ghostty_options = struct {
    pub const Renderer = Terminal.Renderer;
    pub const runtime = @import("Terminal.zig");
};

// Animation transition time for moving between panes
const transition_time_ms = 100;

const Model = struct {
    vts: [4]Terminal,
    focused: u2 = 0,
    offset: i17 = 0,
    win_width: u16 = 0,

    pub fn widget(self: *Model) vxfw.Widget {
        return .{
            .userdata = self,
            .captureHandler = Model.typeErasedCaptureHandler,
            .eventHandler = Model.typeErasedEventHandler,
            .drawFn = Model.typeErasedDrawFn,
        };
    }

    fn typeErasedCaptureHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        switch (event) {
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    ctx.quit = true;
                    return ctx.consumeEvent();
                }
                if (key.matches(vaxis.Key.right, .{ .ctrl = true })) {
                    self.focused +%= 1;
                    ctx.consumeEvent();
                    self.offset = self.rightOffset();
                    try ctx.tick(8, self.widget());
                    return ctx.requestFocus(self.focusedVt().widget());
                }
                if (key.matches(vaxis.Key.left, .{ .ctrl = true })) {
                    self.focused -%= 1;
                    ctx.consumeEvent();
                    self.offset = self.leftOffset();
                    try ctx.tick(8, self.widget());
                    return ctx.requestFocus(self.focusedVt().widget());
                }
            },
            else => {},
        }
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        switch (event) {
            .init => {
                for (self.vts, 0..) |_, i| {
                    const vt = &self.vts[i];
                    try vt.handleEvent(ctx, event);
                }
                ctx.redraw = true;
                return ctx.requestFocus(self.focusedVt().widget());
            },
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    ctx.quit = true;
                    return;
                }
            },
            .focus_in => return ctx.requestFocus(self.focusedVt().widget()),
            .tick => {
                if (self.offset < 0) {
                    const speed_cells_per_ms: i17 = @max(1, self.win_width / transition_time_ms);
                    self.offset += speed_cells_per_ms * 8;
                    self.offset = @min(self.offset, 0);
                    ctx.redraw = true;
                    return ctx.tick(8, self.widget());
                }
                if (self.offset > 0) {
                    const speed_cells_per_ms: i17 = @max(1, self.win_width / transition_time_ms);
                    self.offset -= speed_cells_per_ms * 8;
                    self.offset = @max(self.offset, 0);
                    ctx.redraw = true;
                    return ctx.tick(8, self.widget());
                }
            },
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        const max_size = ctx.max.size();
        self.win_width = max_size.width;

        const children = try ctx.arena.alloc(vxfw.SubSurface, 3);

        {
            // centered/focused child
            const border: vxfw.Border = .{
                .child = self.focusedVt().widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(4);
            padding.padding.left = 8;
            padding.padding.right = 8;
            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.offset },
                .surface = try padding.draw(ctx),
            };

            children[0] = button_child;
        }

        {
            // left child
            const left = self.focused -% 1;
            const border: vxfw.Border = .{
                .child = self.vts[left].widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(4);
            padding.padding.left = 8;
            padding.padding.right = 8;

            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.leftOffset() },
                .surface = try padding.draw(ctx),
            };

            children[1] = button_child;
        }

        {
            // right child
            const right = self.focused +% 1;
            const border: vxfw.Border = .{
                .child = self.vts[right].widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(4);
            padding.padding.left = 8;
            padding.padding.right = 8;

            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.rightOffset() },
                .surface = try padding.draw(ctx),
            };

            children[2] = button_child;
        }

        return .{
            .size = max_size,
            .widget = self.widget(),
            .focusable = false,
            .buffer = &.{},
            .children = children,
        };
    }

    fn focusedVt(self: *Model) *Terminal {
        return &self.vts[self.focused];
    }

    fn rightOffset(self: *Model) i17 {
        return self.offset + self.win_width - 12;
    }

    fn leftOffset(self: *Model) i17 {
        return self.offset - self.win_width + 12;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var app = try vxfw.App.init(gpa.allocator());
    defer app.deinit();

    var model: Model = .{
        .focused = 0,
        .offset = 0,
        .vts = undefined,
        .win_width = 0,
    };

    for (model.vts, 0..) |_, i| {
        const vt = &model.vts[i];
        try vt.init(gpa.allocator(), .{});
    }

    defer {
        for (model.vts, 0..) |_, i| {
            const vt = &model.vts[i];
            vt.close(null) catch {
                vt.deinit();
            };
            // vt.deinit();
        }
    }

    try app.run(model.widget(), .{});
}
