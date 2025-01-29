const std = @import("std");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal");

const Allocator = std.mem.Allocator;

const vxfw = vaxis.vxfw;

// Animation transition time for moving between panes
const transition_time_ms = 100;

pub const Model = struct {
    vts: [4]Terminal,
    focused: u2 = 0,
    offset: i17 = 0,
    win_width: u16 = 0,

    pub fn init(self: *Model, gpa: Allocator) !void {
        self.* = .{
            .focused = 0,
            .offset = 0,
            .vts = undefined,
            .win_width = 0,
        };

        for (self.vts, 0..) |_, i| {
            const vt = &self.vts[i];
            try vt.init(gpa, .{});
        }
    }

    pub fn deinit(self: *Model) void {
        for (self.vts, 0..) |_, i| {
            const vt = &self.vts[i];
            vt.close(null) catch {
                vt.deinit();
            };
        }
    }

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
        return self.captureEvent(ctx, event);
    }

    pub fn captureEvent(self: *Model, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
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
        return self.focusedVt().handleEvent(ctx, event);
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        return self.handleEvent(ctx, event);
    }

    pub fn handleEvent(self: *Model, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
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
        return self.draw(ctx);
    }

    pub fn draw(self: *Model, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const max_size = ctx.max.size();
        self.win_width = max_size.width;

        const children = try ctx.arena.alloc(vxfw.SubSurface, 3);
        for (&self.vts) |*vt| {
            vt.visible = false;
        }

        {
            // centered/focused child
            const border: vxfw.Border = .{
                .child = self.focusedVt().widget(),
            };
            self.focusedVt().visible = true;
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(2);
            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.offset },
                .surface = try padding.draw(ctx),
            };

            children[0] = button_child;
        }

        {
            // left child
            const left = self.focused -% 1;
            self.vts[left].visible = true;
            const border: vxfw.Border = .{
                .child = self.vts[left].widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(2);

            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.leftOffset() },
                .surface = try padding.draw(ctx),
            };

            children[1] = button_child;
        }

        {
            // right child
            const right = self.focused +% 1;
            self.vts[right].visible = true;
            const border: vxfw.Border = .{
                .child = self.vts[right].widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(2);

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

    pub fn focusedVt(self: *Model) *Terminal {
        return &self.vts[self.focused];
    }

    fn rightOffset(self: *Model) i17 {
        return self.offset + self.win_width - 3;
    }

    fn leftOffset(self: *Model) i17 {
        return self.offset - self.win_width + 3;
    }
};
