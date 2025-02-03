const std = @import("std");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal");
const SplitVertical = @import("SplitVertical.zig");

const Allocator = std.mem.Allocator;

const vxfw = vaxis.vxfw;

pub const Node = union(enum) {
    split_v: *SplitVertical,
    vt: *Terminal,

    pub fn handleEvent(self: Node, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        switch (self) {
            .split_v => |v| return v.widget().handleEvent(ctx, event),
            .vt => |v| return v.widget().handleEvent(ctx, event),
        }
    }

    pub fn draw(self: Node, ctx: vxfw.DrawContext) Allocator.Error!vxfw.Surface {
        switch (self) {
            .split_v => |v| return v.widget().draw(ctx),
            .vt => |v| return v.widget().draw(ctx),
        }
    }
};

const SplitHorizontal = struct {
    top: Node,
    bot: Node,
};

pub const Model = struct {
    gpa: Allocator,
    root: Node,

    prefix_pressed: bool,

    pub fn init(self: *Model, gpa: Allocator) !void {
        const vt = try gpa.create(Terminal);
        try vt.init(gpa, .{});
        self.* = .{
            .gpa = gpa,
            .root = .{ .vt = vt },
            .prefix_pressed = false,
        };
    }

    pub fn deinit(self: *Model) void {
        self.root.vt.close(null) catch {};
        self.gpa.destroy(self.root.vt);
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
                if (key.matches('b', .{ .ctrl = true })) {
                    self.prefix_pressed = true;
                    return ctx.consumeEvent();
                }
                if (self.prefix_pressed) {
                    self.prefix_pressed = false;
                    if (key.matches('v', .{})) {
                        // Split the focused node vertically
                        const split = try self.gpa.create(SplitVertical);
                        const rhs = try self.gpa.create(Terminal);
                        try rhs.init(self.gpa, .{});
                        try rhs.handleEvent(ctx, .init);
                        const lhs = self.root;
                        split.* = .{ .lhs = .{ .vt = lhs.vt }, .rhs = .{ .vt = rhs }, .width = 20 };
                        self.root = .{ .split_v = split };
                        return ctx.consumeAndRedraw();
                    }
                }
            },
            else => {},
        }
        // return self.root.handleEvent(ctx, event);
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        return self.handleEvent(ctx, event);
    }

    pub fn handleEvent(self: *Model, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        switch (event) {
            .init => {
                try self.root.vt.handleEvent(ctx, event);
                ctx.redraw = true;
                return ctx.requestFocus(self.focusedVt().widget());
            },
            .focus_in => return ctx.requestFocus(self.focusedVt().widget()),
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        return self.draw(ctx);
    }

    pub fn draw(self: *Model, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const max_size = ctx.max.size();

        var children = std.ArrayList(vxfw.SubSurface).init(ctx.arena);

        const content = try self.root.draw(ctx);
        const content_sub: vxfw.SubSurface = .{
            .origin = .{ .row = 0, .col = 0 },
            .surface = content,
        };

        try children.append(content_sub);

        return .{
            .size = max_size,
            .widget = self.widget(),
            .focusable = false,
            .buffer = &.{},
            .children = children.items,
        };
    }

    pub fn focusedVt(self: *Model) *Terminal {
        return switch (self.root) {
            .vt => |vt| vt,
            .split_v => |split| split.lhs.vt,
        };
    }
};
