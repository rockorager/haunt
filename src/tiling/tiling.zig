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

    fn tick(self: Node, ctx: *vxfw.EventContext) anyerror!void {
        switch (self) {
            .vt => |vt| return vt.tick(ctx),
            .split_v => |split| {
                try split.lhs.tick(ctx);
                try split.rhs.tick(ctx);
            },
        }
    }

    pub fn nextFocus(self: Node) *Terminal {
        switch (self) {
            .vt => |vt| return vt,
            .split_v => |split| return split.lhs.nextFocus(),
        }
    }

    pub fn prune(self: *Node, gpa: Allocator, model: *Model) void {
        switch (self.*) {
            .vt => {},
            .split_v => |split| {
                if (split.lhs.childExited()) {
                    if (split.lhs.vt == model.focused) {
                        model.focused = split.rhs.nextFocus();
                    }
                    split.lhs.vt.deinit();
                    gpa.destroy(split.lhs.vt);
                    self.* = split.rhs;
                    gpa.destroy(split);
                    return;
                }
                if (split.rhs.childExited()) {
                    if (split.rhs.vt == model.focused) {
                        model.focused = split.lhs.nextFocus();
                    }
                    split.rhs.vt.deinit();
                    gpa.destroy(split.rhs.vt);
                    self.* = split.lhs;
                    gpa.destroy(split);
                    return;
                }
            },
        }
    }

    pub fn childExited(self: Node) bool {
        switch (self) {
            .vt => |vt| return vt.child_exited,
            else => return false,
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

    focused: *Terminal,

    prefix_pressed: bool,

    pub fn init(self: *Model, gpa: Allocator) !void {
        const vt = try gpa.create(Terminal);
        try vt.init(gpa, .{});
        self.* = .{
            .gpa = gpa,
            .root = .{ .vt = vt },
            .prefix_pressed = false,
            .focused = vt,
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

    pub fn shouldQuit(self: Model) bool {
        return self.root == .vt and self.root.vt.child_exited;
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
                        const lhs = self.root;
                        split.* = .{
                            .lhs = .{ .vt = lhs.vt },
                            .rhs = .{ .vt = rhs },
                        };
                        self.root = .{ .split_v = split };
                        return ctx.consumeAndRedraw();
                    }
                    if (key.matches('a', .{})) {
                        // Split the focused node vertically
                        const split = try self.gpa.create(SplitVertical);
                        const rhs = try self.gpa.create(Terminal);
                        try rhs.init(self.gpa, .{});
                        const lhs = self.root;
                        split.* = .{
                            .lhs = .{ .vt = lhs.vt },
                            .rhs = .{ .vt = rhs },
                            .direction = .horizontal,
                        };
                        self.root = .{ .split_v = split };
                        return ctx.consumeAndRedraw();
                    }
                    if (key.matches('l', .{})) {
                        switch (self.root) {
                            .split_v => |split| {
                                self.focused = split.rhs.vt;
                                try ctx.requestFocus(split.rhs.vt.widget());
                                return ctx.consumeAndRedraw();
                            },
                            else => {},
                        }
                    }
                    if (key.matches('h', .{})) {
                        switch (self.root) {
                            .split_v => |split| {
                                self.focused = split.lhs.vt;
                                try ctx.requestFocus(split.lhs.vt.widget());
                                return ctx.consumeAndRedraw();
                            },
                            else => {},
                        }
                    }
                }
            },
            else => {},
        }
        // self.root.prune(self.gpa, self.focused, ctx);
        // return self.root.handleEvent(ctx, event);
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        return self.handleEvent(ctx, event);
    }

    pub fn handleEvent(self: *Model, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        switch (event) {
            .init => {
                ctx.redraw = true;
                try ctx.tick(8, self.widget());
                return ctx.requestFocus(self.focusedVt().widget());
            },
            .tick => {
                // tick each terminal event loop
                try self.root.tick(ctx);
                // Prune the tree
                self.root.prune(self.gpa, self);
                // Update focus
                try ctx.requestFocus(self.focusedVt().widget());
                // Add another tick
                try ctx.tick(8, self.widget());
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
        self.root.prune(self.gpa, self);
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
        return self.focused;
    }
};
