const std = @import("std");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal");

const Allocator = std.mem.Allocator;

const vxfw = vaxis.vxfw;

// Animation transition time for moving between panes
const transition_time_ms = 100;

pub const Model = struct {
    gpa: Allocator,
    vts: std.ArrayList(*Terminal),
    focused: u2 = 0,
    offset: i17 = 0,
    win_width: u16 = 0,

    pub fn init(self: *Model, gpa: Allocator) !void {
        self.* = .{
            .gpa = gpa,
            .focused = 0,
            .offset = 0,
            .vts = std.ArrayList(*Terminal).init(gpa),
            .win_width = 0,
        };

        try self.newTerminal();
    }

    pub fn deinit(self: *Model) void {
        for (self.vts.items) |vt| {
            vt.close(null) catch {
                vt.deinit();
            };
        }
        self.vts.deinit();
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
        if (self.vts.items.len == 0) return;
        switch (event) {
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    ctx.quit = true;
                    return ctx.consumeEvent();
                }
                if (key.matches(vaxis.Key.right, .{ .ctrl = true })) {
                    if (self.focused == self.vts.items.len - 1) return;
                    self.focused += 1;
                    ctx.consumeEvent();
                    self.offset = self.rightOffset();
                    try ctx.tick(8, self.widget());
                    return ctx.requestFocus(self.focusedVt().widget());
                }
                if (key.matches(vaxis.Key.left, .{ .ctrl = true })) {
                    if (self.focused == 0) return;
                    self.focused -= 1;
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
                for (self.vts.items) |vt| {
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
                if (self.vts.items.len == 0) return;
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

        // {
        //     var i: usize = 0;
        //     while (i < self.vts.items.len) {
        //         const vt = self.vts.items[i];
        //         if (vt.child_exited) {
        //             // Child exited so we need to clean it up
        //             _ = self.vts.orderedRemove(i);
        //             vt.deinit();
        //             continue;
        //         }
        //         i += 1;
        //     }
        // }

        if (self.vts.items.len == 0) {
            // TODO: set should_quit
            return .{
                .size = max_size,
                .widget = self.widget(),
                .focusable = false,
                .buffer = &.{},
                .children = &.{},
            };
        }

        var children = std.ArrayList(vxfw.SubSurface).init(ctx.arena);

        for (self.vts.items) |vt| {
            vt.visible = false;
        }

        const maybe_left: ?usize = if (self.focused == 0) null else self.focused - 1;
        const maybe_right: ?usize = if (self.focused < self.vts.items.len -| 1)
            self.focused + 1
        else
            null;

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

            try children.append(button_child);
        }

        if (maybe_left) |left| {
            // left child
            const vt = self.vts.items[left];
            vt.visible = true;
            const border: vxfw.Border = .{
                .child = vt.widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(2);

            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.leftOffset() },
                .surface = try padding.draw(ctx),
            };

            try children.append(button_child);
        }

        if (maybe_right) |right| {
            // right child
            const vt = self.vts.items[right];
            vt.visible = true;
            const border: vxfw.Border = .{
                .child = vt.widget(),
            };
            var padding: vxfw.Padding = .{
                .child = border.widget(),
            };
            padding.padding = vxfw.Padding.all(2);

            const button_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = self.rightOffset() },
                .surface = try padding.draw(ctx),
            };

            try children.append(button_child);
        }

        return .{
            .size = max_size,
            .widget = self.widget(),
            .focusable = false,
            .buffer = &.{},
            .children = children.items,
        };
    }

    pub fn focusedVt(self: *Model) *Terminal {
        return self.vts.items[self.focused];
    }

    fn rightOffset(self: *Model) i17 {
        return self.offset + self.win_width - 3;
    }

    fn leftOffset(self: *Model) i17 {
        return self.offset - self.win_width + 3;
    }

    pub fn newTerminal(self: *Model) !void {
        const vt = try self.gpa.create(Terminal);
        try vt.init(self.gpa, .{});
        try self.vts.append(vt);
    }
};
