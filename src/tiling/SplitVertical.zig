const std = @import("std");
const vaxis = @import("vaxis");

const tiling = @import("tiling.zig");

const Allocator = std.mem.Allocator;
const Node = tiling.Node;

const vxfw = vaxis.vxfw;

const SplitVertical = @This();

lhs: tiling.Node,
rhs: tiling.Node,
parent: ?tiling.Node = null,
style: vaxis.Style = .{},
/// Pending width change
lhs_pending_width: ?u16 = null,
lhs_width: u16 = 0,
direction: enum { vertical, horizontal } = .vertical,

// 0 - 100 for width ratio that goes to the left side
ratio: u8 = 50,

/// Statically allocated children
children: [2]vxfw.SubSurface = undefined,

// State
pressed: bool = false,
mouse_set: bool = false,

pub fn widget(self: *const SplitVertical) vxfw.Widget {
    return .{
        .userdata = @constCast(self),
        .captureHandler = typeErasedEventHandler,
        .eventHandler = typeErasedEventHandler,
        .drawFn = typeErasedDrawFn,
    };
}

fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
    const self: *SplitVertical = @ptrCast(@alignCast(ptr));
    switch (event) {
        .mouse_leave => {
            self.pressed = false;
            return;
        },
        .mouse => {},
        else => return,
    }
    const mouse = event.mouse;
    switch (self.direction) {
        .vertical => {
            const separator_col = self.lhs_width + 1;

            // If we are on the separator, we always set the mouse shape
            if (mouse.col == separator_col) {
                try ctx.setMouseShape(.@"ew-resize");
                self.mouse_set = true;
                // Set pressed state if we are a left click
                if (mouse.type == .press and mouse.button == .left) {
                    self.pressed = true;
                }
            } else if (self.mouse_set) {
                // If we have set the mouse state and *aren't* over the separator, default the mouse state
                try ctx.setMouseShape(.default);
                self.mouse_set = false;
            }

            // On release, we reset state
            if (mouse.type == .release) {
                self.pressed = false;
                self.mouse_set = false;
                try ctx.setMouseShape(.default);
            }

            // If pressed, we always keep the mouse shape and we update the width
            if (self.pressed) {
                self.mouse_set = true;
                try ctx.setMouseShape(.@"ew-resize");
                self.lhs_pending_width = mouse.col -| 1;
                ctx.consume_event = true;
            }
        },
        .horizontal => {
            const separator_row = self.lhs_width + 1;

            // If we are on the separator, we always set the mouse shape
            if (mouse.col == separator_row) {
                try ctx.setMouseShape(.@"ns-resize");
                self.mouse_set = true;
                // Set pressed state if we are a left click
                if (mouse.type == .press and mouse.button == .left) {
                    self.pressed = true;
                }
            } else if (self.mouse_set) {
                // If we have set the mouse state and *aren't* over the separator, default the mouse state
                try ctx.setMouseShape(.default);
                self.mouse_set = false;
            }

            // On release, we reset state
            if (mouse.type == .release) {
                self.pressed = false;
                self.mouse_set = false;
                try ctx.setMouseShape(.default);
            }

            // If pressed, we always keep the mouse shape and we update the width
            if (self.pressed) {
                self.mouse_set = true;
                try ctx.setMouseShape(.@"ns-resize");
                self.lhs_pending_width = mouse.row -| 1;
                ctx.consume_event = true;
            }
        },
    }
}

fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) Allocator.Error!vxfw.Surface {
    const self: *SplitVertical = @ptrCast(@alignCast(ptr));
    // Fills entire space
    const max = ctx.max.size();
    std.debug.assert(max.width != 0);

    switch (self.direction) {
        .vertical => {
            const lhs_width = blk: {
                // If we have a pending width change, we use that and calculate a new ratio
                if (self.lhs_pending_width) |lhs_width| {
                    self.ratio = @intCast((lhs_width * 100) / max.width);
                    self.lhs_pending_width = null;
                    break :blk lhs_width;
                }

                // Otherwise, we calculate the lhs width from our ratio
                if (self.ratio == 0) {
                    break :blk 0;
                } else {
                    break :blk (max.width * self.ratio) / 100;
                }
            };

            // Store this for handling mouse events
            self.lhs_width = lhs_width;
            const lhs_min: vxfw.Size = .{ .width = lhs_width, .height = max.height };
            const lhs_max: vxfw.MaxSize = .{ .width = lhs_width, .height = max.height };

            const rhs_min: vxfw.Size = .{ .width = max.width - lhs_width - 2, .height = max.height };
            const rhs_max: vxfw.MaxSize = .{ .width = max.width - lhs_width - 2, .height = max.height };

            const lhs_ctx = ctx.withConstraints(lhs_min, lhs_max);
            const lhs_surface = try self.lhs.draw(lhs_ctx);

            self.children[0] = .{
                .surface = lhs_surface,
                .origin = .{ .row = 0, .col = 0 },
            };
            const rhs_ctx = ctx.withConstraints(rhs_min, rhs_max);
            const rhs_surface = try self.rhs.draw(rhs_ctx);
            self.children[1] = .{
                .surface = rhs_surface,
                .origin = .{ .row = 0, .col = self.lhs_width + 2 },
            };

            var surface = try vxfw.Surface.initWithChildren(ctx.arena, self.widget(), max, &self.children);
            for (0..max.height) |row| {
                surface.writeCell(self.lhs_width + 1, @intCast(row), .{
                    .char = .{ .grapheme = "│", .width = 1 },
                    .style = self.style,
                });
            }
            return surface;
        },
        .horizontal => {
            const lhs_width = blk: {
                // If we have a pending width change, we use that and calculate a new ratio
                if (self.lhs_pending_width) |lhs_width| {
                    self.ratio = @intCast((lhs_width * 100) / max.height);
                    self.lhs_pending_width = null;
                    break :blk lhs_width;
                }

                // Otherwise, we calculate the lhs width from our ratio
                if (self.ratio == 0) {
                    break :blk 0;
                } else {
                    break :blk (max.height * self.ratio) / 100;
                }
            };
            // Store this for handling mouse events
            self.lhs_width = lhs_width;

            const lhs_min: vxfw.Size = .{ .width = max.width, .height = lhs_width };
            const lhs_max: vxfw.MaxSize = .{ .width = max.width, .height = lhs_width };

            const rhs_min: vxfw.Size = .{ .width = max.width, .height = max.height - lhs_width - 2 };
            const rhs_max: vxfw.MaxSize = .{ .width = max.width, .height = max.height - lhs_width - 2 };

            const lhs_ctx = ctx.withConstraints(lhs_min, lhs_max);
            const lhs_surface = try self.lhs.draw(lhs_ctx);

            self.children[0] = .{
                .surface = lhs_surface,
                .origin = .{ .row = 0, .col = 0 },
            };
            const rhs_ctx = ctx.withConstraints(rhs_min, rhs_max);
            const rhs_surface = try self.rhs.draw(rhs_ctx);
            self.children[1] = .{
                .surface = rhs_surface,
                .origin = .{ .row = self.lhs_width + 2, .col = 0 },
            };

            var surface = try vxfw.Surface.initWithChildren(ctx.arena, self.widget(), max, &self.children);
            for (0..max.width) |col| {
                surface.writeCell(@intCast(col), self.lhs_width + 1, .{
                    .char = .{ .grapheme = "─", .width = 1 },
                    .style = self.style,
                });
            }
            return surface;
        },
    }
}
