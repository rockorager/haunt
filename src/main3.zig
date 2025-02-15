const std = @import("std");
const vaxis = @import("vaxis");

const Terminal = @import("Terminal");

const Allocator = std.mem.Allocator;

const vxfw = vaxis.vxfw;

// Animation transition time for moving between panes
const transition_time_ms = 100;

const Model = struct {
    gpa: std.mem.Allocator,
    text_field: vxfw.TextField,
    vts: std.ArrayList(*Command),
    last_size: vaxis.Winsize,
    cwd: []const u8,
    focused: ?usize,

    pub fn widget(self: *Model) vxfw.Widget {
        return .{
            .userdata = self,
            .captureHandler = Model.typeErasedCaptureHandler,
            .eventHandler = Model.typeErasedEventHandler,
            .drawFn = Model.typeErasedDrawFn,
        };
    }

    pub fn deinit(self: *Model) void {
        self.text_field.deinit();
        for (self.vts.items) |vt| {
            vt.deinit(self.gpa);
            self.gpa.destroy(vt);
        }
        self.vts.deinit();
        self.gpa.free(self.cwd);
    }

    fn typeErasedCaptureHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        switch (event) {
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    ctx.quit = true;
                    return ctx.consumeEvent();
                }
                if (key.matches('n', .{ .ctrl = true })) {
                    if (self.focused) |focused| {
                        if (focused + 1 >= self.vts.items.len) {
                            self.focused = null;
                            try ctx.requestFocus(self.text_field.widget());
                            return ctx.consumeAndRedraw();
                        }
                        self.focused = focused + 1;
                        try ctx.requestFocus(self.vts.items[focused + 1].widget());
                        return ctx.consumeAndRedraw();
                    }
                }
                if (key.matches('p', .{ .ctrl = true })) {
                    if (self.focused) |focused| {
                        self.focused = focused -| 1;
                        try ctx.requestFocus(self.vts.items[focused -| 1].widget());
                        return ctx.consumeAndRedraw();
                    }
                    if (self.vts.items.len > 0) {
                        self.focused = self.vts.items.len - 1;
                        try ctx.requestFocus(self.vts.items[self.focused.?].widget());
                        return ctx.consumeAndRedraw();
                    }
                }
            },
            else => {},
        }
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Model = @ptrCast(@alignCast(ptr));
        switch (event) {
            .init => {
                try ctx.tick(8, self.widget());
                return ctx.requestFocus(self.text_field.widget());
            },
            .key_press => |key| {
                _ = key;
            },
            .focus_in => {},
            .tick => {
                for (self.vts.items) |vt| {
                    try vt.tick(ctx);
                }
                try ctx.tick(8, self.widget());
                if (self.focused != null) return;
                if (self.vts.getLastOrNull()) |vt| {
                    // We focus the last widget if it hasn't exited, and is in the alt screen
                    if (!vt.vt.child_exited and vt.inAltScreen()) {
                        try ctx.requestFocus(vt.vt.widget());
                    } else {
                        try ctx.requestFocus(self.text_field.widget());
                    }
                }
            },
            else => {},
        }
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Model = @ptrCast(@alignCast(ptr));
        const max_size = ctx.max.size();

        var children = std.ArrayList(vxfw.SubSurface).init(ctx.arena);
        // If our last vt is in the alt screen, we draw the whole thing
        if (self.vts.getLastOrNull()) |vt| {
            if (vt.inAltScreen()) {
                const child: vxfw.SubSurface = .{
                    .origin = .{ .row = 0, .col = 0 },
                    .surface = try vt.draw(ctx),
                };
                try children.append(child);
                return .{
                    .size = max_size,
                    .widget = self.widget(),
                    .focusable = false,
                    .buffer = &.{},
                    .children = children.items,
                };
            }
        }

        const width = max_size.width;
        const height = max_size.height - 1;
        self.last_size = .{
            .rows = height,
            .cols = width,
            .x_pixel = ctx.cell_size.width * width,
            .y_pixel = ctx.cell_size.height * height,
        };

        var prompt_row: u16 = 0;

        for (self.vts.items, 0..) |vt, i| {
            if (self.focused) |focused| {
                vt.focused = i == focused;
            } else {
                vt.focused = false;
            }
            // if (vt.vt.child_exited and
            //     (i < self.vts.items.len - 1 or !vt.hasOutputOnStdout()))
            // {
            //     vt.collapsed = true;
            // }
            const vt_ctx = ctx.withConstraints(ctx.min, .{
                .width = ctx.max.width,
                .height = @intCast(max_size.height / 2),
            });
            const child: vxfw.SubSurface = .{
                .origin = .{ .row = prompt_row, .col = 0 },
                .surface = try vt.draw(vt_ctx),
            };
            try children.append(child);
            prompt_row += child.surface.size.height;
        }

        {
            const prompt: vxfw.RichText = .{
                .text = &.{
                    .{ .text = " " },
                    .{ .text = self.cwd, .style = .{ .fg = .{ .index = 4 } } },
                    .{ .text = " " },
                    .{ .text = "", .style = .{ .fg = .{ .index = 5 } } },
                },
            };
            const prompt_child: vxfw.SubSurface = .{
                .origin = .{ .row = prompt_row, .col = 0 },
                .surface = try prompt.draw(ctx),
            };
            try children.append(prompt_child);

            const prompt_width = prompt_child.surface.size.width;

            const text_field_ctx = ctx.withConstraints(ctx.min, .{
                .width = max_size.width -| prompt_width -| 1,
                .height = 1,
            });
            const child: vxfw.SubSurface = .{
                .origin = .{ .row = prompt_row, .col = prompt_width + 1 },
                .surface = try self.text_field.draw(text_field_ctx),
            };

            try children.append(child);
        }

        return .{
            .size = max_size,
            .widget = self.widget(),
            .focusable = false,
            .buffer = &.{},
            .children = children.items,
        };
    }

    fn onSubmit(maybe_ptr: ?*anyopaque, ctx: *vxfw.EventContext, cmdline: []const u8) anyerror!void {
        if (cmdline.len == 0) return;
        const ptr = maybe_ptr orelse unreachable;
        const self: *Model = @ptrCast(@alignCast(ptr));
        const gpa = self.gpa;

        var env = std.process.EnvMap.init(self.gpa);
        defer env.deinit();

        // var columns_buf: [8]u8 = undefined;
        // const columns = try std.fmt.bufPrint(&columns_buf, "{d}", .{self.last_size.cols});
        // try env.put("COLUMNS", columns);
        //
        // var row_buf: [8]u8 = undefined;
        // const rows = try std.fmt.bufPrint(&row_buf, "{d}", .{self.last_size.rows});
        // try env.put("LINES", rows);

        const vt = try gpa.create(Command);
        // vt will dupe cmdline
        try vt.init(gpa, .{ .command = cmdline, .size = self.last_size, .env = &env });

        try self.vts.append(vt);

        self.text_field.clearAndFree();

        ctx.redraw = true;
    }
};

const Command = struct {
    vt: Terminal,
    collapsed: bool,
    cwd: []const u8,
    focused: bool,

    pub fn init(self: *Command, gpa: Allocator, opts: Terminal.Options) !void {
        self.* = .{
            .vt = undefined,
            .collapsed = false,
            .cwd = try getCwd(gpa),
            .focused = false,
        };
        try self.vt.init(gpa, opts);
    }

    pub fn deinit(self: *Command, gpa: Allocator) void {
        gpa.free(self.cwd);
        self.vt.deinit();
    }

    pub fn tick(self: *Command, ctx: *vxfw.EventContext) anyerror!void {
        return self.vt.tick(ctx);
    }

    pub fn widget(self: *Command) vxfw.Widget {
        return .{
            .userdata = self,
            .eventHandler = Command.typeErasedEventHandler,
            .drawFn = Command.typeErasedDrawFn,
        };
    }

    fn typeErasedEventHandler(ptr: *anyopaque, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const self: *Command = @ptrCast(@alignCast(ptr));
        switch (event) {
            .key_press => |key| {
                std.log.debug("here 1", .{});
                if (key.matches(vaxis.Key.tab, .{})) {
                    std.log.debug("here 2", .{});
                    self.collapsed = !self.collapsed;
                    std.log.debug("collapsed={}", .{self.collapsed});
                    return ctx.consumeAndRedraw();
                }
            },
            else => {},
        }
        return self.vt.handleEvent(ctx, event);
    }

    fn typeErasedDrawFn(ptr: *anyopaque, ctx: vxfw.DrawContext) std.mem.Allocator.Error!vxfw.Surface {
        const self: *Command = @ptrCast(@alignCast(ptr));
        return self.draw(ctx);
    }

    pub fn draw(self: *Command, ctx: vxfw.DrawContext) Allocator.Error!vxfw.Surface {
        var children = std.ArrayList(vxfw.SubSurface).init(ctx.arena);

        const caret_style: vaxis.Style = if (self.focused)
            .{ .fg = .{ .index = 1 } }
        else
            .{};

        if (self.collapsed) {
            const rich_text: vxfw.RichText =
                .{
                .text = &.{
                    .{ .text = " ", .style = caret_style },
                    .{ .text = self.cwd, .style = .{ .fg = .{ .index = 4 } } },
                    .{ .text = " " },
                    .{ .text = self.vt.command },
                },
            };

            const prompt_child: vxfw.SubSurface = .{
                .origin = .{ .row = 0, .col = 0 },
                .surface = try rich_text.draw(ctx),
            };
            try children.append(prompt_child);

            return .{
                .size = prompt_child.surface.size,
                .widget = self.widget(),
                .focusable = true,
                .buffer = &.{},
                .children = children.items,
            };
        }

        // Not collapsed. We draw the command and it's output
        const rich_text: vxfw.RichText = .{
            .text = &.{
                .{ .text = " ", .style = caret_style },
                .{ .text = self.cwd, .style = .{ .fg = .{ .index = 4 } } },
                .{ .text = " " },
                .{ .text = self.vt.command },
            },
        };
        const prompt_child: vxfw.SubSurface = .{
            .origin = .{ .row = 0, .col = 0 },
            .surface = try rich_text.draw(ctx),
        };
        try children.append(prompt_child);

        const vt_ctx = ctx.withConstraints(
            ctx.min,
            .{ .width = ctx.max.width.?, .height = ctx.max.height.? -| 1 },
        );
        var vt_surface = try self.vt.draw(vt_ctx);
        const lines = self.linesOfOutput();
        if (lines < vt_surface.size.height) {
            vt_surface.buffer = vt_surface.buffer[0 .. lines * vt_surface.size.width];
            vt_surface.size.height = lines;
        }
        const child: vxfw.SubSurface = .{
            .origin = .{ .row = 1, .col = 0 },
            .surface = vt_surface,
        };
        try children.append(child);

        const size: vxfw.Size = .{
            .width = ctx.max.size().width,
            .height = children.items[0].surface.size.height + children.items[1].surface.size.height,
        };

        return .{
            .size = size,
            .widget = self.widget(),
            .focusable = true,
            .buffer = &.{},
            .children = children.items,
        };
    }

    fn hasOutputOnStdout(self: *Command) bool {
        // if the child hasn't exited, we assume it could still print to stdout
        if (!self.vt.child_exited) return true;

        // If we have a resize, we might have output on stdout and not know yet
        if (self.vt.pending_renderer_resize.load(.unordered)) return true;

        self.vt.screen_mutex.lock();
        defer self.vt.screen_mutex.unlock();
        const default_cell: vaxis.Cell = .{};

        for (self.vt.screen.buf) |cell| {
            if (cell.eql(default_cell)) continue;
            return true;
        } else return false;
    }

    fn linesOfOutput(self: *Command) u16 {
        self.vt.screen_mutex.lock();
        defer self.vt.screen_mutex.unlock();
        // If the child hasn't exited, or we have a pending resize, we will exit early
        // if (!self.vt.child_exited or self.vt.pending_renderer_resize.load(.unordered)) {
        //     return self.vt.screen.height;
        // }
        var lines: u16 = 1;
        var col: u16 = 0;
        var last_line_with_output: u16 = 0;
        const default_cell: vaxis.Cell = .{};
        for (self.vt.screen.buf) |cell| {
            col += 1;
            if (col == self.vt.screen.width) {
                col = 0;
                lines += 1;
            }
            if (cell.eql(default_cell)) continue;
            last_line_with_output = lines;
        }
        return last_line_with_output;
    }

    fn inAltScreen(self: *Command) bool {
        self.vt.renderer_mutex.lock();
        defer self.vt.renderer_mutex.unlock();
        return self.vt.renderer_state.terminal.active_screen == .alternate;
    }
};

pub fn getCwd(gpa: Allocator) ![]const u8 {
    const home = try std.process.getEnvVarOwned(gpa, "HOME");
    defer gpa.free(home);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = try std.process.getCwd(&buf);

    var list = try std.ArrayList(u8).initCapacity(gpa, cwd.len);
    defer list.deinit();
    list.appendSliceAssumeCapacity(cwd);

    if (home.len > 0 and std.mem.startsWith(u8, cwd, home)) {
        list.replaceRange(0, home.len, "~") catch unreachable;
    }
    return list.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var app = try vxfw.App.init(gpa.allocator());
    defer app.deinit();

    var model: Model = undefined;
    model = .{
        .gpa = gpa.allocator(),
        .text_field = .{
            .buf = vxfw.TextField.Buffer.init(gpa.allocator()),
            .unicode = &app.vx.unicode,
            .userdata = &model,
            .onChange = null,
            .onSubmit = Model.onSubmit,
        },
        .vts = std.ArrayList(*Command).init(gpa.allocator()),
        .last_size = .{ .rows = 0, .cols = 0, .x_pixel = 0, .y_pixel = 0 },
        .cwd = try getCwd(gpa.allocator()),
        .focused = null,
    };
    defer model.deinit();

    try app.run(model.widget(), .{});
}

test {
    _ = @import("server.zig");
}
