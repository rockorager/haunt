const net = std.net;
const posix = std.posix;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const xev = @import("xev");

const protocol = @import("protocol.zig");
const ScrollingWM = @import("scrolling.zig").Model;
const TilingWM = @import("tiling/tiling.zig").Model;

const std = @import("std");
pub const Server = struct {
    gpa: Allocator,

    /// Absolute path to the socket
    sockpath: []const u8,

    /// xev server
    tcp: xev.TCP,

    accept_c: xev.Completion,

    connections: std.ArrayList(*RpcConnection),
    sessions: std.ArrayList(*Session),

    unicode: vaxis.Unicode,

    timer: xev.Timer,
    timer_c: xev.Completion,

    const log = std.log.scoped(.server);

    /// Initialize a server listening at sockpath. This will create a unix socket. Asserts that no
    /// server is currently listening at this address. sockpath will be duped and does not need to live
    /// longer than this call
    pub fn init(
        self: *Server,
        gpa: Allocator,
        sockpath: []const u8,
        loop: *xev.Loop,
    ) !void {
        assert(std.fs.path.isAbsolute(sockpath));
        assert(!serverIsRunningAtPath(sockpath));

        const path = try gpa.dupe(u8, sockpath);
        const address = try net.Address.initUnix(path);
        self.* = .{
            .gpa = gpa,
            .sockpath = path,
            .tcp = try xev.TCP.init(address),
            .accept_c = undefined,
            .connections = std.ArrayList(*RpcConnection).init(gpa),
            .sessions = std.ArrayList(*Session).init(gpa),
            .unicode = try vaxis.Unicode.init(gpa),
            .timer = try xev.Timer.init(),
            .timer_c = undefined,
        };

        try self.tcp.bind(address);
        try self.tcp.listen(1);
        log.debug("server listening at {s}...", .{path});

        vxfw.DrawContext.init(&self.unicode, .unicode);

        self.tcp.accept(loop, &self.accept_c, Server, self, Server.acceptCallback);
        self.timer.run(loop, &self.timer_c, 8, Server, self, Server.timerCallback);
    }

    fn timerCallback(
        maybe_self: ?*Server,
        loop: *xev.Loop,
        completion: *xev.Completion,
        result: xev.Timer.RunError!void,
    ) xev.CallbackAction {
        _ = completion; // autofix
        _ = loop; // autofix
        const self = maybe_self orelse unreachable;
        result catch |err| {
            std.log.err("timer error: {}", .{err});
            return .rearm;
        };

        for (self.sessions.items) |session| {
            session.checkTimers() catch |err| {
                std.log.err("timer error: {}", .{err});
            };
        }

        return .rearm;
    }

    pub fn deinit(
        self: *Server,
    ) void {
        for (self.connections.items) |connection| {
            const detach = false;
            connection.deinit(detach);
            self.gpa.destroy(connection);
        }
        for (self.sessions.items) |session| {
            session.deinit();
            self.gpa.destroy(session);
        }
        self.sessions.deinit();

        self.connections.deinit();

        self.unicode.deinit();

        posix.close(self.tcp.fd);
        posix.unlink(self.sockpath) catch {};
        self.gpa.free(self.sockpath);
    }

    fn acceptCallback(
        maybe_self: ?*Server,
        loop: *xev.Loop,
        completion: *xev.Completion,
        result: xev.TCP.AcceptError!xev.TCP,
    ) xev.CallbackAction {
        const self = maybe_self orelse unreachable;

        const conn = result catch |err| {
            std.log.err("error: {}", .{err});
            return .disarm;
        };

        log.debug("accepting connection fd={d}", .{conn.fd});

        // Heap allocate because we always want stable pointers for callbacks
        const connection = self.gpa.create(RpcConnection) catch |err| {
            std.log.err("error: {}", .{err});
            return .disarm;
        };

        connection.* = .{
            .tcp = conn,
            .server = self,
            .buffer = undefined,
            .read_c = undefined,
            .queue = std.ArrayList(u8).init(self.gpa),
            .session = null,
            .prefix_pressed = false,
            .connected = std.atomic.Value(bool).init(true),
        };

        self.connections.append(connection) catch |err| {
            std.log.err("error: {}", .{err});
            return .disarm;
        };

        connection.tcp.read(
            loop,
            &connection.read_c,
            .{ .slice = &connection.buffer },
            RpcConnection,
            connection,
            RpcConnection.readCallback,
        );

        _ = completion; // autofix
        return .rearm;
    }

    fn removeConnection(self: *Server, connection: *const RpcConnection) void {
        for (self.connections.items, 0..) |item, i| {
            if (item == connection) {
                std.log.debug("removing connection from server {x}", .{@intFromPtr(connection)});
                _ = self.connections.swapRemove(i);
                break;
            }
        }
    }

    fn getSession(self: *Server, name: []const u8) ?*Session {
        for (self.sessions.items) |session| {
            if (std.mem.eql(u8, session.name, name)) {
                return session;
            }
        }
        return null;
    }

    fn removeSession(self: *Server, session: *const Session) void {
        for (self.sessions.items, 0..) |s, i| {
            if (s == session) {
                _ = self.sessions.swapRemove(i);
                break;
            }
        }
    }
};

/// Checks if a server is already listening at abs_path. If a server is not listening, this attempts
/// to remove the file
pub fn serverIsRunningAtPath(abs_path: []const u8) bool {
    // Any error means nothing is listening
    const stream = net.connectUnixSocket(abs_path) catch {
        std.posix.unlink(abs_path) catch {};
        return false;
    };
    stream.close();
    return true;
}

pub const RpcConnection = struct {
    tcp: xev.TCP,
    server: *Server,
    buffer: [4096]u8,
    read_c: xev.Completion,
    queue: std.ArrayList(u8),

    session: ?SessionConnection,

    prefix_pressed: bool,

    connected: std.atomic.Value(bool),

    const ReadError = Allocator.Error || protocol.Error;

    pub fn deinit(self: *RpcConnection, detach: bool) void {
        self.server.removeConnection(self);
        self.queue.deinit();
        if (self.session) |*session| {
            session.deinit(self.server.gpa, detach);
            self.session = null;
        }
        self.sendCloseMessage() catch {};
        posix.close(self.tcp.fd);
        self.connected.store(false, .unordered);
    }

    pub fn deinitLocked(self: *RpcConnection, detach: bool) void {
        self.connected.store(false, .unordered);
        self.server.removeConnection(self);
        if (self.session) |*session| {
            session.deinitLocked(self.server.gpa, detach);
            self.session = null;
        }
        self.queue.deinit();
        self.sendCloseMessage() catch {};
        posix.close(self.tcp.fd);
    }

    fn readCallback(
        maybe_self: ?*RpcConnection,
        loop: *xev.Loop,
        completion: *xev.Completion,
        tcp: xev.TCP,
        buffer: xev.ReadBuffer,
        result: xev.Stream.ReadError!usize,
    ) xev.CallbackAction {
        _ = buffer; // autofix
        _ = tcp; // autofix
        _ = completion; // autofix

        const self = maybe_self orelse unreachable;
        const n = result catch |err| {
            if (!self.connected.load(.unordered)) return .disarm;
            std.log.err("read error: {}", .{err});
            if (err == error.EOF) {
                // Set that we are disconnected
                self.connected.store(false, .unordered);

                // If we have a session, trigger a close of the tty thread
                if (self.session) |*session| {
                    // Queue a message to close our tty, if we need to. If we have a tty, it will
                    // pick this up in it's read thread and deinit in process events
                    session.queueTtyThreadClose() catch |err2| {
                        std.log.err("close ttythread error: {}", .{err2});
                    };
                } else {

                    // Otherwise we deinit ourself
                    self.deinit(false);
                }
            }
            return .disarm;
        };
        self.handleRead(loop, n) catch |err| {
            std.log.err("read error: {}", .{err});
            return .rearm;
        };
        return .rearm;
    }

    fn handleRead(self: *RpcConnection, loop: *xev.Loop, n: usize) !void {
        var arena = std.heap.ArenaAllocator.init(self.server.gpa);
        defer arena.deinit();

        // We always append this to our queue and read directly from that. This helps handling of
        // partial reads
        try self.queue.appendSlice(self.buffer[0..n]);

        while (std.mem.indexOfScalar(u8, self.queue.items, '\n')) |end| {
            defer self.queue.replaceRangeAssumeCapacity(0, end + 1, "");
            if (end == 0) continue;
            const raw_msg = self.queue.items[0..end];

            std.log.debug("wire msg={s}", .{raw_msg});
            const request = try protocol.Request.decode(arena.allocator(), self.tcp.fd, raw_msg);
            try self.handleRequest(loop, request);
        }
    }

    fn handleRequest(self: *RpcConnection, loop: *xev.Loop, request: protocol.Request) !void {
        switch (request.method) {
            .attach => |attach| try self.handleAttach(loop, attach),
            .@"list-sessions" => try self.handleListSessions(request.id.?),
            // We should never receive an exit from a client
            .exit => unreachable,
        }
    }

    fn handleListSessions(self: *RpcConnection, id: protocol.Id) !void {
        std.log.debug("client list-sessions", .{});
        var sessions = std.json.Array.init(self.server.gpa);
        defer sessions.deinit();

        for (self.server.sessions.items) |session| {
            try sessions.append(.{ .string = session.name });
        }

        const result: protocol.Response = .{
            .id = id,
            .result = .{ .array = sessions },
        };

        const file: std.fs.File = .{ .handle = self.tcp.fd };
        try result.stringify(file.writer().any());
    }

    fn handleAttach(self: *RpcConnection, loop: *xev.Loop, request: protocol.Attach) !void {
        std.log.debug(
            "client attach request: ttyname={s}, session={?s}",
            .{ request.ttyname, request.session },
        );
        const gpa = self.server.gpa;
        // Remove ourselves from any existing sessions
        if (self.session) |*session| {
            session.deinit(self.server.gpa, false);
            self.session = null;
        }

        self.session = @as(SessionConnection, undefined);
        try SessionConnection.init(&self.session.?, gpa, self, request, loop);

        std.log.debug("ttyname={s}", .{request.ttyname});
    }

    fn sendCloseMessage(self: *RpcConnection) !void {
        const msg =
            \\{"jsonrpc":"2.0","id":1,"method":"exit","params":0}
            \\
        ;
        const file: std.fs.File = .{ .handle = self.tcp.fd };
        try file.writer().writeAll(msg);
    }
};

const SessionConnection = struct {
    rpc_conn: *RpcConnection,
    ttyname: []const u8,
    session: *Session,
    vx: vaxis.Vaxis,
    tty: vaxis.Tty,
    mutex: std.Thread.Mutex,
    process_events: xev.Async,
    process_events_c: xev.Completion,
    winsize: vaxis.Winsize,
    events: std.ArrayList(vaxis.Event),

    read_thread: ?std.Thread,

    prefix_pressed: bool,

    connected: std.atomic.Value(bool),

    /// Initialize an AttachedSession. This should only be called from the main thread
    fn init(
        self: *SessionConnection,
        gpa: Allocator,
        connection: *RpcConnection,
        request: protocol.Attach,
        loop: *xev.Loop,
    ) !void {
        const ttyname = try gpa.dupe(u8, request.ttyname);
        const server = connection.server;
        const session: *Session = blk: {
            // Check if we requested a session
            if (request.session) |session_name| {
                // Find the given session
                if (server.getSession(session_name)) |session| break :blk session;
            }
            // We didn't have the session, or we didn't supply a name. Either way create a new
            // Session
            const session = try gpa.create(Session);
            try session.init(server, request.session);
            break :blk session;
        };

        // Set up vaxis
        const vx: vaxis.Vaxis = .{
            .opts = .{ .kitty_keyboard_flags = .{ .report_events = true } },
            .screen = .{},
            .screen_last = .{},
            .unicode = server.unicode,
        };

        const tty: vaxis.Tty = blk: {

            // Open our tty
            const fd = try posix.open(ttyname, .{ .ACCMODE = .RDWR }, 0);

            // Set the termios
            const termios = try vaxis.Tty.makeRaw(fd);
            break :blk .{
                .fd = fd,
                .termios = termios,
            };
        };

        const winsize: vaxis.Winsize = try vaxis.Tty.getWinsize(tty.fd);

        // Initalize ourself
        self.* = .{
            .rpc_conn = connection,
            .ttyname = ttyname,
            .session = session,
            .vx = vx,
            .tty = tty,
            .mutex = .{},
            .process_events = try xev.Async.init(),
            .process_events_c = undefined,
            .winsize = winsize,
            .events = std.ArrayList(vaxis.Event).init(gpa),
            .read_thread = null,
            .prefix_pressed = false,
            .connected = std.atomic.Value(bool).init(true),
        };

        // Schedule the process events async waiter
        self.process_events.wait(
            loop,
            &self.process_events_c,
            SessionConnection,
            self,
            SessionConnection.processEvents,
        );

        // Start a thread to read from the tty
        self.read_thread = try std.Thread.spawn(.{}, SessionConnection.ttyThread, .{self});

        // Now we finish setup of vaxis. This has to happen after setting up the async waiter and
        // the read_thread since we need our thread and async active to handle responses to these
        // calls
        try self.vx.resize(server.gpa, self.tty.anyWriter(), self.winsize);
        try self.vx.enterAltScreen(self.tty.anyWriter());
        try self.vx.setMouseMode(self.tty.anyWriter(), true);
        try self.vx.queryTerminalSend(self.tty.anyWriter());

        // Finally, add our connection to the session
        try session.attach(self);
    }

    // Only call if AttachedSession is locked
    fn deinit(self: *SessionConnection, gpa: Allocator, detach: bool) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.deinitLocked(gpa, detach);
    }

    // Only call if AttachedSession is locked. If detach is true, the session will stay running
    fn deinitLocked(self: *SessionConnection, gpa: Allocator, detach: bool) void {
        self.session.removeConnection(self);
        if (!detach) {
            self.session.maybeDeinit();
        }
        gpa.free(self.ttyname);
        self.events.deinit();

        self.vx.resetState(self.tty.anyWriter()) catch |err| {
            std.log.err("couldn't reset state: {}", .{err});
        };
        self.vx.screen.deinit(gpa);
        self.vx.screen_last.deinit(gpa);
        if (self.connected.load(.unordered)) {
            self.queueTtyThreadClose() catch |err| {
                std.log.debug("couldn't send DSR {}", .{err});
            };
        }
        self.process_events.deinit();
        if (self.read_thread) |thread| {
            std.log.debug("joining thread", .{});
            thread.join();
            std.log.debug("thread joined", .{});
            self.read_thread = null;
        }
        self.tty.deinit();
    }

    fn processEvents(
        maybe_self: ?*SessionConnection,
        _: *xev.Loop,
        _: *xev.Completion,
        result: xev.Async.WaitError!void,
    ) xev.CallbackAction {
        const self = maybe_self orelse unreachable;
        if (!self.connected.load(.unordered)) {
            // If we get here, it's because the client hung up.
            self.deinit(self.session.server.gpa, false);
            return .disarm;
        }
        result catch |err| {
            std.log.err("wait error: {}", .{err});
            return .rearm;
        };

        var run_defers: bool = true;
        self.mutex.lock();
        defer {
            if (run_defers) {
                self.mutex.unlock();
                self.events.clearRetainingCapacity();
            }
        }

        var send_to_session: bool = false;
        for (self.events.items) |event| {
            switch (event) {
                .winsize => |ws| {
                    self.vx.resize(self.rpc_conn.server.gpa, self.tty.anyWriter(), ws) catch |err| {
                        std.log.err("wait error: {}", .{err});
                    };
                    self.winsize = ws;
                },
                .key_press => |key| {
                    send_to_session = true;
                    if (self.prefix_pressed) {
                        if (key.matches('d', .{})) {
                            self.queueTtyThreadClose() catch |err| {
                                std.log.err("queueTtyThreadClose error: {}", .{err});
                            };
                            self.rpc_conn.deinitLocked(true);
                            run_defers = false;
                            return .rearm;
                        }
                    }
                    // self.prefix_pressed = false;
                    if (key.matches('b', .{ .ctrl = true })) {
                        self.prefix_pressed = true;
                    }
                },
                .cap_kitty_keyboard => {
                    std.log.info("kitty keyboard capability detected", .{});
                    self.vx.caps.kitty_keyboard = true;
                },
                .cap_kitty_graphics => {
                    if (!self.vx.caps.kitty_graphics) {
                        std.log.info("kitty graphics capability detected", .{});
                        self.vx.caps.kitty_graphics = true;
                    }
                },
                .cap_rgb => {
                    std.log.info("rgb capability detected", .{});
                    self.vx.caps.rgb = true;
                },
                .cap_unicode => {
                    std.log.info("unicode capability detected", .{});
                    self.vx.caps.unicode = .unicode;
                    self.vx.screen.width_method = .unicode;
                },
                .cap_sgr_pixels => {
                    std.log.info("pixel mouse capability detected", .{});
                    self.vx.caps.sgr_pixels = true;
                },
                .cap_color_scheme_updates => {
                    std.log.info("color_scheme_updates capability detected", .{});
                    self.vx.caps.color_scheme_updates = true;
                },
                .cap_da1 => {
                    var buf_writer = std.io.bufferedWriter(self.tty.anyWriter());
                    self.vx.enableDetectedFeatures(buf_writer.writer().any()) catch |err| {
                        std.log.err("write error: {}", .{err});
                        return .rearm;
                    };
                    buf_writer.flush() catch |err| {
                        std.log.err("flush error: {}", .{err});
                        return .rearm;
                    };
                },
                else => send_to_session = true,
            }
        }
        self.session.handleEvents(self.events.items) catch |err| {
            std.log.err("wait error: {}", .{err});
            return .rearm;
        };

        return .rearm;
    }

    fn ttyThread(self: *SessionConnection) !void {
        const tty = self.tty;
        const server = self.rpc_conn.server;

        var parser: vaxis.Parser = .{
            .grapheme_data = &server.unicode.width_data.g_data,
        };

        // initialize the read buffer
        var buf: [1024]u8 = undefined;
        var read_start: usize = 0;
        defer std.log.err("ttyThread exited", .{});
        // read loop
        read_loop: while (true) {
            const n = tty.read(buf[read_start..]) catch |err| {
                // We get to this branch if the client disconnected. In that case we need to clean
                // up in the main thread. Set the connected state and wakeup the main thread to
                // handle cleanup
                std.log.err("ttyThread read error: {}", .{err});
                self.connected.store(false, .unordered);
                try self.process_events.notify();
                return;
            };
            if (!self.connected.load(.unordered)) {
                // We get here because we told the tty thread to close. Nothing more is needed
                return;
            }

            // Lock from here on out so we can append to the event list
            self.mutex.lock();
            defer self.mutex.unlock();
            var seq_start: usize = 0;
            while (seq_start < n) {
                const result = try parser.parse(buf[seq_start..n], null);
                if (result.n == 0) {
                    // copy the read to the beginning. We don't use memcpy because
                    // this could be overlapping, and it's also rare
                    const initial_start = seq_start;
                    while (seq_start < n) : (seq_start += 1) {
                        buf[seq_start - initial_start] = buf[seq_start];
                    }
                    read_start = seq_start - initial_start + 1;
                    continue :read_loop;
                }
                read_start = 0;
                seq_start += result.n;

                const event = result.event orelse continue;
                try self.events.append(event);
            }
            if (self.events.items.len > 0) {
                // Wake up the main event to process any events we received
                try self.process_events.notify();
            }
        }
    }

    fn queueTtyThreadClose(self: *SessionConnection) !void {
        std.log.debug("queuing ttyThreadClose", .{});
        self.connected.store(false, .unordered);
        try self.vx.deviceStatusReport(self.tty.anyWriter());
    }
};

pub const Session = struct {
    name: []const u8,
    connections: std.ArrayList(*SessionConnection),
    server: *Server,

    widget: TilingWM,
    focus_handler: FocusHandler,
    mouse_handler: MouseHandler,
    wants_focus: ?vxfw.Widget,
    frame_arena: std.heap.ArenaAllocator,

    timers: std.ArrayList(vxfw.Tick),

    const adjectives = [_][]const u8{
        "arcane",
        "chilling",
        "cryptic",
        "cursed",
        "disembodied",
        "dreadful",
        "eerie",
        "ethereal",
        "foreboding",
        "ghastly",
        "ghoulish",
        "gruesome",
        "hallowed",
        "haunted",
        "invisible",
        "macabre",
        "misty",
        "morbid",
        "mysterious",
        "ominous",
        "otherworldly",
        "phantasmal",
        "shadowy",
        "sinister",
        "spectral",
        "twilight",
        "unsettling",
        "veiled",
        "wailing",
        "wispy",
    };

    const nouns = [_][]const u8{
        "apparition",
        "candle",
        "cemetery",
        "crypt",
        "curse",
        "echo",
        "entity",
        "fog",
        "ghost",
        "haunt",
        "haunting",
        "manifestation",
        "mausoleum",
        "nightmare",
        "phantom",
        "poltergeist",
        "portal",
        "possession",
        "revenant",
        "shade",
        "shadow",
        "skull",
        "specter",
        "spirit",
        "séance",
        "tomb",
        "vision",
        "whisper",
        "wraith",
    };

    fn init(self: *Session, server: *Server, maybe_name: ?[]const u8) !void {
        const name = if (maybe_name) |n|
            try server.gpa.dupe(u8, n)
        else
            try randomName(server.gpa);

        self.* = .{
            .name = name,
            .connections = std.ArrayList(*SessionConnection).init(server.gpa),
            .server = server,
            .widget = undefined,
            .timers = std.ArrayList(vxfw.Tick).init(server.gpa),
            .focus_handler = undefined,
            .wants_focus = null,
            .mouse_handler = undefined,
            .frame_arena = std.heap.ArenaAllocator.init(server.gpa),
        };
        // Initialize the widget first
        try self.widget.init(server.gpa);

        // Now we have a stable widget, initialize event handlers
        self.focus_handler.init(server.gpa, self.widget.widget());
        try self.focus_handler.path_to_focused.append(self.widget.widget());
        self.mouse_handler = MouseHandler.init(self.widget.widget());

        var ctx: vxfw.EventContext = .{
            .phase = .at_target,
            .cmds = std.ArrayList(vxfw.Command).init(self.server.gpa),
            .consume_event = false,
            .redraw = false,
            .quit = false,
        };
        defer ctx.cmds.deinit();
        try self.widget.widget().handleEvent(&ctx, .init);
        try self.handleCommands(&ctx.cmds);
        try self.server.sessions.append(self);
    }

    fn deinit(self: *Session) void {
        self.focus_handler.deinit();
        self.mouse_handler.deinit(self.server.gpa);
        self.server.gpa.free(self.name);
        self.connections.deinit();
        self.widget.deinit();
        self.timers.deinit();
        self.frame_arena.deinit();
    }

    fn checkTimers(self: *Session) !void {
        const now_ms = std.time.milliTimestamp();

        var ctx: vxfw.EventContext = .{
            .phase = .at_target,
            .cmds = std.ArrayList(vxfw.Command).init(self.server.gpa),
            .consume_event = false,
            .redraw = false,
            .quit = false,
        };
        defer ctx.cmds.deinit();

        // timers are always sorted descending
        while (self.timers.popOrNull()) |tick| {
            if (now_ms < tick.deadline_ms) {
                // re-add the timer
                try self.timers.append(tick);
                break;
            }
            try tick.widget.handleEvent(&ctx, .tick);
            try self.handleCommands(&ctx.cmds);
        }
        if (self.widget.shouldQuit()) {
            self.server.removeSession(self);
            for (self.connections.items) |conn| {
                // Deinit the connection
                conn.rpc_conn.deinit(false);
            }
            self.server.gpa.destroy(self);
            return;
        }
        if (self.wants_focus) |focus| {
            try self.focus_handler.focusWidget(&ctx, focus);
            try self.handleCommands(&ctx.cmds);
            self.wants_focus = null;
        }
        if (ctx.redraw)
            try self.draw();
    }

    fn handleCommands(self: *Session, cmds: *std.ArrayList(vxfw.Command)) !void {
        defer cmds.clearRetainingCapacity();
        for (cmds.items) |cmd| {
            switch (cmd) {
                .tick => |tick| {
                    try self.timers.append(tick);
                    std.sort.insertion(vxfw.Tick, self.timers.items, {}, vxfw.Tick.lessThan);
                },
                .set_mouse_shape => |shape| {
                    std.log.debug("setting mouse shape to {s}", .{@tagName(shape)});
                    for (self.connections.items) |conn| {
                        conn.vx.setMouseShape(shape);
                    }
                },
                .request_focus => |widget| self.wants_focus = widget,
                // .copy_to_clipboard => |content| {
                //     self.vx.copyToSystemClipboard(self.tty.anyWriter(), content, self.allocator) catch |err| {
                //         switch (err) {
                //             error.OutOfMemory => return Allocator.Error.OutOfMemory,
                //             else => std.log.err("copy error: {}", .{err}),
                //         }
                //     };
                // },
                else => {},
            }
        }
    }

    // Generates a random session name
    fn randomName(gpa: Allocator) Allocator.Error![]const u8 {
        const seed: u64 = @intCast(std.time.nanoTimestamp());
        var prng = std.rand.DefaultPrng.init(seed);
        const noun_idx = prng.random().intRangeAtMost(u8, 0, nouns.len - 1);
        const adjective_idx = prng.random().intRangeAtMost(u8, 0, adjectives.len - 1);
        const noun = nouns[noun_idx];
        const adjective = adjectives[adjective_idx];
        return std.fmt.allocPrint(gpa, "{s}-{s}", .{ adjective, noun });
    }

    fn attach(self: *Session, conn: *SessionConnection) !void {
        try self.connections.append(conn);
    }

    fn removeConnection(self: *Session, connection: *SessionConnection) void {
        for (self.connections.items, 0..) |item, i| {
            if (item == connection) {
                std.log.debug("removing connection from session {x}", .{@intFromPtr(connection)});
                _ = self.connections.swapRemove(i);
                break;
            }
        }
    }

    // Close the session if there are no connections
    fn maybeDeinit(self: *Session) void {
        if (self.connections.items.len == 0) {
            std.log.debug("closing session {s}", .{self.name});
            self.server.removeSession(self);
            self.deinit();
        }
    }

    fn handleEvents(self: *Session, events: []const vaxis.Event) !void {
        var ctx: vxfw.EventContext = .{
            .phase = .at_target,
            .cmds = std.ArrayList(vxfw.Command).init(self.server.gpa),

            .consume_event = false,
            .redraw = false,
            .quit = false,
        };
        defer ctx.cmds.deinit();
        for (events) |vaxis_event| {
            if (vaxis_event == .winsize) {
                ctx.redraw = true;
            }
            const event = vaxisToVxfwEvent(vaxis_event) orelse continue;
            if (event == .mouse) {
                try self.mouse_handler.handleMouse(self, &ctx, event.mouse);
            } else {
                try self.focus_handler.handleEvent(&ctx, event);
            }
            try self.handleCommands(&ctx.cmds);
        }
        if (self.wants_focus) |focus| {
            try self.focus_handler.focusWidget(&ctx, focus);
            try self.handleCommands(&ctx.cmds);
            self.wants_focus = null;
        }
        if (ctx.redraw) {
            try self.draw();
        }
    }

    fn draw(self: *Session) !void {
        assert(self.connections.items.len > 0);
        // We let each session keep up to 5mb allocated.
        _ = self.frame_arena.reset(.{ .retain_with_limit = 5_000_000 });
        const arena = &self.frame_arena;

        const ctx: vxfw.DrawContext = .{
            .arena = arena.allocator(),
            .min = .{ .width = 0, .height = 0 },
            .max = .{
                .width = self.connections.items[0].winsize.cols,
                .height = self.connections.items[0].winsize.rows,
            },
            .cell_size = .{ .width = 8, .height = 16 },
        };
        for (self.connections.items) |connection| {
            const vx = &connection.vx;
            const tty = connection.tty;
            const win = vx.window();
            win.clear();
            win.setCursorShape(.default);
            const surface = try self.widget.draw(ctx);
            surface.render(vx.window(), self.widget.focusedVt().widget());

            var buf = std.io.bufferedWriter(tty.anyWriter());
            try vx.render(buf.writer().any());
            try buf.flush();

            try self.focus_handler.update(surface);
            self.mouse_handler.last_frame = surface;
        }
    }
};

fn vaxisToVxfwEvent(event: vaxis.Event) ?vxfw.Event {
    switch (event) {
        .key_press => |key| return .{ .key_press = key },
        .key_release => |key| return .{ .key_release = key },
        .mouse => |mouse| return .{ .mouse = mouse },
        else => std.log.debug("unhandled event: {}", .{event}),
    }
    return null;
}

/// Maintains a tree of focusable nodes. Delivers events to the currently focused node, walking up
/// the tree until the event is handled
const FocusHandler = struct {
    arena: std.heap.ArenaAllocator,

    root: Node,
    focused: *Node,
    focused_widget: vxfw.Widget,
    path_to_focused: std.ArrayList(vxfw.Widget),

    const Node = struct {
        widget: vxfw.Widget,
        parent: ?*Node,
        children: []*Node,

        fn nextSibling(self: Node) ?*Node {
            const parent = self.parent orelse return null;
            const idx = for (0..parent.children.len) |i| {
                const node = parent.children[i];
                if (self.widget.eql(node.widget))
                    break i;
            } else unreachable;

            // Return null if last child
            if (idx == parent.children.len - 1)
                return null
            else
                return parent.children[idx + 1];
        }

        fn prevSibling(self: Node) ?*Node {
            const parent = self.parent orelse return null;
            const idx = for (0..parent.children.len) |i| {
                const node = parent.children[i];
                if (self.widget.eql(node.widget))
                    break i;
            } else unreachable;

            // Return null if first child
            if (idx == 0)
                return null
            else
                return parent.children[idx - 1];
        }

        fn lastChild(self: Node) ?*Node {
            if (self.children.len > 0)
                return self.children[self.children.len - 1]
            else
                return null;
        }

        fn firstChild(self: Node) ?*Node {
            if (self.children.len > 0)
                return self.children[0]
            else
                return null;
        }

        /// returns the next logical node in the tree
        fn nextNode(self: *Node) *Node {
            // If we have a sibling, we return it's first descendant line
            if (self.nextSibling()) |sibling| {
                var node = sibling;
                while (node.firstChild()) |child| {
                    node = child;
                }
                return node;
            }

            // If we don't have a sibling, we return our parent
            if (self.parent) |parent| return parent;

            // If we don't have a parent, we are the root and we return or first descendant
            var node = self;
            while (node.firstChild()) |child| {
                node = child;
            }
            return node;
        }

        fn prevNode(self: *Node) *Node {
            // If we have children, we return the last child descendant
            if (self.children.len > 0) {
                var node = self;
                while (node.lastChild()) |child| {
                    node = child;
                }
                return node;
            }

            // If we have siblings, we return the last descendant line of the sibling
            if (self.prevSibling()) |sibling| {
                var node = sibling;
                while (node.lastChild()) |child| {
                    node = child;
                }
                return node;
            }

            // If we don't have a sibling, we return our parent
            if (self.parent) |parent| return parent;

            // If we don't have a parent, we are the root and we return our last descendant
            var node = self;
            while (node.lastChild()) |child| {
                node = child;
            }
            return node;
        }
    };

    fn init(self: *FocusHandler, allocator: Allocator, root: vxfw.Widget) void {
        self.* = .{
            .root = .{
                .widget = root,
                .parent = null,
                .children = &.{},
            },
            .focused = &self.root,
            .focused_widget = root,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .path_to_focused = std.ArrayList(vxfw.Widget).init(allocator),
        };
    }

    fn deinit(self: *FocusHandler) void {
        self.path_to_focused.deinit();
        self.arena.deinit();
    }

    /// Update the focus list
    fn update(self: *FocusHandler, root: vxfw.Surface) Allocator.Error!void {
        _ = self.arena.reset(.retain_capacity);

        var list = std.ArrayList(*Node).init(self.arena.allocator());
        for (root.children) |child| {
            try self.findFocusableChildren(&self.root, &list, child.surface);
        }

        // Update children
        self.root.children = list.items;

        // Update path
        self.path_to_focused.clearAndFree();
        if (!self.root.widget.eql(root.widget)) {
            // Always make sure the root widget (the one we started with) is the first item, even if
            // it isn't focusable or in the path
            try self.path_to_focused.append(self.root.widget);
        }
        _ = try childHasFocus(root, &self.path_to_focused, self.focused.widget);

        // reverse path_to_focused so that it is root first
        std.mem.reverse(vxfw.Widget, self.path_to_focused.items);
    }

    /// Returns true if a child of surface is the focused widget
    fn childHasFocus(
        surface: vxfw.Surface,
        list: *std.ArrayList(vxfw.Widget),
        focused: vxfw.Widget,
    ) Allocator.Error!bool {
        // Check if we are the focused widget
        if (focused.eql(surface.widget)) {
            try list.append(surface.widget);
            return true;
        }
        for (surface.children) |child| {
            // Add child to list if it is the focused widget or one of it's own children is
            if (try childHasFocus(child.surface, list, focused)) {
                try list.append(surface.widget);
                return true;
            }
        }
        return false;
    }

    /// Walks the surface tree, adding all focusable nodes to list
    fn findFocusableChildren(
        self: *FocusHandler,
        parent: *Node,
        list: *std.ArrayList(*Node),
        surface: vxfw.Surface,
    ) Allocator.Error!void {
        if (self.root.widget.eql(surface.widget)) {
            // Never add the root_widget. We will always have this as the root
            for (surface.children) |child| {
                try self.findFocusableChildren(parent, list, child.surface);
            }
        } else if (surface.focusable) {
            // We are a focusable child of parent. Create a new node, and find our own focusable
            // children
            const node = try self.arena.allocator().create(Node);
            var child_list = std.ArrayList(*Node).init(self.arena.allocator());
            for (surface.children) |child| {
                try self.findFocusableChildren(node, &child_list, child.surface);
            }
            node.* = .{
                .widget = surface.widget,
                .parent = parent,
                .children = child_list.items,
            };
            if (self.focused_widget.eql(surface.widget)) {
                self.focused = node;
            }
            try list.append(node);
        } else {
            for (surface.children) |child| {
                try self.findFocusableChildren(parent, list, child.surface);
            }
        }
    }

    fn focusWidget(self: *FocusHandler, ctx: *vxfw.EventContext, widget: vxfw.Widget) anyerror!void {
        if (self.focused_widget.eql(widget)) return;

        ctx.phase = .at_target;
        try self.focused_widget.handleEvent(ctx, .focus_out);
        self.focused_widget = widget;
        try self.focused_widget.handleEvent(ctx, .focus_in);
    }

    fn focusNode(self: *FocusHandler, ctx: *vxfw.EventContext, node: *Node) anyerror!void {
        if (self.focused.widget.eql(node.widget)) return;

        try self.focused.widget.handleEvent(ctx, .focus_out);
        self.focused = node;
        try self.focused.widget.handleEvent(ctx, .focus_in);
    }

    /// Focuses the next focusable widget
    fn focusNext(self: *FocusHandler, ctx: *vxfw.EventContext) anyerror!void {
        return self.focusNode(ctx, self.focused.nextNode());
    }

    /// Focuses the previous focusable widget
    fn focusPrev(self: *FocusHandler, ctx: *vxfw.EventContext) anyerror!void {
        return self.focusNode(ctx, self.focused.prevNode());
    }

    fn handleEvent(self: *FocusHandler, ctx: *vxfw.EventContext, event: vxfw.Event) anyerror!void {
        const path = self.path_to_focused.items;
        if (path.len == 0) return;

        const target_idx = path.len - 1;

        // Capturing phase
        ctx.phase = .capturing;
        for (path[0..target_idx]) |widget| {
            try widget.captureEvent(ctx, event);
            if (ctx.consume_event) return;
        }

        // Target phase
        ctx.phase = .at_target;
        const target = path[target_idx];
        try target.handleEvent(ctx, event);
        if (ctx.consume_event) return;

        // Bubbling phase
        ctx.phase = .bubbling;
        var iter = std.mem.reverseIterator(path[0..target_idx]);
        while (iter.next()) |widget| {
            try widget.handleEvent(ctx, event);
            if (ctx.consume_event) return;
        }
    }
};

const MouseHandler = struct {
    last_frame: vxfw.Surface,
    last_hit_list: []vxfw.HitResult,

    fn init(root: vxfw.Widget) MouseHandler {
        return .{
            .last_frame = .{
                .size = .{ .width = 0, .height = 0 },
                .widget = root,
                .buffer = &.{},
                .children = &.{},
            },
            .last_hit_list = &.{},
        };
    }

    fn deinit(self: MouseHandler, gpa: Allocator) void {
        gpa.free(self.last_hit_list);
    }

    fn handleMouse(self: *MouseHandler, session: *Session, ctx: *vxfw.EventContext, mouse: vaxis.Mouse) anyerror!void {
        // For mouse events we store the last frame and use that for hit testing
        const last_frame = self.last_frame;

        const gpa = session.server.gpa;

        var hits = std.ArrayList(vxfw.HitResult).init(gpa);
        defer hits.deinit();
        const sub: vxfw.SubSurface = .{
            .origin = .{ .row = 0, .col = 0 },
            .surface = last_frame,
            .z_index = 0,
        };
        const mouse_point: vxfw.Point = .{
            .row = @intCast(mouse.row),
            .col = @intCast(mouse.col),
        };
        if (sub.containsPoint(mouse_point)) {
            try last_frame.hitTest(&hits, mouse_point);
        }

        // Handle mouse_enter and mouse_leave events
        {
            // We store the hit list from the last mouse event to determine mouse_enter and mouse_leave
            // events. If list a is the previous hit list, and list b is the current hit list:
            // - Widgets in a but not in b get a mouse_leave event
            // - Widgets in b but not in a get a mouse_enter event
            // - Widgets in both receive nothing
            const a = self.last_hit_list;
            const b = hits.items;

            // Find widgets in a but not b
            for (a) |a_item| {
                const a_widget = a_item.widget;
                for (b) |b_item| {
                    const b_widget = b_item.widget;
                    if (a_widget.eql(b_widget)) break;
                } else {
                    // a_item is not in b
                    try a_widget.handleEvent(ctx, .mouse_leave);
                    try session.handleCommands(&ctx.cmds);
                }
            }

            // Widgets in b but not in a
            for (b) |b_item| {
                const b_widget = b_item.widget;
                for (a) |a_item| {
                    const a_widget = a_item.widget;
                    if (b_widget.eql(a_widget)) break;
                } else {
                    // b_item is not in a.
                    try b_widget.handleEvent(ctx, .mouse_enter);
                    try session.handleCommands(&ctx.cmds);
                }
            }

            // Store a copy of this hit list for next frame
            gpa.free(self.last_hit_list);
            self.last_hit_list = try gpa.dupe(vxfw.HitResult, hits.items);
        }

        const target = hits.popOrNull() orelse return;

        // capturing phase
        ctx.phase = .capturing;
        for (hits.items) |item| {
            var m_local = mouse;
            m_local.col = item.local.col;
            m_local.row = item.local.row;
            try item.widget.captureEvent(ctx, .{ .mouse = m_local });
            try session.handleCommands(&ctx.cmds);

            if (ctx.consume_event) return;
        }

        // target phase
        ctx.phase = .at_target;
        {
            var m_local = mouse;
            m_local.col = target.local.col;
            m_local.row = target.local.row;
            try target.widget.handleEvent(ctx, .{ .mouse = m_local });
            try session.handleCommands(&ctx.cmds);

            if (ctx.consume_event) return;
        }

        // Bubbling phase
        ctx.phase = .bubbling;
        while (hits.popOrNull()) |item| {
            var m_local = mouse;
            m_local.col = item.local.col;
            m_local.row = item.local.row;
            try item.widget.handleEvent(ctx, .{ .mouse = m_local });
            try session.handleCommands(&ctx.cmds);

            if (ctx.consume_event) return;
        }
    }

    /// sends .mouse_leave to all of the widgets from the last_hit_list
    fn mouseExit(self: *MouseHandler, session: *Session, ctx: *vxfw.EventContext) anyerror!void {
        for (self.last_hit_list) |item| {
            try item.widget.handleEvent(ctx, .mouse_leave);
            try session.handleCommand(&ctx.cmds);
        }
    }
};
