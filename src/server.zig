const std = @import("std");
const xev = @import("xev");
const vaxis = @import("vaxis");

const net = std.net;
const posix = std.posix;
const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;

const assert = std.debug.assert;

pub const Server = struct {
    gpa: Allocator,

    /// Absolute path to the socket
    sockpath: []const u8,

    /// xev server
    tcp: xev.TCP,

    accept_c: xev.Completion,

    connections: std.ArrayList(*Connection),
    sessions: std.ArrayList(*Session),

    unicode: vaxis.Unicode,

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
            .connections = std.ArrayList(*Connection).init(gpa),
            .sessions = std.ArrayList(*Session).init(gpa),
            .unicode = try vaxis.Unicode.init(gpa),
        };

        try self.tcp.bind(address);
        try self.tcp.listen(1);
        log.debug("server listening at {s}...", .{path});

        self.tcp.accept(loop, &self.accept_c, Server, self, Server.acceptCallback);
    }

    pub fn deinit(
        self: *Server,
    ) void {
        for (self.connections.items) |connection| {
            connection.deinit();
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
        const connection = self.gpa.create(Connection) catch |err| {
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
            .ttyname = null,
            .tty = null,
            .vx = null,
            .events = std.ArrayList(vaxis.Event).init(self.gpa),
            .read_thread = null,
            .mutex = .{},
            .process_events = xev.Async.init() catch |err| {
                std.log.err("error: {}", .{err});
                return .disarm;
            },
            .process_events_c = undefined,
            .connection_closed = false,
        };

        self.connections.append(connection) catch |err| {
            std.log.err("error: {}", .{err});
            return .disarm;
        };

        connection.tcp.read(
            loop,
            &connection.read_c,
            .{ .slice = &connection.buffer },
            Connection,
            connection,
            Connection.readCallback,
        );

        _ = completion; // autofix
        return .rearm;
    }

    fn removeConnection(self: *Server, connection: *Connection) void {
        for (self.connections.items, 0..) |item, i| {
            if (item == connection) {
                _ = self.connections.swapRemove(i);
                break;
            }
        }
        connection.deinit();
        self.gpa.destroy(connection);
    }

    fn getSession(self: *Server, name: []const u8) ?*Session {
        for (self.sessions.items) |session| {
            if (std.mem.eql(u8, session.name, name)) {
                return session;
            }
        }
        return null;
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

pub const Connection = struct {
    tcp: xev.TCP,
    server: *Server,
    buffer: [4096]u8,
    read_c: xev.Completion,
    queue: std.ArrayList(u8),

    session: ?*Session,
    ttyname: ?[]const u8,

    // Mutex guards access to tty, vx, and events
    mutex: std.Thread.Mutex,
    tty: ?vaxis.Tty,
    vx: ?vaxis.Vaxis,
    events: std.ArrayList(vaxis.Event),
    read_thread: ?std.Thread,
    /// Wakes up the main thread to process events
    process_events: xev.Async,
    process_events_c: xev.Completion,
    // Set to true if the read thread exits unexpectedly
    connection_closed: bool,

    const ReadError = Allocator.Error || protocol.Error;

    pub fn deinit(self: *Connection) void {
        if (self.ttyname) |ttyname| {
            self.server.gpa.free(ttyname);
        }
        if (self.session) |session| {
            session.removeConnection(self);
        }
        if (self.vx) |*vx| {
            vx.screen.deinit(self.server.gpa);
            vx.screen_last.deinit(self.server.gpa);
        }
        if (self.tty) |tty| {
            tty.deinit();
        }
        self.process_events.deinit();
        self.events.deinit();
        self.queue.deinit();
        posix.close(self.tcp.fd);
    }

    fn readCallback(
        maybe_self: ?*Connection,
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
            std.log.err("read error: {}", .{err});
            const server = self.server;
            if (err == error.EOF) {
                self.server.removeConnection(self);
            }
            if (server.connections.items.len == 0) {
                // TODO: uncomment this
                loop.stop();
            }
            return .disarm;
        };
        self.handleRead(loop, n) catch |err| {
            std.log.err("read error: {}", .{err});
            return .rearm;
        };
        return .rearm;
    }

    fn handleRead(self: *Connection, loop: *xev.Loop, n: usize) !void {
        var arena = std.heap.ArenaAllocator.init(self.server.gpa);
        defer arena.deinit();

        // We always append this to our queue and read directly from that. This helps handling of
        // partial reads
        try self.queue.appendSlice(self.buffer[0..n]);

        while (std.mem.indexOfScalar(u8, self.queue.items, '\n')) |end| {
            defer self.queue.replaceRangeAssumeCapacity(0, end + 1, "");
            if (end == 0) continue;
            const raw_msg = self.queue.items[0..end];

            const request = try protocol.Request.decode(arena.allocator(), self, raw_msg);
            std.log.err("request = {}", .{request});
            try self.handleRequest(loop, request);
        }
    }

    fn handleRequest(self: *Connection, loop: *xev.Loop, request: protocol.Request) !void {
        switch (request.method) {
            .attach => |attach| try self.handleAttach(loop, attach),
        }
    }

    fn handleAttach(self: *Connection, loop: *xev.Loop, request: protocol.Attach) !void {
        const gpa = self.server.gpa;
        // Remove ourselves from any existing sessions
        if (self.session) |session| {
            session.removeConnection(self);
            self.session = null;
        }

        if (self.ttyname == null) {
            self.ttyname = try gpa.dupe(u8, request.ttyname);
        }

        self.process_events.wait(
            loop,
            &self.process_events_c,
            Connection,
            self,
            Connection.processEvents,
        );

        // Start a thread to read from the tty
        if (self.read_thread == null) {
            self.read_thread = try std.Thread.spawn(.{}, Connection.ttyThread, .{self});
        }
        if (request.session) |session_name| blk: {
            // Find the session and return
            const session = self.server.getSession(session_name) orelse
                break :blk;
            self.session = session;
            return session.attach(self);
        }

        // Create a new session
        const session = try self.server.gpa.create(Session);
        session.* = try Session.init(self.server, request.session);
        std.log.debug("creating new session: {s}", .{session.name});
        try self.server.sessions.append(session);
        self.session = session;
        try session.attach(self);

        std.log.debug("ttyname={s}", .{request.ttyname});
    }

    fn ttyThread(self: *Connection) !void {
        // Open our tty
        const fd = try posix.open(self.ttyname.?, .{ .ACCMODE = .RDWR }, 0);

        // Set the termios
        const termios = try vaxis.Tty.makeRaw(fd);

        {
            self.mutex.lock();
            defer self.mutex.unlock();
            self.tty = .{
                .fd = fd,
                .termios = termios,
            };

            // Initialize vaxis
            self.vx = .{
                .opts = .{},
                .screen = .{},
                .screen_last = .{},
                .unicode = self.server.unicode,
            };
        }

        // get our initial winsize
        const winsize = try vaxis.Tty.getWinsize(fd);
        {
            self.mutex.lock();
            defer self.mutex.unlock();
            try self.events.append(.{ .winsize = winsize });
        }

        var parser: vaxis.Parser = .{
            .grapheme_data = &self.server.unicode.width_data.g_data,
        };

        // initialize the read buffer
        var buf: [1024]u8 = undefined;
        var read_start: usize = 0;
        defer std.log.err("goodbye thread", .{});
        // read loop
        read_loop: while (true) {
            const n = self.tty.?.read(buf[read_start..]) catch |err| {
                // TODO: clean up connection when this happens
                std.log.err("ttyThread read error: {}", .{err});
                self.connection_closed = true;
                try self.process_events.notify();
                return;
            };

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
                std.log.debug("{}", .{event});
                try self.events.append(event);
            }
            if (self.events.items.len > 0) {
                // Wake up the main event to process any events we received
                try self.process_events.notify();
            }
        }
    }

    fn processEvents(
        maybe_self: ?*Connection,
        _: *xev.Loop,
        _: *xev.Completion,
        result: xev.Async.WaitError!void,
    ) xev.CallbackAction {
        const self = maybe_self orelse unreachable;
        if (self.connection_closed) {
            self.read_thread.?.join();
            return .disarm;
        }
        result catch |err| {
            std.log.err("wait error: {}", .{err});
            return .rearm;
        };

        self.mutex.lock();
        defer self.mutex.unlock();
        defer self.events.clearRetainingCapacity();
        for (self.events.items) |event| {
            std.log.debug("event processed: {}", .{event});
        }

        return .rearm;
    }
};

pub const Session = struct {
    name: []const u8,
    connections: std.ArrayList(*Connection),
    server: *Server,

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

    fn init(server: *Server, maybe_name: ?[]const u8) !Session {
        const name = if (maybe_name) |n|
            try server.gpa.dupe(u8, n)
        else
            try randomName(server.gpa);

        return .{
            .name = name,
            .connections = std.ArrayList(*Connection).init(server.gpa),
            .server = server,
        };
    }

    fn deinit(self: *Session) void {
        self.server.gpa.free(self.name);
        self.connections.deinit();
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

    fn attach(self: *Session, conn: *Connection) !void {
        try self.connections.append(conn);
    }

    fn removeConnection(self: *Session, connection: *Connection) void {
        for (self.connections.items, 0..) |item, i| {
            if (item == connection) {
                _ = self.connections.swapRemove(i);
                return;
            }
        }
    }
};
