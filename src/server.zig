const std = @import("std");
const xev = @import("xev");

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

    const ReadError = Allocator.Error || protocol.Error;

    pub fn deinit(self: *Connection) void {
        if (self.ttyname) |ttyname| {
            self.server.gpa.free(ttyname);
        }
        if (self.session) |session| {
            session.removeConnection(self);
        }
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
                loop.stop();
            }
            return .disarm;
        };
        self.handleRead(n) catch |err| {
            std.log.err("read error: {}", .{err});
            return .rearm;
        };
        return .rearm;
    }

    fn handleRead(self: *Connection, n: usize) ReadError!void {
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
            try self.handleRequest(request);
        }
    }

    fn handleRequest(self: *Connection, request: protocol.Request) !void {
        switch (request.method) {
            .attach => |attach| {
                const gpa = self.server.gpa;
                if (self.session) |session| {
                    session.removeConnection(self);
                }
                if (self.ttyname == null) {
                    self.ttyname = try gpa.dupe(u8, attach.ttyname);
                }
                if (attach.session) |session_name| blk: {
                    const session = self.server.getSession(session_name) orelse
                        break :blk;
                    return session.attach(self);
                    // Find the session and return
                }

                // Create a new session
                const session = try self.server.gpa.create(Session);
                session.* = try Session.init(self.server, attach.session);
                std.log.debug("creating new session: {s}", .{session.name});
                try self.server.sessions.append(session);
                try session.attach(self);

                std.log.debug("ttyname={s}", .{attach.ttyname});
            },
        }
    }
};

pub const Session = struct {
    name: []const u8,
    connections: std.ArrayList(*Connection),
    server: *Server,

    const adjectives = [_][]const u8{
        "eerie",
        "ethereal",
        "haunted",
        "shadowy",
        "spectral",
        "ghoulish",
        "ominous",
        "phantasmal",
        "sinister",
        "chilling",
        "otherworldly",
        "foreboding",
        "cursed",
        "wispy",
        "macabre",
        "invisible",
        "veiled",
        "cryptic",
        "unsettling",
        "wailing",
        "mysterious",
        "gruesome",
        "dreadful",
        "arcane",
        "misty",
        "hallowed",
        "morbid",
        "twilight",
        "ghastly",
        "disembodied",
    };

    const nouns = [_][]const u8{
        "apparition",
        "phantom",
        "spirit",
        "wraith",
        "poltergeist",
        "haunting",
        "shade",
        "specter",
        "entity",
        "ghost",
        "spook",
        "revenant",
        "manifestation",
        "possession",
        "séance",
        "curse",
        "cemetery",
        "tomb",
        "crypt",
        "mausoleum",
        "shadow",
        "echo",
        "vision",
        "nightmare",
        "haunt",
        "portal",
        "skull",
        "candle",
        "whisper",
        "fog",
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
