const std = @import("std");
const server = @import("server.zig");

const json = std.json;

const Allocator = std.mem.Allocator;

const assert = std.debug.assert;

pub const Error = error{
    /// The request could not be parsed
    ParseError,
    /// The request was not a valid JSON-RPC request
    InvalidRequest,
    /// Method does not exist or is not available
    MethodNotFound,
    /// Params invalid
    InvalidParams,
    /// Internal JSON-RPC error
    InternalError,

    // All other errors are server defined. Values are -32000 to -32099
};

pub const MethodEnum = enum {
    attach,
};

/// Protocol requests. JSON-RPC 2.0
pub const Method = union(MethodEnum) {
    attach: Attach,
};

pub const Id = union(enum) {
    string: []const u8,
    integer: i64,

    fn fromValue(maybe_value: ?json.Value) ?Id {
        const value = maybe_value orelse return null;
        switch (value) {
            .string => |v| return .{ .string = v },
            .integer => |v| return .{ .integer = v },
            .null => return null,
            else => unreachable,
        }
    }
};

pub const Request = struct {
    method: Method,
    id: ?Id,

    pub fn isNotification(self: Request) bool {
        return self.id == null;
    }

    pub fn decode(arena: std.mem.Allocator, conn: *server.Connection, slice: []const u8) Error!Request {
        const value = std.json.parseFromSliceLeaky(
            json.Value,
            arena,
            slice,
            .{ .allocate = .alloc_always },
        ) catch {
            errorResponse(arena, conn, error.ParseError, "bad json", null);
            return error.ParseError;
        };

        assert(value == .object);
        const object = value.object;

        // Get the id
        const id = Id.fromValue(object.get("id"));

        // Check jsonrpc version
        const version = object.get("jsonrpc") orelse {
            errorResponse(arena, conn, error.InvalidRequest, "missing jsonrpc field", id);
            return error.InvalidRequest;
        };
        assert(version == .string);
        assert(std.mem.eql(u8, version.string, "2.0"));

        // Get the method
        const method = object.get("method") orelse {
            errorResponse(arena, conn, error.InvalidRequest, "missing method field", id);
            return error.InvalidRequest;
        };
        assert(method == .string);

        const method_type: MethodEnum = std.meta.stringToEnum(MethodEnum, method.string) orelse {
            const msg = std.fmt.allocPrint(arena, "method '{s}' not found", .{method.string}) catch
                return error.MethodNotFound;
            errorResponse(arena, conn, error.MethodNotFound, msg, id);
            return error.MethodNotFound;
        };

        const params = object.get("params") orelse {
            errorResponse(arena, conn, error.InvalidParams, "missing params field", id);
            return error.InvalidParams;
        };

        return switch (method_type) {
            .attach => .{
                .id = id,
                .method = .{ .attach = try Attach.fromValue(arena, conn, params, id) },
            },
        };
    }

    pub fn stringify(self: Request, writer: std.io.AnyWriter) !void {
        var buf = std.io.bufferedWriter(writer);
        const bw = buf.writer();
        if (self.id) |id| {
            switch (id) {
                .integer => |v| {
                    try json.stringify(.{
                        .jsonrpc = "2.0",
                        .method = @tagName(self.method),
                        .params = switch (self.method) {
                            inline else => |method| method,
                        },
                        .id = v,
                    }, .{}, bw);
                },
                .string => |v| {
                    try json.stringify(.{
                        .jsonrpc = "2.0",
                        .method = @tagName(self.method),
                        .params = switch (self.method) {
                            inline else => |method| method,
                        },
                        .id = v,
                    }, .{}, bw);
                },
            }
        } else {
            try json.stringify(.{
                .jsonrpc = "2.0",
                .method = @tagName(self.method),
                .params = switch (self.method) {
                    inline else => |method| method,
                },
            }, .{}, bw);
        }

        try bw.writeByte('\n');
        try buf.flush();
    }
};

pub const Attach = struct {
    /// Path to the tty, eg /dev/pts/2
    ttyname: []const u8,
    /// Name of the session to attach to. Creates a new session if null
    session: ?[]const u8 = null,

    pub fn fromValue(arena: Allocator, conn: *server.Connection, value: json.Value, id: ?Id) Error!Attach {
        if (value != .object) return error.InvalidParams;
        const ttyname = value.object.get("ttyname") orelse {
            errorResponse(arena, conn, error.InvalidParams, "missing ttyname field", id);
            return error.InvalidParams;
        };
        if (ttyname != .string) {
            errorResponse(arena, conn, error.InvalidParams, "ttyname must be a string", id);
            return error.InvalidParams;
        }
        const session = value.object.get("session") orelse
            return .{
            .ttyname = ttyname.string,
            .session = null,
        };
        switch (session) {
            .null => return .{
                .ttyname = ttyname.string,
                .session = null,
            },
            .string => return .{
                .ttyname = ttyname.string,
                .session = session.string,
            },
            else => {
                errorResponse(arena, conn, error.InvalidParams, "session must be a string or null", id);
                return error.InvalidParams;
            },
        }
    }
};

pub fn errorResponse(
    gpa: Allocator,
    conn: *const server.Connection,
    err: Error,
    maybe_msg: ?[]const u8,
    maybe_id: ?Id,
) void {
    const err_num: i64 = switch (err) {
        error.ParseError => -32700,
        error.InvalidRequest => -32600,
        error.MethodNotFound => -32601,
        error.InvalidParams => -32601,
        error.InternalError => -32603,
    };

    var error_object = json.ObjectMap.init(gpa);
    defer error_object.deinit();

    error_object.put("code", .{ .integer = err_num }) catch return;
    if (maybe_msg) |msg| {
        error_object.put("message", .{ .string = msg }) catch return;
    }

    var object = json.ObjectMap.init(gpa);
    defer object.deinit();

    object.put("jsonrpc", .{ .string = "2.0" }) catch return;
    object.put("error", .{ .object = error_object }) catch return;
    if (maybe_id) |id| {
        switch (id) {
            .string => object.put("id", .{ .string = id.string }) catch return,
            .integer => object.put("id", .{ .integer = id.integer }) catch return,
        }
    } else {
        object.put("id", .null) catch return;
    }

    const file: std.fs.File = .{ .handle = conn.tcp.fd };

    const msg = json.stringifyAlloc(gpa, json.Value{ .object = object }, .{}) catch return;
    defer gpa.free(msg);
    file.writeAll(msg) catch return;
}
