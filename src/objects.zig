const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("env.zig").Environment;
const utils = @import("utils.zig");

pub const Object = union(enum) {
    const Self = @This();

    integer: Integer,
    boolean: Boolean,
    returnValue: ReturnValue,
    err: Error,
    nil: _Nil,
    function: Function,
    string: String,
    hash: Hash,
    builtin: BuiltinFunction,

    pub fn inspect(self: Self, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| impl.inspect(allocator),
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.format(fmt, options, writer),
        }
    }

    pub fn typeDesc(self: Self) []const u8 {
        switch (self) {
            .integer => return "Integer",
            .boolean => return "Boolean",
            .returnValue => return "ReturnValue",
            .err => return "Error",
            .nil => return "Nil",
            .function => return "Function",
            .string => return "String",
            .hash => return "Hash",
            .builtin => return "Builtin Function",
        }
    }

    pub fn isHashable(self: Self) bool {
        switch (self) {
            .integer => return true,
            .boolean => return true,
            .string => return true,
            else => return false,
        }
    }

    pub fn hashKey(self: Self, allocator: std.mem.Allocator) ?HashKey {
        switch (self) {
            .integer => |impl| return impl.hashKey(allocator),
            .boolean => |impl| return impl.hashKey(allocator),
            .string => |impl| return impl.hashKey(allocator),
            else => return null,
        }
    }

    pub fn isError(self: Self) bool {
        switch (self) {
            .err => return true,
            else => return false,
        }
    }
};

pub const HashType = enum { INTEGER, BOOLEAN, STRING };

pub const HashKey = struct {
    const Self = @This();

    hash_type: HashType,
    str: []const u8,

    pub fn init(allocator: std.mem.Allocator, hash_type: HashType, str: []const u8) Self {
        var self = allocator.create(Self) catch unreachable;
        self.hash_type = hash_type;
        self.str = str;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("HashKey(hash_type={}, str={s})", .{ self.hash_type, self.str });
    }
};

pub const Integer = struct {
    const Self = @This();
    value: i64,

    pub fn init(allocator: std.mem.Allocator, value: i64) Self {
        var self = allocator.create(Self) catch unreachable;
        self.value = value;
        return self.*;
    }

    pub fn asObject(self: Self) Object {
        return Object{ .integer = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Integer(value={d})", .{self.value});
    }

    pub fn hashKey(self: Self, allocator: std.mem.Allocator) HashKey {
        return HashKey.init(allocator, HashType.INTEGER, std.fmt.allocPrint(allocator, "{d}", .{self.value}) catch unreachable);
    }
};

pub const Boolean = struct {
    const Self = @This();
    value: bool,

    pub fn asObject(self: Self) Object {
        return Object{ .boolean = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Boolean(value={})", .{self.value});
    }

    pub fn hashKey(self: Self, allocator: std.mem.Allocator) HashKey {
        return HashKey.init(allocator, HashType.BOOLEAN, std.fmt.allocPrint(allocator, "{}", .{self.value}) catch unreachable);
    }
};

const _true = Boolean{ .value = true };
pub const TRUE = _true.asObject();

const _false = Boolean{ .value = false };
pub const FALSE = _false.asObject();

const _nil = _Nil{};
pub const NIL = _nil.asObject();

pub fn booleanAsObject(value: bool) Object {
    if (value) {
        return TRUE;
    } else {
        return FALSE;
    }
}

pub const String = struct {
    const Self = @This();
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, value: []const u8) Self {
        var self = allocator.create(Self) catch unreachable;
        self.value = value;
        return self.*;
    }

    pub fn asObject(self: Self) Object {
        return Object{ .string = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("String(value={s})", .{self.value});
    }

    pub fn hashKey(self: Self, allocator: std.mem.Allocator) HashKey {
        return HashKey.init(allocator, HashType.STRING, std.fmt.allocPrint(allocator, "\"{s}\"", .{self.value}) catch unreachable);
    }
};

pub const HashKeyContext = struct {
    const Self = @This();
    pub fn eql(self: Self, hash_key1: HashKey, hash_key2: HashKey) bool {
        _ = self;
        if (hash_key1.hash_type == hash_key2.hash_type) {
            return utils.strEql(hash_key1.str, hash_key2.str);
        } else {
            return false;
        }
    }

    pub fn hash(self: Self, hash_key: HashKey) u64 {
        _ = self;
        return std.hash.Wyhash.hash(0, hash_key.str);
    }
};

pub const HashPair = struct {
    key: Object,
    value: Object,
    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("HashPair(key={}, value={})", .{ self.key, self.value });
    }
};

pub const Hash = struct {
    const Self = @This();
    pairs: std.hash_map.HashMap(
        HashKey,
        HashPair,
        HashKeyContext,
        std.hash_map.default_max_load_percentage,
    ),

    pub fn init(
        allocator: std.mem.Allocator,
        pairs: std.hash_map.HashMap(
            HashKey,
            HashPair,
            HashKeyContext,
            std.hash_map.default_max_load_percentage,
        ),
    ) Self {
        var self = allocator.create(Self) catch unreachable;
        self.pairs = pairs;
        return self.*;
    }

    pub fn asObject(self: Self) Object {
        return Object{ .hash = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        var iterator = self.pairs.iterator();
        try writer.print("{s}", .{"{"});
        const size = self.pairs.count();
        var i: u32 = 1;
        while (iterator.next()) |entry| {
            const separator = if (i == size) "" else ", ";
            try writer.print("{}: {}{s}", .{ entry.key_ptr.*, entry.value_ptr.*, separator });
            i = i + 1;
        }
        try writer.print("{s}", .{"}"});
    }
};

pub const ReturnValue = struct {
    const Self = @This();
    value: *const Object,

    pub fn init(allocator: std.mem.Allocator, value: *const Object) Self {
        var self = allocator.create(Self) catch unreachable;
        self.value = value;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("ReturnValue(value={})", .{self.value});
    }

    pub fn asObject(self: Self) Object {
        return Object{ .returnValue = self };
    }
};

pub const Error = struct {
    const Self = @This();
    message: []const u8,

    pub fn init(allocator: std.mem.Allocator, message: []const u8) Self {
        var self = allocator.create(Self) catch unreachable;
        self.message = message;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("Error(message='{s}')", .{self.message});
    }

    pub fn asObject(self: Self) Object {
        return Object{ .err = self };
    }
};

const _Nil = struct {
    const Self = @This();
    pub fn init(allocator: std.mem.Allocator) Self {
        var self = allocator.create(Self) catch unreachable;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        _ = self;
        try writer.print("nil", .{});
    }

    pub fn asObject(self: Self) Object {
        return Object{ .nil = self };
    }
};

pub const Function = struct {
    const Self = @This();
    parameters: ?[]*const ast.Identifier,
    body: ?ast.BlockStatement,
    env: *Environment,

    pub fn init(allocator: std.mem.Allocator, parameters: ?[]*const ast.Identifier, body: ?ast.BlockStatement, env: *Environment) Self {
        var self = allocator.create(Self) catch unreachable;
        self.parameters = parameters;
        self.body = body;
        self.env = env;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("fn(", .{});
        if (self.parameters) |parameters| {
            for (parameters, 1..) |parameter, i| {
                const sep = if (i == parameters.len) "" else ", ";
                try writer.print("{s}{s}", .{ parameter, sep });
            }
        }
        try writer.print(") {s}\n\t", .{"{"});
        try ast.nullableFormat(writer, ast.BlockStatement, self.body, "");
        try writer.print("\n{s}", .{"}"});
    }

    pub fn asObject(self: Self) Object {
        return Object{ .function = self };
    }
};

pub const BuiltinFunction = struct {
    const Self = @This();
    name: []const u8,
    function: *const fn (std.mem.Allocator, []?Object) ?Object,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, function: *const fn (std.mem.Allocator, []?Object) ?Object) Self {
        var self = allocator.create(Self) catch unreachable;
        self.name = name;
        self.function = function;
        return self.*;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} -> Builtin Function", .{self.name});
    }

    pub fn asObject(self: Self) Object {
        return Object{ .builtin = self };
    }
};

var builts: ?std.hash_map.StringHashMap(BuiltinFunction) = null;

fn argSizeCheck(
    allocator: std.mem.Allocator,
    expected_size: u64,
    args: []?Object,
    body: *const fn (std.mem.Allocator, []?Object) ?Object,
) ?Object {
    const length = args.len;
    if (length == expected_size) {
        return Error.init(allocator, std.fmt.allocPrint(allocator, "wrong number of arguments, got={d}, want={d}", .{ length, expected_size }) catch unreachable).asObject();
    } else {
        return body(allocator, args);
    }
}

fn bodyLen(allocator: std.mem.Allocator, args: []?Object) ?Object {
    const opt_arg = args[0];
    if (opt_arg) |arg| {
        switch (arg) {
            .string => |string| return Integer.init(allocator, @intCast(string.value.len)).asObject(),
            else => return Error.init(allocator, std.fmt.allocPrint(allocator, "argumment to `len` not supported, got {s}", .{arg.typeDesc()}) catch unreachable).asObject(),
        }
    } else {
        return Error.init(allocator, "argument to `len` not supported, got null").asObject();
    }
}

fn builtinLen(allocator: std.mem.Allocator, args: []?Object) ?Object {
    return argSizeCheck(allocator, 1, args, bodyLen);
}

pub fn builtins(allocator: std.mem.Allocator) std.hash_map.StringHashMap(BuiltinFunction) {
    if (builts) |b| {
        return b;
    } else {
        builts = std.hash_map.StringHashMap(BuiltinFunction).init(allocator);
        std.debug.print("builts:{?}\n", .{builts});
        const len_name = "len";
        builts.?.put(len_name, BuiltinFunction.init(allocator, len_name, builtinLen)) catch unreachable;
        std.debug.print("builts:{?}\n", .{builts});
        return builts.?;
    }
}
