const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("env.zig").Environment;

pub const Object = union(enum) {
    const Self = @This();

    integer: Integer,
    boolean: Boolean,
    returnValue: ReturnValue,
    err: Error,
    nil: _Nil,
    function: Function,

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

    pub fn isError(self: Self) bool {
        switch (self) {
            .err => return true,
            else => return false,
        }
    }
};

pub const Value = union(enum) {
    int: i64,
    boolean: bool,
    string: []const u8,
};

pub const ObjectValue = union(enum) {
    const Self = @This();

    integer: Integer,
    boolean: Boolean,

    pub fn getValue(self: Self) Value {
        switch (self) {
            inline else => |impl| return impl.value,
        }
    }

    pub fn asObject(self: Self) Object {
        switch (self) {
            inline else => |impl| impl.asObject(),
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        writer.write("{s}", .{self.getValue()});
    }
};

pub const HashType = enum { INTEGER, BOOLEAN, STRING };

pub const HashKey = struct {
    const Self = @This();

    hash_type: HashType,
    value: u64,

    pub fn init(allocator: std.mem.Allocator, hash_type: HashType, value: u64) Self {
        var self = allocator.create(Self) catch unreachable;
        self.hash_type = hash_type;
        self.value = value;
        return self.*;
    }
};

pub const Hashable = union(enum) {
    const Self = @This();

    integer: Integer,

    pub fn hashType(self: Self) HashType {
        switch (self) {
            inline else => |impl| return impl.hashType,
        }
    }

    pub fn asObjectValue(self: Self) ObjectValue {
        switch (self) {
            inline else => |impl| return impl.asObjectValue(),
        }
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
};

pub const Boolean = struct {
    const Self = @This();
    value: bool,

    pub fn init(allocator: std.mem.Allocator, value: bool) Self {
        var self = allocator.create(Self) catch unreachable;
        self.value = value;
        return self.*;
    }

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
};

var TRUE: ?Object = null;
var FALSE: ?Object = null;
var NIL: ?Object = null;

pub fn booleanAsObject(allocator: std.mem.Allocator, value: bool) Object {
    if (value) {
        if (TRUE == null) {
            TRUE = Boolean.init(allocator, value).asObject();
        }
        return TRUE.?;
    } else {
        if (FALSE == null) {
            FALSE = Boolean.init(allocator, value).asObject();
        }
        return FALSE.?;
    }
}

pub fn True() Object {
    return TRUE.?;
}

pub fn False() Object {
    return FALSE.?;
}

pub fn Nil(allocator: std.mem.Allocator) Object {
    if (NIL == null) {
        NIL = _Nil.init(allocator).asObject();
    }
    return NIL.?;
}

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
        try writer.print("Error(messaeg='{s}')", .{self.message});
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
