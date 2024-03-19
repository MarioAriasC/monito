const std = @import("std");

pub const Object = union(enum) {
    const Self = @This();

    integer: Integer,
    returnValue: ReturnValue,
    err: Error,

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
};

pub const Value = union(enum) {
    int: i64,
    boolean: bool,
    string: []const u8,
};

pub const ObjectValue = union(enum) {
    const Self = @This();

    integer: Integer,

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
};

pub const ReturnValue = struct {
    const Self = @This();
    value: *const Object,

    pub fn init(allocator: std.mem.Allocator, value: *const Object) Self {
        var self = allocator.create(Self) catch unreachable;
        self.value = value;
        return self.*;
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

    pub fn asObject(self: Self) Object {
        return Object{ .err = self };
    }
};
