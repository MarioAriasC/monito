const std = @import("std");
const objects = @import("objects.zig");
const Object = objects.Object;

pub const Environment = struct {
    const Self = @This();
    store: std.hash_map.StringHashMap(Object),
    outer: ?*const Environment,

    pub fn init(allocator: std.mem.Allocator) Self {
        var self = allocator.create(Self) catch unreachable;
        self.store = std.hash_map.StringHashMap(Object).init(allocator);
        return self.*;
    }

    pub fn initWithOuter(allocator: std.mem.Allocator, outer: *const Environment) Self {
        var self = allocator.create(Self) catch unreachable;
        self.store = std.hash_map.StringHashMap(Object).init(allocator);
        self.outer = outer;
        return self.*;
    }

    pub fn set(self: Self, name: []const u8, value: Object) void {
        self.store.put(name, value);
    }

    pub fn put(self: Self, name: []const u8, value: Object) Object {
        return self.store.fetchPut(name, value);
    }

    pub fn get(self: Self, name: []const u8) ?Object {
        const obj = self.store.get(name);
        if (obj == null and self.outer != null) {
            return self.store.get(name);
        }
        return obj;
    }
};
