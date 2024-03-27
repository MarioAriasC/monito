const std = @import("std");
const print = std.debug.print;
const objects = @import("objects.zig");
const Object = objects.Object;

pub const Environment = struct {
    const Self = @This();
    store: std.hash_map.StringHashMap(Object),
    outer: ?*const Environment,

    pub fn init(allocator: std.mem.Allocator) Self {
        var self = allocator.create(Self) catch unreachable;
        self.store = std.hash_map.StringHashMap(Object).init(allocator);
        self.outer = null;
        return self.*;
    }

    pub fn initWithOuter(allocator: std.mem.Allocator, outer: *const Environment) Self {
        var self = allocator.create(Self) catch unreachable;
        self.store = std.hash_map.StringHashMap(Object).init(allocator);
        self.outer = outer;
        return self.*;
    }

    pub fn set(self: *Self, name: []const u8, value: Object) void {
        self.store.put(name, value) catch unreachable;
    }

    pub fn put(self: *Self, name: []const u8, value: Object) Object {
        self.store.put(name, value) catch unreachable;
        return self.store.get(name).?;
    }

    pub fn get(self: Self, name: []const u8) ?Object {
        const obj = self.store.get(name);
        if (obj == null) {
            if (self.outer) |outer| {
                return outer.get(name);
            }
        }
        return obj;
    }
};
