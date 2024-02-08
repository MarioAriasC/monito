const std = @import("std");
const mem = std.mem;

pub fn strEql(strA: []const u8, strB: []const u8) bool {
    return mem.eql(u8, strA, strB);
}

// TODO Check how to do it generic enough, if not then move to skipWhiteSpace function
pub fn contains(str: []const u8, ch: u8) bool {
    for (str) |i| {
        if (i == ch) {
            return true;
        }
    }
    return false;
}

pub fn Tuple2(comptime A: type, comptime B: type) type {
    return struct { a: A, b: B };
}

pub fn Tuple3(comptime A: type, comptime B: type, comptime C: type) type {
    return struct { a: A, b: B, c: C };
}

pub fn Tuple4(comptime A: type, comptime B: type, comptime C: type, comptime D: type) type {
    return struct { a: A, b: B, c: C, d: D };
}
