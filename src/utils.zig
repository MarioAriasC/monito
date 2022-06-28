const std = @import("std");
const mem = std.mem;

pub fn strEql(strA: []const u8, strB: []const u8) bool {
    return mem.eql(u8, strA, strB);
}
