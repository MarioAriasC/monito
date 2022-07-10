const std = @import("std");
const mem = std.mem;

pub fn strEql(strA: []const u8, strB: []const u8) bool {
    return mem.eql(u8, strA, strB);
}

//TODO Check how to do it generic enough, if not then move to skipWhiteSpace function
pub fn contains(str: []const u8, ch: u8) bool {
    for (str) |i| {
        if (i == ch) {
            return true;
        }
    }
    return false;
}
