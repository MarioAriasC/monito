const std = @import("std");
const utils = @import("utils.zig");

const Opcode = u8;
const Instructions = []Opcode;

pub const OpConstant: Opcode = 0;
pub const OpAdd: Opcode = 1;
pub const OpPop: Opcode = 2;
pub const OpSub: Opcode = 3;
pub const OpMul: Opcode = 4;
pub const OpDiv: Opcode = 5;
pub const OpTrue: Opcode = 6;
pub const OpFalse: Opcode = 7;
pub const OpEqual: Opcode = 8;
pub const OpNotEqual: Opcode = 9;
pub const OpGreaterThan: Opcode = 10;
pub const OpMinus: Opcode = 11;
pub const OpBang: Opcode = 12;
pub const OpJumpNotTruthy: Opcode = 13;
pub const OpJump: Opcode = 14;
pub const OpNull: Opcode = 15;
pub const OpGetGlobal: Opcode = 16;
pub const OpSetGlobal: Opcode = 17;
pub const OpArray: Opcode = 18;
pub const OpHash: Opcode = 19;
pub const OpIndex: Opcode = 20;
pub const OpCall: Opcode = 21;
pub const OpReturnValue: Opcode = 22;
pub const OpReturn: Opcode = 23;
pub const OpGetLocal: Opcode = 24;
pub const OpSetLocal: Opcode = 25;
pub const OpGetBuiltin: Opcode = 26;
pub const OpClosure: Opcode = 27;
pub const OpGetFree: Opcode = 28;
pub const OpCurrentClosure: Opcode = 29;

pub const Definition = struct {
    const Self = @This();
    name: []const u8,
    operands_width: []u8,

    // pub fn init(allocator: std.mem.Allocator, name: []const u8, operands_width: []u8) Self {
    //     var self = allocator.create(Self) catch unreachable;
    //     self.name = name;
    //     self.operands_width = operands_width;
    //     return self.*;
    // }
};

fn definition(name: []const u8) Definition {
    return Definition{ .name = name, .operands_width = [_]u8{} };
}

pub const defOpConstant = Definition{ .name = "OpConstant", .operands_width = [_]u8{2} };
pub const defOpAdd = definition("OpAdd");
pub const defOpPop = definition("OpPop");
pub const defOpSub = definition("OpSub");
pub const defOpMul = definition("OpMul");
pub const defOpDiv = definition("OpDiv");
pub const defOpTrue = definition("OpTrue");
pub const defOpFalse = definition("OpFalse");
pub const defOpEqual = definition("OpEqual");
pub const defOpNotEqual = definition("OpNotEqual");
pub const defOpGreaterThan = definition("OpGreaterThan");
pub const defOpMinus = definition("OpMinus");
pub const defOpBang = definition("OpBang");
pub const defOpJumpNotTruthy = Definition{ .name = "OpJumpNotTruthy", .operands_width = [_]u8{2} };
pub const defOpJump = Definition{ .name = "OpJump", .operands_width = [_]u8{2} };
pub const defOpNull = definition("OpNull");
pub const defOpGetGlobal = Definition{ .name = "OpGetGlobal", .operands_width = [_]u8{2} };
pub const defOpSetGlobal = Definition{ .name = "OpSetGlobal", .operands_width = [_]u8{2} };
pub const defOpArray = Definition{ .name = "OpArray", .operands_width = [_]u8{2} };
pub const defOpHash = Definition{ .name = "OpHash", .operands_width = [_]u8{2} };
pub const defOpIndex = definition("OpIndex");
pub const defOpCall = Definition{ .name = "OpCall", .operands_width = [_]u8{1} };
pub const defOpReturnValue = definition("OpReturnValue");
pub const defOpReturn = definition("OpReturn");
pub const defOpGetLocal = Definition{ .name = "OpGetLocal", .operands_width = [_]u8{1} };
pub const defOpSetLocal = Definition{ .name = "OpSetLocal", .operands_width = [_]u8{1} };
pub const defOpGetBuiltin = Definition{ .name = "OgGetBuiltin", .operands_width = [_]u8{1} };
pub const defOpClosure = Definition{ .name = "OpClosure", .operands_width = [_]u8{ 2, 1 } };
pub const defOpGetFree = Definition{ .name = "OpGetFree", .operands_width = [_]u8{1} };
pub const defOpCurrentClosure = definition("OpCurrentClosure");

fn lookup(op: Opcode) Definition {
    return switch (op) {
        OpConstant => defOpConstant,
        OpAdd => defOpAdd,
        OpPop => defOpPop,
        OpSub => defOpSub,
        OpMul => defOpMul,
        OpDiv => defOpDiv,
        OpTrue => defOpTrue,
        OpFalse => defOpFalse,
        OpEqual => defOpEqual,
        OpNotEqual => defOpNotEqual,
        OpGreaterThan => defOpGreaterThan,
        OpMinus => defOpMinus,
        OpBang => defOpBang,
        OpJumpNotTruthy => defOpJumpNotTruthy,
        OpJump => defOpJump,
        OpNull => defOpNull,
        OpGetGlobal => defOpGetGlobal,
        OpSetGlobal => defOpSetGlobal,
        OpArray => defOpArray,
        OpHash => defOpHash,
        OpIndex => defOpIndex,
        OpCall => defOpCall,
        OpReturnValue => defOpReturnValue,
        OpReturn => defOpReturn,
        OpGetLocal => defOpGetLocal,
        OpSetLocal => defOpSetLocal,
        OpGetBuiltin => defOpGetBuiltin,
        OpClosure => defOpClosure,
        OpGetFree => defOpGetFree,
        OpCurrentClosure => defOpCurrentClosure,
    };
}

pub fn make(op: Opcode, operands: []u8) []Opcode {
    const def = lookup(op);
    const instructions_len = utils.sum(def.operands_width) + 1;
    const instructions = [instructions_len]Opcode{};
    instructions[0] = op;
    var offset = 1;
    for (operands, 0..) |operand, i| {
        const width = def.operands_width[i];
        switch (width) {
            2 => writeChar(&instructions, offset, operand),
            1 => instructions[offset] = operand,
        }
        offset += width;
    }
    return instructions;
}

fn writeChar(instructions: *Instructions, offset: usize, i: u8) void {
    instructions[offset] = ((i >> 8) & 255);
    instructions[offset + 1]((i >> 0) & 255);
}

test "make" {
    const TestCase = utils.Tuple3(Opcode, []const u8, []const u8);
    const tests = [_]TestCase{
        TestCase{ .a = OpConstant, .b = &[_]u8{255}, .c = &[_]u8{ OpConstant, 255, 254 } },
        TestCase{ .a = OpAdd, .b = &[_]u8{}, .c = &[_]u8{OpAdd} },
        TestCase{ .a = OpGetLocal, .b = &[_]u8{255}, .c = &[_]u8{ OpGetLocal, 255 } },
    };

    for (tests) |case| {
        const instructions = make(case.a, case.b);
        std.testing.expectEqual(case.c.len, instructions.len);
        for (case.c, 0..) |byte, i| {
            std.testing.expectEqual(byte, instructions[i]);
        }
    }
}
