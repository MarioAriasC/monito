const std = @import("std");
const print = std.debug.print;
const tokens = @import("tokens.zig");
const utils = @import("utils.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const expect = std.testing.expect;

pub const Program = struct { statements: []Statement };

pub const Expression = struct {
    impl: *anyopaque,
    _token: Token,
    toStringFn: *const fn (*anyopaque, std.mem.Allocator) []const u8,

    pub fn token(expression: *const Expression) Token {
        return expression._token;
    }

    pub fn tokenLiteral(expression: *const Expression) []const u8 {
        return expression.token().literal;
    }

    pub fn toString(expression: *const Expression, allocator: std.mem.Allocator) []const u8 {
        return expression.toStringFn(expression.impl, allocator);
    }
};

pub const Statement = Expression;

const StringLiteral = struct {
    const Self = @This();
    token: Token,
    value: []const u8,
    pub fn toString(self: Self) []const u8 {
        return self.value;
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.value = value;
        return self.*;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        const this: *Self = @ptrCast(@alignCast(self));
        return this.token.literal;
    }

    pub fn asExpression(self: *const Identifier) Expression {
        return Expression{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

pub const InfixExpression = struct {
    const Self = @This();
    token: Token,
    left: ?Expression,
    operator: []const u8,
    right: ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, left: ?Expression, operator: []const u8, right: ?Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.left = left;
        self.operator = operator;
        self.right = right;
        return self.*;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        return std.fmt.allocPrint(allocator, "({any} {any} {any})", .{ this.left, this.operator, this.right }) catch unreachable;
        //return this.left ++ " " ++ this.operator ++ " " ++ this.right;
    }

    pub fn asExpression(self: *const Self) Expression {
        return Expression{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Token,
    value: i64,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: i64) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.value = value;
        return self.*;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        const this: *Self = @ptrCast(@alignCast(self));
        std.debug.print("$from the toString implementation: {s}\n", .{this.token.literal});
        return this.token.literal;
    }

    pub fn asExpression(self: *const Self) Expression {
        return Expression{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

pub const LetStatement = struct {
    const Self = @This();
    token: Token,
    name: Identifier,
    value: ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, name: Identifier, value: ?Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.name = name;
        self.value = value;
        return self.*;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        const valueStr = blk: {
            if (this.value) |value| {
                break :blk value.toString(allocator);
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s} {s} {s}", .{ this.token.literal, this.name.asExpression().toString(allocator), valueStr }) catch unreachable;
    }

    pub fn asStatement(self: *const Self) Statement {
        return Statement{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();
    token: Token,
    expression: ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, expression: ?Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.expression = expression;
        return self.*;
    }

    fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        if (this.expression) |expression| {
            return expression.toString(allocator);
        } else {
            return "";
        }
    }

    pub fn asStatement(self: *const Self) Statement {
        return Statement{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

pub const ReturnStatement = struct {
    const Self = @This();
    token: Token,
    returnValue: ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, returnValue: ?Expression) Self {
        var statement = allocator.create(Self) catch unreachable;
        statement.token = token;
        statement.returnValue = returnValue;
        return statement.*;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        const returnStr = blk: {
            if (this.returnValue) |returnValue| {
                break :blk returnValue.toString(allocator);
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s} {s}", .{ this.token.literal, returnStr }) catch unreachable;
    }

    pub fn asStatement(self: *const Self) Statement {
        return Statement{ .impl = @constCast(self), ._token = self.token, .toStringFn = toString };
    }
};

test "casting back and for" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const integerLiteral = IntegerLiteral.init(allocator, Token.init(allocator, TokenType.INT, "5"), 5);
    const expression = integerLiteral.asExpression();
    const original: *IntegerLiteral = @ptrCast(@alignCast(expression.impl));
    std.debug.print("from literal :{d}\n", .{integerLiteral.value});
    std.debug.print("from original :{d}\n", .{original.value});
    std.debug.print("{s}\n", .{expression.toString(allocator)});
    try expect(integerLiteral.value == 5);
    try expect(original.value == 5);
}
