const std = @import("std");
const print = std.debug.print;
const tokens = @import("tokens.zig");
const utils = @import("utils.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const expect = std.testing.expect;

pub const Program = struct { statements: []Statement };

pub const Expression = union(enum) {
    const Self = @This();

    infixExpression: InfixExpression,
    identifier: Identifier,
    integerLiteral: IntegerLiteral,

    pub fn token(self: Self) Token {
        switch (self) {
            .identifier => |identifier| return identifier.token,
            .infixExpression => |expression| return expression.token,
            .integerLiteral => |literal| return literal.token,
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token().literal;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            .identifier => |identifier| return identifier.toStringInternal(),
            .infixExpression => |expression| return expression.toStringInternal(allocator),
            .integerLiteral => |literal| return literal.toStringInternal(),
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();
    letStatement: LetStatement,
    expressionStatement: ExpressionStatement,
    returnStatement: ReturnStatement,

    pub fn token(self: Self) Token {
        switch (self) {
            .letStatement => |statement| return statement.token,
            .expressionStatement => |statement| return statement.token,
            .returnStatement => |statement| return statement.token,
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token().literal;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            .letStatement => |statement| return statement.toStringInternal(allocator),
            .expressionStatement => |statement| return statement.toStringInternal(allocator),
            .returnStatement => |statement| return statement.toStringInternal(allocator),
        }
    }
};

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

    pub fn toStringInternal(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        const this: *Self = @ptrCast(@alignCast(self));
        return this.toStringInternal();
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .identifier = self };
    }
};

pub const InfixExpression = struct {
    const Self = @This();
    token: Token,
    left: *const ?Expression,
    operator: []const u8,
    right: *const ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, left: ?Expression, operator: []const u8, right: ?Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.left = &left;
        self.operator = operator;
        self.right = &right;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({any} {any} {any})", .{ self.left, self.operator, self.right }) catch unreachable;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        return this.toStringInternal(allocator);
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .infixExpression = self };
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

    pub fn toStringInternal(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        const this: *Self = @ptrCast(@alignCast(self));
        std.debug.print("$from the toString implementation: {s}\n", .{this.token.literal});
        return this.toStringInternal();
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .integerLiteral = self };
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

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        const valueStr = blk: {
            if (self.value) |value| {
                break :blk value.toString(allocator);
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s} {s} = {s}", .{ self.token.literal, self.name.toStringInternal(), valueStr }) catch unreachable;
    }
    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        return this.toStringInternal(allocator);
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .letStatement = self };
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();
    token: Token,
    expression: *const ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, expression: ?Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.expression = &expression;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        if (self.expression.*) |expression| {
            return expression.toString(allocator);
        } else {
            return "";
        }
    }

    fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        return this.toStringInternal(allocator);
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .expressionStatement = self };
    }
};

pub const ReturnStatement = struct {
    const Self = @This();
    token: Token,
    returnValue: *const ?Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, returnValue: ?Expression) Self {
        var statement = allocator.create(Self) catch unreachable;
        statement.token = token;
        statement.returnValue = &returnValue;
        return statement.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        const returnStr = blk: {
            if (self.returnValue.*) |returnValue| {
                break :blk returnValue.toString(allocator);
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s} {s}", .{ self.token.literal, returnStr }) catch unreachable;
    }

    pub fn toString(self: *anyopaque, allocator: std.mem.Allocator) []const u8 {
        const this: *Self = @ptrCast(@alignCast(self));
        return this.toStringInternal(allocator);
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .returnStatement = self };
    }
};

test "casting back and for with enums" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const integer_literal = IntegerLiteral.init(allocator, Token.init(allocator, TokenType.INT, "5"), 5);
    const expression = integer_literal.asExpression();
    const original = expression.integerLiteral;
    print("from literal :{d}\n", .{integer_literal.value});
    print("from original :{d}\n", .{original.value});
    print("{s}\n", .{expression.toString(allocator)});
    try expect(integer_literal.value == 5);
    try expect(original.value == 5);
}

test "casting back and for with nested enums" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const integer_literal = IntegerLiteral.init(allocator, Token.init(allocator, TokenType.INT, "5"), 5);
    const identifier = Identifier.init(allocator, Token.init(allocator, TokenType.IDENT, "x"), "x");
    const let_statement = LetStatement.init(allocator, Token.init(allocator, TokenType.LET, "let"), identifier, integer_literal.asExpression());
    print("let_statement :{any}\n", .{let_statement});
    print("let_statement: {s}\n", .{let_statement.toStringInternal(allocator)});
    const statement = let_statement.asStatement();
    print("statement:: {any}\n", .{statement});
    print("statement:: {s}\n", .{statement.toString(allocator)});
    const let_statement_casted = statement.letStatement;
    print("casted: {any}\n", .{let_statement_casted});
    print("casted: {s}\n", .{let_statement_casted.toStringInternal(allocator)});
    print("value: {any}\n", .{let_statement_casted.value});
    if (let_statement_casted.value) |value| {
        print("value: {any}\n", .{value});
    } else {
        print("value is null\n", .{});
    }
}
