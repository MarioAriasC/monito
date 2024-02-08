const std = @import("std");
const print = std.debug.print;
const tokens = @import("tokens.zig");
const utils = @import("utils.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const expect = std.testing.expect;

fn joinToString(comptime T: type, values: []T, allocator: std.mem.Allocator, separartor: []const u8) []const u8 {
    var list = std.ArrayList(u8).init(allocator);
    const writer = list.writer();
    for (values, 1..) |value, i| {
        const sep = blk: {
            if (i == values.len) {
                break :blk "";
            } else {
                break :blk separartor;
            }
        };
        writer.print("{s}{s}", .{ value.toString(allocator), sep }) catch unreachable;
    }
    return list.items;
}

fn toStringInternal(statements: []Statement, allocator: std.mem.Allocator) []const u8 {
    return joinToString(Statement, statements, allocator, "");
}

pub const Program = struct {
    statements: []Statement,

    pub fn toString(self: Program, allocator: std.mem.Allocator) []const u8 {
        switch (self.statements.len) {
            0 => return "",
            1 => return self.statements[0].toString(allocator),
            else => return toStringInternal(self.statements, allocator),
        }
    }
};

pub const Expression = union(enum) {
    const Self = @This();

    infixExpression: InfixExpression,
    identifier: Identifier,
    integerLiteral: IntegerLiteral,
    booleanLiteral: BooleanLiteral,
    prefixExpression: PrefixExpression,
    callExpression: CallExpression,
    arrayLiteral: ArrayLiteral,

    pub fn token(self: Self) Token {
        switch (self) {
            inline else => |impl| return impl.token,
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token().literal;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.toStringInternal(allocator),
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
            inline else => |impl| return impl.token,
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token().literal;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            inline else => |impl| return impl.toStringInternal(allocator),
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

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .identifier = self };
    }
};

fn nullableToString(allocator: std.mem.Allocator, expression: ?*const Expression) []const u8 {
    if (expression) |exp| {
        return exp.toString(allocator);
    } else {
        return "null";
    }
}

pub const InfixExpression = struct {
    const Self = @This();
    token: Token,
    left: ?*const Expression,
    operator: []const u8,
    right: ?*const Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, left: ?*const Expression, operator: []const u8, right: ?*const Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.left = left;
        self.operator = operator;
        self.right = right;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ nullableToString(allocator, self.left), self.operator, nullableToString(allocator, self.right) }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .infixExpression = self };
    }
};

fn literalInit(comptime T: type, comptime V: type, allocator: std.mem.Allocator, token: Token, value: V) T {
    var self = allocator.create(T) catch unreachable;
    self.token = token;
    self.value = value;
    return self.*;
}

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Token,
    value: i64,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: i64) Self {
        return literalInit(Self, i64, allocator, token, value);
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .integerLiteral = self };
    }
};

pub const BooleanLiteral = struct {
    const Self = @This();
    token: Token,
    value: bool,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: bool) Self {
        return literalInit(Self, bool, allocator, token, value);
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .booleanLiteral = self };
    }
};

pub const PrefixExpression = struct {
    const Self = @This();
    token: Token,
    operator: []const u8,
    right: ?*const Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, operator: []const u8, right: ?*const Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.operator = operator;
        self.right = right;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({s}{s})", .{ self.operator, nullableToString(allocator, self.right) }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .prefixExpression = self };
    }
};

fn nullableJoinString(allocator: std.mem.Allocator, nullable_expressions: ?[]?*const Expression) []const u8 {
    if (nullable_expressions) |expressions| {
        var list = std.ArrayList(u8).init(allocator);
        const writer = list.writer();
        for (expressions, 1..) |expression, i| {
            const sep = sepBlk: {
                if (i == expressions.len) {
                    break :sepBlk "";
                } else {
                    break :sepBlk ", ";
                }
            };
            writer.print("{s}{s}", .{ nullableToString(allocator, expression), sep }) catch unreachable;
        }
        return list.items;
    } else {
        return "null";
    }
}

pub const CallExpression = struct {
    const Self = @This();
    token: Token,
    function: ?*const Expression,
    arguments: ?[]?*const Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, function: ?*const Expression, arguments: ?[]?*const Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.function = function;
        self.arguments = arguments;
        return self.*;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .callExpression = self };
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}({s})", .{ nullableToString(allocator, self.function), nullableJoinString(allocator, self.arguments) }) catch unreachable;
    }
};

pub const IndexExpression = struct {
    const Self = @This();
    token: Token,
    left: ?*const Expression,
    index: ?*const Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, left: ?*const Expression, index: ?*const Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.left = left;
        self.index = index;
        return self.*;
    }
};

pub const ArrayLiteral = struct {
    const Self = @This();
    token: Token,
    elements: ?[]?*const Expression,

    pub fn init(allocator: std.mem.Allocator, token: Token, elements: ?[]?*const Expression) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.elements = elements;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "[{s}]", .{nullableJoinString(allocator, self.elements)}) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .arrayLiteral = self };
    }
};

fn nullableToStringForStatement(allocator: std.mem.Allocator, value: ?Expression) []const u8 {
    if (value) |vl| {
        return vl.toString(allocator);
    } else {
        return "";
    }
}

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
        return std.fmt.allocPrint(allocator, "{s} {s} = {s}", .{ self.token.literal, self.name.toStringInternal(allocator), nullableToStringForStatement(allocator, self.value) }) catch unreachable;
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .letStatement = self };
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

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        return nullableToStringForStatement(allocator, self.expression);
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .expressionStatement = self };
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

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        const returnStr = blk: {
            if (self.returnValue) |returnValue| {
                break :blk returnValue.toString(allocator);
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s} {s}", .{ self.token.literal, returnStr }) catch unreachable;
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

    const statement = let_statement.asStatement();

    const let_statement_casted = statement.letStatement;
    _ = let_statement_casted;

    try expect(utils.strEql(statement.tokenLiteral(), "let"));
}

// test "extracting value from a prefix expression" {
//     const test_allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(test_allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();
//
//     const integer_literal = IntegerLiteral.init(allocator, Token.init(allocator, TokenType.INT, "5"), 5);
//     const prefix = PrefixExpression.init(allocator, Token.init(allocator, TokenType.MINUS, "-"), "-", &integer_literal.asExpression());
//
//     const statement = ExpressionStatement.init(allocator, Token.init(allocator, TokenType.MINUS, "-"), prefix.asExpression());
//
//     const expression = prefix.asExpression();
//     const prefix2 = expression.prefixExpression;
//
//     const right = prefix.right;
//
// }
