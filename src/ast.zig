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
    booleanLiteral: BooleanLiteral,
    prefixExpression: PrefixExpression,

    pub fn token(self: Self) Token {
        switch (self) {
            // .identifier => |identifier| return identifier.token,
            // .infixExpression => |expression| return expression.token,
            // .integerLiteral => |literal| return literal.token,
            // .booleanLiteral => |literal| return literal.token,
            // .prefixExpression => |expression| return expression.token,
            inline else => |impl| return impl.token,
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
            .booleanLiteral => |literal| return literal.toStringInternal(),
            .prefixExpression => |expression| return expression.toStringInternal(allocator),
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

    pub fn toStringInternal(self: Self) []const u8 {
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

    pub fn toStringInternal(self: Self) []const u8 {
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
        // _ = allocator;
        // return PrefixExpression{ .token = token, .operator = operator, .right = right };
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.operator = operator;
        self.right = right;
        return self.*;
    }

    pub fn toStringInternal(self: Self, allocator: std.mem.Allocator) []const u8 {
        const rightStr = blk: {
            if (self.right) |right| {
                break :blk right.toString(allocator);
            } else {
                break :blk "null";
            }
        };

        return std.fmt.allocPrint(allocator, "({s}{s})", .{ self.operator, rightStr });
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .prefixExpression = self };
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
        if (self.expression) |expression| {
            return expression.toString(allocator);
        } else {
            return "";
        }
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

test "extracting value from a prefix expression" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const integer_literal = IntegerLiteral.init(allocator, Token.init(allocator, TokenType.INT, "5"), 5);
    const prefix = PrefixExpression.init(allocator, Token.init(allocator, TokenType.MINUS, "-"), "-", &integer_literal.asExpression());

    const statement = ExpressionStatement.init(allocator, Token.init(allocator, TokenType.MINUS, "-"), prefix.asExpression());

    const expression = prefix.asExpression();
    const prefix2 = expression.prefixExpression;
    std.debug.print("{any}\n", .{prefix2});
    std.debug.print("{any}\n", .{prefix2.right});
    const right = prefix.right;
    if (right) |r| {
        print("from pointer {any}\n", .{r.*});
    }

    print("{any}\n", .{statement});
    print("{any}\n", .{statement.expression});
}
