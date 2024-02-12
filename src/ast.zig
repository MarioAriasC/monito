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
    indexExpression: IndexExpression,
    ifExpression: IfExpression,
    functionLiteral: FunctionLiteral,
    stringLiteral: StringLiteral,

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
            inline else => |impl| return impl.toString(allocator),
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();
    letStatement: LetStatement,
    expressionStatement: ExpressionStatement,
    returnStatement: ReturnStatement,
    blockStatement: BlockStatement,

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
            inline else => |impl| return impl.toString(allocator),
        }
    }
};

fn ptrNullableToString(allocator: std.mem.Allocator, comptime T: type, nullable: ?*const T, default: []const u8) []const u8 {
    if (nullable) |not_null| {
        return not_null.toString(allocator);
    } else {
        return default;
    }
}

fn nullableToString(allocator: std.mem.Allocator, comptime T: type, nullable: ?T, default: []const u8) []const u8 {
    if (nullable) |not_null| {
        return not_null.toString(allocator);
    } else {
        return default;
    }
}

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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.token.literal;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .booleanLiteral = self };
    }
};

pub const StringLiteral = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) Self {
        return literalInit(Self, []const u8, allocator, token, value);
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.value;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .stringLiteral = self };
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) Self {
        return literalInit(Self, []const u8, allocator, token, value);
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        _ = allocator;
        return self.value;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .identifier = self };
    }
};

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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ ptrNullableToString(allocator, Expression, self.left, "null"), self.operator, ptrNullableToString(allocator, Expression, self.right, "null") }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .infixExpression = self };
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({s}{s})", .{ self.operator, ptrNullableToString(allocator, Expression, self.right, "null") }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .prefixExpression = self };
    }
};

fn nullableJoinString(allocator: std.mem.Allocator, comptime T: type, nullables: ?[]?*const T, separator: []const u8) []const u8 {
    if (nullables) |not_nullables| {
        var list = std.ArrayList(u8).init(allocator);
        const writer = list.writer();
        for (not_nullables, 1..) |nullable, i| {
            const real_sep = sepBlk: {
                if (i == not_nullables.len) {
                    break :sepBlk "";
                } else {
                    break :sepBlk separator;
                }
            };
            writer.print("{s}{s}", .{ ptrNullableToString(allocator, T, nullable, "null"), real_sep }) catch unreachable;
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}({s})", .{ ptrNullableToString(allocator, Expression, self.function, "null"), nullableJoinString(allocator, Expression, self.arguments, ", ") }) catch unreachable;
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "({s}[{s}])", .{ ptrNullableToString(allocator, Expression, self.left, "null"), ptrNullableToString(allocator, Expression, self.index, "null") }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .indexExpression = self };
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "[{s}]", .{nullableJoinString(allocator, Expression, self.elements, ", ")}) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .arrayLiteral = self };
    }
};

pub const IfExpression = struct {
    const Self = @This();
    token: Token,
    condition: ?*const Expression,
    consequence: ?BlockStatement,
    alternative: ?BlockStatement,

    pub fn init(allocator: std.mem.Allocator, token: Token, condition: ?*const Expression, consequence: ?BlockStatement, alternative: ?BlockStatement) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.condition = condition;
        self.consequence = consequence;
        self.alternative = alternative;
        return self.*;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        const alter_str = blk: {
            if (self.alternative) |alternative| {
                break :blk std.fmt.allocPrint(allocator, "else {s}", .{alternative.toString(allocator)}) catch unreachable;
            } else {
                break :blk "";
            }
        };
        const consequence_str = blk: {
            if (self.consequence) |consequence| {
                break :blk consequence.toString(allocator);
            } else {
                break :blk "null";
            }
        };
        return std.fmt.allocPrint(allocator, "if {s} {s} {s}", .{ ptrNullableToString(allocator, Expression, self.condition, "null"), consequence_str, alter_str }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .ifExpression = self };
    }
};

pub const FunctionLiteral = struct {
    const Self = @This();
    token: Token,
    parameters: ?[]*const Identifier,
    body: ?BlockStatement,
    name: []const u8,

    var another: []const u8 = "";

    pub fn init(allocator: std.mem.Allocator, token: Token, parameters: ?[]*const Identifier, body: ?BlockStatement, name: []const u8) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.parameters = parameters;
        self.body = body;
        self.name = name;
        return self.*;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        const token_literal = self.token.literal;
        const name = if (self.name.len != 0) std.fmt.allocPrint(allocator, "<{s}>", .{self.name}) catch unreachable else "";
        const parameters = blk: {
            if (self.parameters) |params| {
                break :blk joinToString(*const Identifier, params, allocator, ", ");
            } else {
                break :blk "";
            }
        };
        return std.fmt.allocPrint(allocator, "{s}{s}({s}) {s}", .{ token_literal, name, parameters, nullableToString(allocator, BlockStatement, self.body, "") }) catch unreachable;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .functionLiteral = self };
    }

    pub fn setName(self: *Self, name: []const u8) void {
        self.name = name;
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s} {s} = {s}", .{ self.token.literal, self.name.toString(allocator), nullableToString(allocator, Expression, self.value, "") }) catch unreachable;
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        return nullableToString(allocator, Expression, self.expression, "");
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

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
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

pub const BlockStatement = struct {
    const Self = @This();
    token: Token,
    statements: ?[]?*const Statement,

    pub fn init(allocator: std.mem.Allocator, token: Token, statements: ?[]?*const Statement) Self {
        var statement = allocator.create(Self) catch unreachable;
        statement.token = token;
        statement.statements = statements;
        return statement.*;
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) []const u8 {
        if (self.statements == null) {
            return nullableJoinString(allocator, Statement, self.statements, "");
        } else {
            return "";
        }
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .blockStatement = self };
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
