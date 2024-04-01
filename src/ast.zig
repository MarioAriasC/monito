const std = @import("std");
const print = std.debug.print;
const tokens = @import("tokens.zig");
const utils = @import("utils.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const expect = std.testing.expect;

pub const Program = struct {
    const Self = @This();
    statements: []Statement,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self.statements.len) {
            0 => try writer.print("", .{}),
            1 => try writer.print("{s}", .{self.statements[0]}),
            else => {
                for (self.statements) |statement| {
                    try writer.print("{s}", .{statement});
                }
            },
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
    hashLiteral: HashLiteral,

    pub fn token(self: Self) Token {
        switch (self) {
            inline else => |impl| return impl.token,
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token().literal;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.format(fmt, options, writer),
        }
    }

    pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
        // print("deinit-expression:{}\n", .{self});
        switch (self) {
            inline else => |impl| @constCast(&impl).deinit(allocator),
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            inline else => |impl| try impl.format(fmt, options, writer),
        }
    }

    pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
        // print("deinit-statement:{}\n", .{self});
        switch (self) {
            inline else => |impl| @constCast(&impl).deinit(allocator),
        }
    }
};

fn ptrNullableFormat(writer: anytype, comptime T: type, nullable: ?*const T, default: []const u8) !void {
    if (nullable) |not_null| {
        try writer.print("{s}", .{not_null});
    } else {
        try writer.print("{s}", .{default});
    }
}

pub fn nullableFormat(writer: anytype, comptime T: type, nullable: ?T, default: []const u8) !void {
    if (nullable) |not_null| {
        try writer.print("{s}", .{not_null});
    } else {
        try writer.print("{s}", .{default});
    }
}

fn literalInit(comptime T: type, comptime V: type, allocator: std.mem.Allocator, token: Token, value: V) T {
    var self = allocator.create(T) catch unreachable;
    self.token = token;
    self.value = value;
    return self.*;
}

pub fn literalFormat(
    comptime T: type,
    self: T,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    try writer.print(fmt, .{self.value});
}

pub const IntegerLiteral = struct {
    const Self = @This();
    token: Token,
    value: i64,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: i64) Self {
        return literalInit(Self, i64, allocator, token, value);
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try literalFormat(Self, self, "{d}", options, writer);
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .integerLiteral = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const BooleanLiteral = struct {
    const Self = @This();
    token: Token,
    value: bool,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: bool) Self {
        return literalInit(Self, bool, allocator, token, value);
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try literalFormat(Self, self, "{}", options, writer);
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .booleanLiteral = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const StringLiteral = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) Self {
        return literalInit(Self, []const u8, allocator, token, value);
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try literalFormat(Self, self, "{s}", options, writer);
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .stringLiteral = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const Identifier = struct {
    const Self = @This();
    token: Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) Self {
        return literalInit(Self, []const u8, allocator, token, value);
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try literalFormat(Self, self, "{s}", options, writer);
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .identifier = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("(", .{});
        try ptrNullableFormat(writer, Expression, self.left, "null");
        try writer.print(" {s} ", .{self.operator});
        try ptrNullableFormat(writer, Expression, self.right, "null");
        try writer.print(")", .{});
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .infixExpression = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("({s}", .{self.operator});
        try ptrNullableFormat(writer, Expression, self.right, "null");
        try writer.print(")", .{});
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .prefixExpression = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

fn nullableJoinFormat(writer: anytype, comptime T: type, nullables: ?[]?*const T, separartor: []const u8) !void {
    if (nullables) |not_nullables| {
        for (not_nullables, 1..) |nullable, i| {
            const real_sep = if (i == not_nullables.len) "" else separartor;
            try ptrNullableFormat(writer, T, nullable, "null");
            try writer.print("{s}", .{real_sep});
        }
    } else {
        try writer.print("null", .{});
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try ptrNullableFormat(writer, Expression, self.function, "null");
        try writer.print("(", .{});
        try nullableJoinFormat(writer, Expression, self.arguments, ", ");
        try writer.print(")", .{});
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn asExpression(self: Self) Expression {
        return Expression{ .indexExpression = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("(", .{});
        try ptrNullableFormat(writer, Expression, self.left, "null");
        try writer.print("[", .{});
        try ptrNullableFormat(writer, Expression, self.index, "null");
        try writer.print("])", .{});
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn asExpression(self: Self) Expression {
        return Expression{ .arrayLiteral = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("[", .{});
        try nullableJoinFormat(writer, Expression, self.elements, ", ");
        try writer.print("]", .{});
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("if ", .{});
        try ptrNullableFormat(writer, Expression, self.condition, "null");
        if (self.consequence) |consequence| {
            try writer.print(" {s}", .{consequence});
        } else {
            try writer.print(" null", .{});
        }
        try writer.print(" ", .{});
        if (self.alternative) |alternative| {
            try writer.print("{s}", .{alternative});
        }
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .ifExpression = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.token.literal});
        if (self.name.len != 0) {
            try writer.print("<{s}>", .{self.name});
        }
        try writer.print("(", .{});
        if (self.parameters) |parameters| {
            for (parameters, 1..) |parameter, i| {
                const sep = if (i == parameters.len) "" else ", ";
                try writer.print("{s}{s}", .{ parameter, sep });
            }
        }
        try writer.print(") ", .{});
        try nullableFormat(writer, BlockStatement, self.body, "");
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .functionLiteral = self };
    }

    pub fn setName(self: *Self, name: []const u8) void {
        self.name = name;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const ExpressionContext = struct {
    const Self = @This();
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        var self = allocator.create(Self) catch unreachable;
        self.allocator = allocator;
        return self.*;
    }

    fn format(self: Self, expression: *const Expression) []const u8 {
        return std.fmt.allocPrint(self.allocator, "{}", .{expression}) catch unreachable;
    }

    pub fn eql(self: Self, expression1: *const Expression, expression2: *const Expression) bool {
        const fmt1 = self.format(expression1);
        const fmt2 = self.format(expression2);
        defer self.allocator.free(fmt1);
        defer self.allocator.free(fmt2);
        return utils.strEql(fmt1, fmt2);
    }

    pub fn hash(self: Self, expression: *const Expression) u64 {
        const fmt = self.format(expression);
        defer self.allocator.free(fmt);
        return std.hash.Wyhash.hash(0, fmt);
    }
};

pub const HashLiteral = struct {
    const Self = @This();
    token: Token,
    pairs: std.hash_map.HashMap(
        *const Expression,
        *const Expression,
        ExpressionContext,
        std.hash_map.default_max_load_percentage,
    ),

    pub fn init(
        allocator: std.mem.Allocator,
        token: Token,
        pairs: std.hash_map.HashMap(
            *const Expression,
            *const Expression,
            ExpressionContext,
            std.hash_map.default_max_load_percentage,
        ),
    ) Self {
        var self = allocator.create(Self) catch unreachable;
        self.token = token;
        self.pairs = pairs;
        return self.*;
    }

    pub fn asExpression(self: Self) Expression {
        return Expression{ .hashLiteral = self };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        var iterator = self.pairs.iterator();
        try writer.print("{s}", .{"{"});
        const size = self.pairs.count();
        var i: u32 = 1;
        while (iterator.next()) |entry| {
            const separator = if (i == size) "" else ", ";
            try writer.print("{}: {}{s}", .{ entry.key_ptr.*.*, entry.value_ptr.*.*, separator });
            i = i + 1;
        }
        try writer.print("{s}", .{"}"});
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} {s} = ", .{ self.token.literal, self.name });
        try nullableFormat(writer, Expression, self.value, "");
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .letStatement = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try nullableFormat(writer, Expression, self.expression, "");
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .expressionStatement = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} ", .{self.token.literal});
        if (self.returnValue) |returnValue| {
            try writer.print("{} ", .{returnValue});
        }
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .returnStatement = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.statements != null) {
            try nullableJoinFormat(writer, Statement, self.statements, "");
        }
    }

    pub fn asStatement(self: Self) Statement {
        return Statement{ .blockStatement = self };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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

// test "hash map" {
// const test_allocator = std.testing.allocator;
// var arena = std.heap.ArenaAllocator.init(test_allocator);
// defer arena.deinit();
// const allocator = arena.allocator();
// var hash_map = std.HashMap(*const Expression, *const Expression, ExpressionContext, std.hash_map.default_max_load_percentage).initContext(allocator, ExpressionContext.init(allocator));
// std.debug.print("format =    {any}\n", .{hash_map});
// std.debug.print("type name = {s}", .{@typeName(@TypeOf(hash_map))});
// }

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
