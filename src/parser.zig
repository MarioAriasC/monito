const std = @import("std");
const print = std.debug.print;
const tokens = @import("tokens.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const utils = @import("utils.zig");
const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const Expression = ast.Expression;

const Precedence = enum { LOWEST, EQUALS, LESS_GREATER, SUM, PRODUCT, PREFIX, CALL, INDEX };

fn findPrecedence(tokenType: TokenType) Precedence {
    return switch (tokenType) {
        .EQ => Precedence.EQUALS,
        .NOT_EQ => Precedence.EQUALS,
        .LT => Precedence.LESS_GREATER,
        .GT => Precedence.LESS_GREATER,
        .PLUS => Precedence.SUM,
        .MINUS => Precedence.SUM,
        .SLASH => Precedence.PRODUCT,
        .ASTERISK => Precedence.PRODUCT,
        .LPAREN => Precedence.CALL,
        .LBRACKET => Precedence.INDEX,
        else => Precedence.LOWEST,
    };
}

const PrefixParser = *const fn (*Parser) ?Expression;

const InfixParser = *const fn (*Parser, ?Expression) ?Expression;

pub const Parser = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    curToken: Token,
    peekToken: Token,
    lexer: Lexer,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, lexer: Lexer) Self {
        var parser = allocator.create(Parser) catch unreachable;
        parser.lexer = lexer;
        parser.allocator = allocator;
        parser.errors = std.ArrayList([]const u8).init(allocator);
        parser.nextToken();
        parser.nextToken();
        return parser.*;
    }

    pub fn parseProgram(self: *Self) Program {
        var statements = std.ArrayList(Statement).init(self.allocator);
        while (self.curToken.tokenType != TokenType.EOF) {
            if (self.parseStatement()) |statement| {
                statements.append(statement) catch unreachable;
            }
            self.nextToken();
        }
        return Program{ .statements = statements.items };
    }

    fn nextToken(self: *Self) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    fn parseStatement(self: *Self) ?Statement {
        if (self.curToken.tokenType == TokenType.LET) {
            return self.parseLetStatement();
        }
        if (self.curToken.tokenType == TokenType.RETURN) {
            return self.parseReturnStatement();
        }
        return self.parseExpressionStatement();
    }

    fn parseLetStatement(self: *Self) ?Statement {
        var token = self.curToken;
        if (!self.expectPeek(TokenType.IDENT)) {
            return null;
        }

        var name = ast.Identifier.init(self.allocator, self.curToken, self.curToken.literal);

        if (!self.expectPeek(TokenType.ASSIGN)) {
            return null;
        }

        self.nextToken();

        var value = self.parseExpression(Precedence.LOWEST);
        print("value from parseLetStatement {any}\n", .{value});

        // TODO add setting the name for function literral

        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return ast.LetStatement.init(self.allocator, token, name, value).asStatement();
    }

    fn parseReturnStatement(self: *Self) ?Statement {
        var token = self.curToken;
        self.nextToken();

        var returnValue = self.parseExpression(Precedence.LOWEST);
        while (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ReturnStatement.init(self.allocator, token, returnValue).asStatement();
    }

    fn parseExpressionStatement(self: *Self) ?Statement {
        var token = self.curToken;
        var expression = self.parseExpression(Precedence.LOWEST);
        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ExpressionStatement.init(self.allocator, token, expression).asStatement();
    }

    fn expectPeek(self: *Self, tokenType: TokenType) bool {
        if (self.peekTokenIs(tokenType)) {
            self.nextToken();
            return true;
        }
        self.peekError(tokenType);
        return false;
    }

    fn peekError(self: *Self, tokenType: TokenType) void {
        var e = std.fmt.allocPrint(self.allocator, "Expected next token to be {any}, got {any} instead", .{ tokenType, self.peekToken.tokenType }) catch unreachable;
        self.errors.append(e) catch unreachable;
    }

    fn noPrefixParserError(self: *Self, tokenType: TokenType) void {
        var e = std.fmt.allocPrint(self.allocator, "No prefix parser for {any} function", .{tokenType}) catch unreachable;
        self.errors.append(e) catch unreachable;
    }

    fn prefixParser(tokenType: TokenType) ?PrefixParser {
        return switch (tokenType) {
            .INT => Self.parseIntegerLiteral,
            .IDENT => Self.parseIdentifier,
            else => null,
        };
    }

    fn infixParser(tokenType: TokenType) ?InfixParser {
        return switch (tokenType) {
            .EQ => Self.parseInfixExpression,
            else => null,
        };
    }

    fn parseIdentifier(self: *Self) ?Expression {
        return ast.Identifier.init(self.allocator, self.curToken, self.curToken.literal).asExpression();
    }

    fn parseIntegerLiteral(self: *Self) ?Expression {
        const token = self.curToken;
        print("token Literal = {s}\n", .{token.literal});
        const value = std.fmt.parseInt(i64, token.literal, 10) catch {
            self.errors.append(std.fmt.allocPrint(self.allocator, "could not parse {s} as integer", .{token.literal}) catch unreachable) catch unreachable;
            return null;
        };
        print("value after parsing:{}\n", .{value});
        return ast.IntegerLiteral.init(self.allocator, token, value).asExpression();
    }

    fn parseInfixExpression(self: *Self, left: ?Expression) ?Expression {
        const token = self.curToken;
        const operator = token.literal;

        const precedence = self.curPrecedence();
        self.nextToken();
        const right = self.parseExpression(precedence);
        return ast.InfixExpression.init(self.allocator, token, left, operator, right).asExpression();
    }

    fn parseExpression(self: *Self, prec: Precedence) ?Expression {
        const prefix = Self.prefixParser(self.curToken.tokenType) orelse {
            self.noPrefixParserError(self.curToken.tokenType);
            return null;
        };

        var left = prefix(self);

        while (!self.peekTokenIs(TokenType.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            const infix = Self.infixParser(self.peekToken.tokenType) orelse {
                return left;
            };

            self.nextToken();
            left = infix(self, left);
        }

        return left;
    }

    fn peekTokenIs(self: *Self, tokenType: TokenType) bool {
        return self.peekToken.tokenType == tokenType;
    }

    fn peekPrecedence(self: Self) Precedence {
        return findPrecedence(self.peekToken.tokenType);
    }

    fn curPrecedence(self: Self) Precedence {
        return findPrecedence(self.curToken.tokenType);
    }
};

// TESTS
const expect = std.testing.expect;

fn checkParserErrors(parser: Parser) !void {
    var errors = parser.errors.items;
    if (errors.len != 0) {
        print("parser has {} errors\n", .{errors.len});
        for (errors) |e| {
            print("{s}\n", .{e});
        }
        try expect(false);
    }
}

fn createProgram(input: []const u8, allocator: std.mem.Allocator) Program {
    var lexer = Lexer.init(allocator, input);
    var parser = Parser.init(allocator, lexer);
    var program = parser.parseProgram();
    checkParserErrors(parser) catch unreachable;
    return program;
}

fn countStatements(count: u8, program: Program) !void {
    try expect(count == program.statements.len);
}

const Payload = union(enum) { int: i64, boolean: bool, string: []const u8 };

test "let statements" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const TestCase = utils.Tuple3([]const u8, []const u8, Payload);
    const expecteds = [_]TestCase{TestCase{ .a = "let x = 5;", .b = "x", .c = Payload{ .int = 5 } }};
    for (expecteds) |expected| {
        var input = expected.a;
        //print("input: {s}\n", .{input});
        //print("expected: {any}\n", .{expected});
        var program = createProgram(input, allocator);
        print("program = {any}\n", .{program});
        countStatements(1, program) catch unreachable;

        const statement = program.statements[0];
        const letStatement = testLetStatement(statement, expected.b, allocator) catch unreachable;
        const value = letStatement.value;
        testLiteralExpression(value.*, expected.c, allocator) catch unreachable;
    }
}

fn testLiteralExpression(expression: ?ast.Expression, expectedValue: Payload, allocator: std.mem.Allocator) !void {
    try switch (expectedValue) {
        .int => |int| testLongLiteral(expression, int, allocator),
        .boolean => |b| testBooleanLiteral(expression, b),
        .string => |string| testIdentifier(expression, string),
    };
}

fn testLongLiteral(expression: ?ast.Expression, int: i64, allocator: std.mem.Allocator) !void {
    const notNull = expression orelse unreachable;
    print("notNull ={any}\n", .{notNull});
    print("notNull.toString = {s}\n", .{notNull.toString(allocator)});
    const literal = notNull.integerLiteral;
    print("{d}\n", .{literal.value});
    print("{d}\n", .{int});
    try expect(literal.value == int);
}

fn testBooleanLiteral(expression: ?ast.Expression, b: bool) !void {
    _ = expression;
    _ = b;
}

fn testIdentifier(expression: ?ast.Expression, string: []const u8) !void {
    _ = expression;
    _ = string;
}

fn testLetStatement(statement: Statement, expectedIdentifier: []const u8, allocator: std.mem.Allocator) !ast.LetStatement {
    print("statement = {any}\n", .{statement});
    try expect(utils.strEql("let", statement.tokenLiteral()));
    print("token = ${any}\n", .{statement.token()});
    const letStatement = statement.letStatement;
    print("letStatement = ${any}\n", .{letStatement});
    //print("TypeOf {any}\n", .{@TypeOf(letStatement)});
    if (letStatement.value.*) |value| {
        print("value = {any}\n", .{value});
        print("letStatement.value.toString = {s}\n", .{value.toString(allocator)});
    } else {
        print("value is null\n", .{});
    }
    //const letStatement: *ast.LetStatement = @ptrCast(@alignCast(@constCast(&statement.impl)));
    print("letStatement.name.value = {s}\n", .{letStatement.name.value});
    try expect(utils.strEql(expectedIdentifier, letStatement.name.value));
    try expect(utils.strEql(expectedIdentifier, letStatement.name.asExpression().tokenLiteral()));
    return letStatement;
}
