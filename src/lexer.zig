const std = @import("std");
// const ArrayList = std.ArrayList;
const tokens = @import("tokens.zig");
const Token = tokens.Token;
const TokenType = tokens.TokenType;
const utils = @import("utils.zig");
const strEql = utils.strEql;
const contains = utils.contains;

const ZERO: u8 = @intCast(0);
const WHITE_SPACES = " \t\n\r";
const VALID_IDENTFIER = "ABCDEFGHIJLKMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz";
const VALID_DIGITS = "0123456789";

fn isIdentifier(ch: u8) bool {
    // std.debug.print("isIdentifier({any})\n", .{ch});
    return contains(VALID_IDENTFIER, ch);
}

fn isDigit(ch: u8) bool {
    return contains(VALID_DIGITS, ch);
}

pub const Lexer = struct {
    input: []const u8,
    allocator: std.mem.Allocator,
    position: u32,
    readPosition: u32,
    ch: u8,
    pub fn init(allocator: std.mem.Allocator, input: []const u8) Lexer {
        var lexer = allocator.create(Lexer) catch unreachable;
        lexer.input = input;
        lexer.allocator = allocator;
        lexer.position = 0;
        lexer.readPosition = 0;
        lexer.ch = ZERO;
        lexer.readChar();
        return lexer.*;
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();
        var r: Token = undefined;
        switch (self.ch) {
            '=' => r = self.endsWithEqual(TokenType.ASSIGN, TokenType.EQ, true),
            ';' => r = self.token(TokenType.SEMICOLON),
            ':' => r = self.token(TokenType.COLON),
            '{' => r = self.token(TokenType.LBRACE),
            '}' => r = self.token(TokenType.RBRACE),
            '[' => r = self.token(TokenType.LBRACKET),
            ']' => r = self.token(TokenType.RBRACKET),
            '(' => r = self.token(TokenType.LPAREN),
            ')' => r = self.token(TokenType.RPAREN),
            ',' => r = self.token(TokenType.COMMA),
            '+' => r = self.token(TokenType.PLUS),
            '-' => r = self.token(TokenType.MINUS),
            '/' => r = self.token(TokenType.SLASH),
            '*' => r = self.token(TokenType.ASTERISK),
            '<' => r = self.token(TokenType.LT),
            '>' => r = self.token(TokenType.GT),
            '!' => r = self.endsWithEqual(TokenType.BANG, TokenType.NOT_EQ, false),
            '"' => r = Token.init(self.allocator, TokenType.STRING, self.readString()),
            ZERO => r = Token.init(self.allocator, TokenType.EOF, ""),
            else => {
                if (isIdentifier(self.ch)) {
                    var identifier = self.readIdentifier();
                    return Token.init(self.allocator, tokens.lookupIdent(identifier), identifier);
                }
                if (isDigit(self.ch)) {
                    return Token.init(self.allocator, TokenType.INT, self.readNumber());
                }
                return self.token(TokenType.ILLEGAL);
            },
        }
        self.readChar();
        return r;
    }

    fn readString(self: *Lexer) []const u8 {
        var start = self.position + 1;
        while (true) {
            self.readChar();
            if (self.ch == '"' or self.ch == ZERO) {
                break;
            }
        }
        return self.input[start..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (contains(WHITE_SPACES, self.ch)) {
            self.readChar();
        }
    }

    fn endsWithEqual(self: *Lexer, oneChar: TokenType, twoChars: TokenType, duplicateChars: bool) Token {
        if (self.peakChar() != '=') {
            return self.token(oneChar);
        }
        const currentChar = self.ch;
        self.readChar();
        var literal: []const u8 = undefined;
        if (duplicateChars) {
            literal = std.fmt.allocPrint(self.allocator, "{c}{c}", .{ currentChar, currentChar }) catch unreachable;
        } else {
            literal = std.fmt.allocPrint(self.allocator, "{c}{c}", .{ currentChar, self.ch }) catch unreachable;
        }
        return Token.init(self.allocator, twoChars, literal);
    }

    fn token(self: *Lexer, tokenType: TokenType) Token {
        // std.debug.print("self.ch {c}\n", .{self.ch});
        // return Token.initWithChar(self.allocator, tokenType, self.ch);
        // return Token.init(self.allocator, tokenType, &[1]u8{self.ch});
        return Token.init(self.allocator, tokenType, self.input[self.position .. self.position + 1]);
    }

    fn readChar(self: *Lexer) void {
        self.ch = self.peakChar();
        // std.debug.print("self.ch {any}\n", .{self.ch});
        self.position = self.readPosition;
        // std.debug.print("self.position {any}\n", .{self.position});
        self.readPosition = self.readPosition + 1;
        // std.debug.print("self.readPosition {any} \n", .{self.readPosition});
    }

    fn peakChar(self: *Lexer) u8 {
        if (self.readPosition >= self.input.len) {
            return ZERO;
        }
        return self.input[self.readPosition];
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        return self.readValue(isIdentifier);
    }

    fn readNumber(self: *Lexer) []const u8 {
        return self.readValue(isDigit);
    }

    fn readValue(self: *Lexer, predicate: *const fn (u8) bool) []const u8 {
        var currentPosition = self.position;
        while (predicate(self.ch)) {
            self.readChar();
        }
        return self.input[currentPosition..self.position];
    }
};

// TESTING
const expect = std.testing.expect;
test "validate lexer" {
    // const allocator = std.testing.allocator;
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const code =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\	x + y;
        \\}
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\	return true;
        \\} else {
        \\	return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\"foobar"
        \\"foo bar"
        \\[1,2];
        \\{"foo":"bar"}
    ;
    const allocator = arena.allocator();
    var lexer = Lexer.init(allocator, code);

    const expected = [_]Token{
        Token{ .tokenType = TokenType.LET, .literal = "let" },
        Token{ .tokenType = TokenType.IDENT, .literal = "five" },
        Token{ .tokenType = TokenType.ASSIGN, .literal = "=" },
        Token{ .tokenType = TokenType.INT, .literal = "5" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.LET, .literal = "let" },
        Token{ .tokenType = TokenType.IDENT, .literal = "ten" },
        Token{ .tokenType = TokenType.ASSIGN, .literal = "=" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.LET, .literal = "let" },
        Token{ .tokenType = TokenType.IDENT, .literal = "add" },
        Token{ .tokenType = TokenType.ASSIGN, .literal = "=" },
        Token{ .tokenType = TokenType.FUNCTION, .literal = "fn" },
        Token{ .tokenType = TokenType.LPAREN, .literal = "(" },
        Token{ .tokenType = TokenType.IDENT, .literal = "x" },
        Token{ .tokenType = TokenType.COMMA, .literal = "," },
        Token{ .tokenType = TokenType.IDENT, .literal = "y" },
        Token{ .tokenType = TokenType.RPAREN, .literal = ")" },
        Token{ .tokenType = TokenType.LBRACE, .literal = "{" },
        Token{ .tokenType = TokenType.IDENT, .literal = "x" },
        Token{ .tokenType = TokenType.PLUS, .literal = "+" },
        Token{ .tokenType = TokenType.IDENT, .literal = "y" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.RBRACE, .literal = "}" },
        Token{ .tokenType = TokenType.LET, .literal = "let" },
        Token{ .tokenType = TokenType.IDENT, .literal = "result" },
        Token{ .tokenType = TokenType.ASSIGN, .literal = "=" },
        Token{ .tokenType = TokenType.IDENT, .literal = "add" },
        Token{ .tokenType = TokenType.LPAREN, .literal = "(" },
        Token{ .tokenType = TokenType.IDENT, .literal = "five" },
        Token{ .tokenType = TokenType.COMMA, .literal = "," },
        Token{ .tokenType = TokenType.IDENT, .literal = "ten" },
        Token{ .tokenType = TokenType.RPAREN, .literal = ")" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.BANG, .literal = "!" },
        Token{ .tokenType = TokenType.MINUS, .literal = "-" },
        Token{ .tokenType = TokenType.SLASH, .literal = "/" },
        Token{ .tokenType = TokenType.ASTERISK, .literal = "*" },
        Token{ .tokenType = TokenType.INT, .literal = "5" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.INT, .literal = "5" },
        Token{ .tokenType = TokenType.LT, .literal = "<" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.GT, .literal = ">" },
        Token{ .tokenType = TokenType.INT, .literal = "5" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.IF, .literal = "if" },
        Token{ .tokenType = TokenType.LPAREN, .literal = "(" },
        Token{ .tokenType = TokenType.INT, .literal = "5" },
        Token{ .tokenType = TokenType.LT, .literal = "<" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.RPAREN, .literal = ")" },
        Token{ .tokenType = TokenType.LBRACE, .literal = "{" },
        Token{ .tokenType = TokenType.RETURN, .literal = "return" },
        Token{ .tokenType = TokenType.TRUE, .literal = "true" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.RBRACE, .literal = "}" },
        Token{ .tokenType = TokenType.ELSE, .literal = "else" },
        Token{ .tokenType = TokenType.LBRACE, .literal = "{" },
        Token{ .tokenType = TokenType.RETURN, .literal = "return" },
        Token{ .tokenType = TokenType.FALSE, .literal = "false" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.RBRACE, .literal = "}" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.EQ, .literal = "==" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.INT, .literal = "10" },
        Token{ .tokenType = TokenType.NOT_EQ, .literal = "!=" },
        Token{ .tokenType = TokenType.INT, .literal = "9" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.STRING, .literal = "foobar" },
        Token{ .tokenType = TokenType.STRING, .literal = "foo bar" },
        Token{ .tokenType = TokenType.LBRACKET, .literal = "[" },
        Token{ .tokenType = TokenType.INT, .literal = "1" },
        Token{ .tokenType = TokenType.COMMA, .literal = "," },
        Token{ .tokenType = TokenType.INT, .literal = "2" },
        Token{ .tokenType = TokenType.RBRACKET, .literal = "]" },
        Token{ .tokenType = TokenType.SEMICOLON, .literal = ";" },
        Token{ .tokenType = TokenType.LBRACE, .literal = "{" },
        Token{ .tokenType = TokenType.STRING, .literal = "foo" },
        Token{ .tokenType = TokenType.COLON, .literal = ":" },
        Token{ .tokenType = TokenType.STRING, .literal = "bar" },
        Token{ .tokenType = TokenType.RBRACE, .literal = "}" },
        Token{ .tokenType = TokenType.EOF, .literal = "" },
    };

    for (expected) |item| {
        var token = lexer.nextToken();
        // std.log.info("token {any}", .{token});
        // std.debug.print("token {any}\n", .{token});
        // std.debug.print("expected {any}\n", .{item});

        // try expect(token.tokenType == item.tokenType);
        try std.testing.expectEqual(item.tokenType, token.tokenType);
        try std.testing.expectEqualStrings(item.literal, token.literal);
        // try expect(strEql(token.literal, item.literal));
        // allocator.destroy(&token);
    }
}
