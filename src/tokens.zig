const utils = @import("utils.zig");
const std = @import("std");
const strEql = utils.strEql;

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    ASSIGN,
    EQ,
    NOT_EQ,
    IDENT,
    INT,
    PLUS,
    COMMA,
    SEMICOLON,
    COLON,
    MINUS,
    BANG,
    SLASH,
    ASTERISK,
    LT,
    GT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    STRING,
};

pub fn lookupIdent(str: []const u8) TokenType {
    if (strEql("fn", str)) {
        return TokenType.FUNCTION;
    }
    if (strEql("let", str)) {
        return TokenType.LET;
    }
    if (strEql("true", str)) {
        return TokenType.TRUE;
    }
    if (strEql("false", str)) {
        return TokenType.FALSE;
    }
    if (strEql("if", str)) {
        return TokenType.IF;
    }
    if (strEql("else", str)) {
        return TokenType.ELSE;
    }
    if (strEql("return", str)) {
        return TokenType.RETURN;
    }

    return TokenType.IDENT;
}

pub const Token = struct {
    tokenType: TokenType,
    literal: []const u8,

    pub fn init(allocator: std.mem.Allocator, tokenType: TokenType, literal: []const u8) Token {
        var token = allocator.create(Token) catch unreachable;
        token.tokenType = tokenType;
        token.literal = literal;
        return token.*;
    }

    pub fn initWithChar(allocator: std.mem.Allocator, tokenType: TokenType, literal: u8) Token {
        var str = std.fmt.allocPrint(allocator, "{c}", .{literal}) catch unreachable;
        return Token.init(allocator, tokenType, str);
    }
};
