const utils = @import("utils");

const TokenType = enum { ILLEGAL, EOF, ASSIGN, EQ, NOT_EQ, IDENT, INT, PLUS, COMMA, SEMICOLON, COLON, MINUS, BANG, SLASH, ASTERISK, LT, GT, LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, FUNCTION, LET, TRUE, FALSE, IF, ELSE, RETURN, STRING };

pub fn lookupIndent(str: []const u8) TokenType {
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
    if (strEql("ELSE", str)) {
        return TokenType.ELSE;
    }
    if (strEql("RETURN", str)) {
        return TokenType.RETURN;
    }

    return TokenType.Ident;
}

const Token = struct { type: TokenType, literal: []const u8 };
