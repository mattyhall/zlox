const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    left_paren,
    right_paren,
    semicolon,
    plus,

    // Literals
    string,
    number,
    identifier,

    // Keywords
    print,

    // Special
    eof,
    err,
};

pub const Token = struct {
    typ: TokenType,
    loc: ?[]const u8,
    line: usize,
};

pub const Scanner = struct {
    start: []const u8,
    current: usize,
    line: usize,
    err: ?[]const u8,

    const Self = @This();

    pub fn init(src: []const u8) Self {
        return .{
            .start = src,
            .current = 0,
            .line = 1,
            .err = null,
        };
    }

    fn isAtEnd(self: *const Self) bool {
        return self.start.len == 0 or self.current >= self.start.len;
    }

    fn makeToken(self: *const Self, typ: TokenType) Token {
        const loc = if (self.start.len != 0) self.start[0..self.current] else "";
        return .{ .typ = typ, .loc = loc, .line = self.line };
    }

    fn errorToken(self: *Self, reason: []const u8) Token {
        const loc = if (self.start.len != 0) self.start[0..self.current] else "";
        self.err = reason;
        return .{ .typ = .err, .loc = loc, .line = self.line };
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.start[self.current - 1];
    }

    fn peekNext(self: *const Self) u8 {
        if (self.start.len < 2) return 0;
        return self.start[self.current + 1];
    }

    fn peek(self: *const Self) u8 {
        return self.start[self.current];
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            if (self.isAtEnd()) return;

            _ = switch (self.peek()) {
                ' ', '\r', '\t' => self.advance(),
                else => return,
            };
        }
    }

    fn string(self: *Self) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string");

        _ = self.advance(); // Closing quote
        return self.makeToken(.string);
    }

    fn number(self: *Self) Token {
        while (!self.isAtEnd() and std.ascii.isDigit(self.peek()))
            _ = self.advance();
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek()))
                _ = self.advance();
        }

        return self.makeToken(.number);
    }

    fn identifierType(self: *const Self) TokenType {
        const s = self.start[0..self.current];
        if (std.mem.eql(u8, s, "print"))
            return .print;
        return .identifier;
    }

    fn identifier(self: *Self) Token {
        while (!self.isAtEnd() and std.ascii.isAlNum(self.peek()))
            _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    pub fn scanToken(self: *Self) Token {
        self.skipWhitespace();

        if (self.isAtEnd()) return self.makeToken(.eof);

        self.start = self.start[self.current..];
        self.current = 0;

        const c = self.advance();

        if (std.ascii.isDigit(c)) return self.number();
        if (std.ascii.isAlpha(c)) return self.identifier();

        switch (c) {
            '(' => return self.makeToken(.left_paren),
            ')' => return self.makeToken(.right_paren),
            ';' => return self.makeToken(.semicolon),
            '+' => return self.makeToken(.plus),
            '"' => return self.string(),
            else => {},
        }

        return self.errorToken("Unexpected character");
    }
};