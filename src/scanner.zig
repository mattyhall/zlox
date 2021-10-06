const std = @import("std");
const ds = @import("ds.zig");

pub const TokenType = enum {
    // Single-character tokens
    left_paren,
    right_paren,
    semicolon,
    plus,
    minus,
    slash,
    star,
    comma,
    left_brace,
    right_brace,
    dot,

    // One or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    less,
    less_equal,
    more,
    more_equal,

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

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.start[self.current] != expected) return false;
        self.current += 1;
        return true;
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
        while (!self.isAtEnd() and (self.peek() == '_' or std.ascii.isAlNum(self.peek())))
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
            '-' => return self.makeToken(.minus),
            '/' => return self.makeToken(.slash),
            '*' => return self.makeToken(.star),
            ',' => return self.makeToken(.comma),
            '{' => return self.makeToken(.left_brace),
            '}' => return self.makeToken(.right_brace),
            '.' => return self.makeToken(.dot),
            '"' => return self.string(),
            '!' => return self.makeToken(if (self.match('=')) .bang_equal else .bang),
            '=' => return self.makeToken(if (self.match('=')) .equal_equal else .equal),
            '<' => return self.makeToken(if (self.match('=')) .less_equal else .less),
            '>' => return self.makeToken(if (self.match('=')) .more_equal else .more),
            else => {},
        }

        return self.errorToken("Unexpected character");
    }
};

fn testTokenise(src: []const u8, expected: []const TokenType) !void {
    var tokens = ds.DynamicArray(TokenType).init(std.testing.allocator);
    defer tokens.deinit();
    var scanner = Scanner.init(src);
    while (true) {
        const token = scanner.scanToken();
        try tokens.append(token.typ);
        if (token.typ == .eof or token.typ == .err)
            break;
    }
    try std.testing.expectEqualSlices(TokenType, expected, tokens.items());
}

test "literals" {
    try testTokenise("1 1.2 5;", &.{ .number, .number, .number, .semicolon, .eof });
    try testTokenise("\"hello\"", &.{ .string, .eof });
    try testTokenise("\"hello;world\";", &.{ .string, .semicolon, .eof });
    try testTokenise("\"", &.{.err});
}

test "identifiers" {
    try testTokenise("helloWorld", &.{ .identifier, .eof });
    try testTokenise("hello_world", &.{ .identifier, .eof });
    try testTokenise("hi1", &.{ .identifier, .eof });
    try testTokenise("hi1_", &.{ .identifier, .eof });
    try testTokenise("_hi", &.{.err});
}

test "punctuation" {
    try testTokenise("(){};", &.{
        .left_paren,
        .right_paren,
        .left_brace,
        .right_brace,
        .semicolon,
        .eof,
    });
    try testTokenise("+-*/", &.{ .plus, .minus, .star, .slash, .eof });
    try testTokenise("< > <= >= == != ! =", &.{
        .less,
        .more,
        .less_equal,
        .more_equal,
        .equal_equal,
        .bang_equal,
        .bang,
        .equal,
        .eof,
    });
    try testTokenise("<#", &.{ .less, .err });
}

test "keyword" {
    const keywords = [_]TokenType{.print};
    for (keywords) |keyword| {
        try testTokenise(@tagName(keyword), &.{ keyword, .eof });
    }
}

test "random long strings" {
    try testTokenise("print 1+ \"hello\" 1.2;", &.{
        .print,
        .number,
        .plus,
        .string,
        .number,
        .semicolon,
        .eof,
    });
    try testTokenise("print 1 >= 2; print !2.67843", &.{
        .print,
        .number,
        .more_equal,
        .number,
        .semicolon,
        .print,
        .bang,
        .number,
        .eof,
    });
}
