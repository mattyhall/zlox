const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");

const Chunk = vm.Chunk;
const OpCode = vm.OpCode;
const Token = scan.Token;

const Precedence = enum {
    none,
    assignment, // =
    or_, // or
    and_, //and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * /
    unary, // ! -
    call, // . ()
    primary,
};

pub const Parser = struct {
    current: Token,
    previous: Token,
    scanner: scan.Scanner,
    had_error: bool,
    panic_mode: bool,
    chunk: *Chunk,

    const Self = @This();

    const ParseRule = struct {
        prefix: ?fn (self: *Self) anyerror!void,
        infix: ?fn (self: *Self) anyerror!void,
        precedence: Precedence,
    };

    // zig-fmt: off
    const rules = [_]ParseRule{
        .{ .prefix = grouping, .infix = null,   .precedence = .none },     //left_paren,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //right_paren,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //semicolon,
        .{ .prefix = null,     .infix = binary, .precedence = .term },     //plus,
        .{ .prefix = unary,    .infix = binary, .precedence = .term },     //minus,
        .{ .prefix = null,     .infix = binary, .precedence = .factor },   //slash,
        .{ .prefix = null,     .infix = binary, .precedence = .factor },   //star,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //comma,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //left_brace,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //right_brace,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //dot,
        .{ .prefix = unary,    .infix = null,   .precedence = .none },     //bang,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //bang_equal,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //equal_equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //less,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //less_equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //more,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //more_equal,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //string,
        .{ .prefix = number,   .infix = null,   .precedence = .none },     //number,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //identifier,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //and_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //class,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //else_,
        .{ .prefix = literal,  .infix = null,   .precedence = .none },     //false_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //for_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //fun,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //if_,
        .{ .prefix = literal,  .infix = null,   .precedence = .none },     //nil,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //or_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //print,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //return_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //super,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //this,
        .{ .prefix = literal,  .infix = null,   .precedence = .none },     //true_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //var_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //while_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //eof,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //err,
    };
    // zig-fmt: on

    pub fn init(src: []const u8) Self {
        return .{
            .scanner = scan.Scanner.init(src),
            .previous = undefined,
            .current = undefined,
            .had_error = false,
            .panic_mode = false,
            .chunk = undefined,
        };
    }

    fn errorAt(self: *Self, tok: *const Token, msg: []const u8) !void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        const stderr = std.io.getStdErr().writer();
        try stderr.print("[line {}] Error", .{tok.line});

        if (tok.typ == .eof) {
            try stderr.print(" at end", .{});
        } else if (tok.typ == .err) {
            // Nothing
        } else {
            try stderr.print(" at {s}", .{tok.loc});
        }

        try stderr.print(": {s}", .{msg});
        self.had_error = true;
    }

    fn err(self: *Self, msg: []const u8) !void {
        try self.errorAt(&self.previous, msg);
    }

    fn errorAtCurrent(self: *Self, msg: []const u8) !void {
        try self.errorAt(&self.current, msg);
    }

    fn consume(self: *Self, typ: scan.TokenType, msg: []const u8) !void {
        if (self.current.typ == typ) {
            try self.advance();
            return;
        }
        try self.errorAtCurrent(msg);
    }

    fn emit(self: *Self, bytes: []const u8) !void {
        for (bytes) |b| {
            try self.chunk.*.writeChunk(b, self.previous.line);
        }
    }

    fn advance(self: *Self) !void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.typ != .err) break;

            try self.errorAtCurrent(self.current.loc orelse "");
        }
    }

    fn emitReturn(self: *Self) !void {
        try self.emit(&.{@enumToInt(OpCode.ret)});
    }

    fn unary(self: *Self) !void {
        const typ = self.previous.typ;
        try self.parsePrecedence(.unary);
        switch (typ) {
            .minus => try self.emit(&.{@enumToInt(OpCode.negate)}),
            .bang => try self.emit(&.{@enumToInt(OpCode.not)}),
            else => unreachable,
        }
    }

    fn literal(self: *Self) !void {
        switch (self.previous.typ) {
            .true_ => try self.emit(&.{@enumToInt(OpCode.true_)}),
            .false_ => try self.emit(&.{@enumToInt(OpCode.false_)}),
            .nil => try self.emit(&.{@enumToInt(OpCode.nil)}),
            else => unreachable,
        }
    }

    fn number(self: *Self) !void {
        const num = try std.fmt.parseFloat(f32, self.previous.loc orelse unreachable);
        try self.emit(&.{ @enumToInt(OpCode.constant), try self.chunk.*.addConstant(.{ .number = num }) });
    }

    fn binary(self: *Self) !void {
        const op = self.previous.typ;
        const rule = rules[@enumToInt(op)];
        try self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));
        switch (op) {
            .plus => try self.emit(&.{@enumToInt(OpCode.add)}),
            .minus => try self.emit(&.{@enumToInt(OpCode.subtract)}),
            .star => try self.emit(&.{@enumToInt(OpCode.multiply)}),
            .slash => try self.emit(&.{@enumToInt(OpCode.divide)}),
            .equal_equal => try self.emit(&.{@enumToInt(OpCode.equal)}),
            .bang_equal => try self.emit(&.{@enumToInt(OpCode.not_equal)}),
            .more => try self.emit(&.{@enumToInt(OpCode.greater)}),
            .more_equal => try self.emit(&.{@enumToInt(OpCode.greater_equal)}),
            .less => try self.emit(&.{@enumToInt(OpCode.less)}),
            .less_equal => try self.emit(&.{@enumToInt(OpCode.less_equal)}),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const rule = rules[@enumToInt(self.previous.typ)].prefix;
        if (rule == null) {
            try self.err("Expect expression");
            return;
        }
        try rule.?(self);

        while (@enumToInt(precedence) <= @enumToInt(rules[@enumToInt(self.current.typ)].precedence)) {
            try self.advance();
            const infix_rule = rules[@enumToInt(self.previous.typ)].infix;
            try infix_rule.?(self);
        }
    }

    fn parseUnary(self: *Self) !void {
        try self.parsePrecedence(.unary);
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after expression");
    }

    pub fn compile(self: *Self, chunk: *Chunk) !bool {
        self.chunk = chunk;
        try self.advance();
        try self.expression();
        try self.consume(.eof, "Expect end of expression");
        try self.emitReturn();
        return self.had_error;
    }
};
