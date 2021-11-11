const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const ds = @import("ds.zig");
const memory = @import("memory.zig");

const Chunk = vm.Chunk;
const OpCode = vm.OpCode;
const Token = scan.Token;

const LOCAL_COUNT: usize = 256;

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

fn identifiersEql(a: *const Token, b: *const Token) bool {
    return a.loc.?.len == b.loc.?.len and std.mem.eql(u8, a.loc.?, b.loc.?);
}

pub const Local = struct {
    name: Token,
    depth: isize,
    captured: bool,
};

pub const Locals = struct {
    locals: [LOCAL_COUNT]Local,
    count: usize,
    depth: isize,

    const Self = @This();

    fn init() Self {
        return .{ .locals = undefined, .count = 0, .depth = 0 };
    }
};

pub const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const Jmp = struct {
    chunk: *Chunk,
    start: usize,

    const Self = @This();

    fn set(self: *Self) void {
        var code = self.chunk.code;
        const diff = @intCast(u16, code.count - self.start - 2);
        code.data[self.start] = @intCast(u8, diff >> 8);
        code.data[self.start + 1] = @intCast(u8, diff & 0xff);
    }
};

pub const FunctionType = enum { function, script, method };

pub const Parser = struct {
    allocator: *memory.ObjectAllocator,

    current: Token,
    previous: Token,
    scanner: scan.Scanner,

    had_error: bool,
    panic_mode: bool,

    locals: Locals,
    upvalues: [256]Upvalue,

    function: *memory.Function,
    function_type: FunctionType,

    tmp_stack: ds.Stack,

    enclosing: ?*Self,

    const Self = @This();

    const ParseRule = struct {
        prefix: ?fn (self: *Self, can_assign: bool) anyerror!void,
        infix: ?fn (self: *Self, can_assign: bool) anyerror!void,
        precedence: Precedence,
    };

    // zig fmt: off
    const rules = [_]ParseRule{
        .{ .prefix = grouping, .infix = call,   .precedence = .call },     //left_paren,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //right_paren,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //semicolon,
        .{ .prefix = null,     .infix = binary, .precedence = .term },     //plus,
        .{ .prefix = unary,    .infix = binary, .precedence = .term },     //minus,
        .{ .prefix = null,     .infix = binary, .precedence = .factor },   //slash,
        .{ .prefix = null,     .infix = binary, .precedence = .factor },   //star,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //comma,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //left_brace,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //right_brace,
        .{ .prefix = null,     .infix = dot,    .precedence = .call },     //dot,
        .{ .prefix = unary,    .infix = null,   .precedence = .none },     //bang,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //bang_equal,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //equal_equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //less,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //less_equal,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //more,
        .{ .prefix = null,     .infix = binary, .precedence = .equality }, //more_equal,
        .{ .prefix = string,   .infix = null,   .precedence = .none },     //string,
        .{ .prefix = number,   .infix = null,   .precedence = .none },     //number,
        .{ .prefix = variable, .infix = null,   .precedence = .none },     //identifier,
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
        .{ .prefix = this,     .infix = null,   .precedence = .none },     //this,
        .{ .prefix = literal,  .infix = null,   .precedence = .none },     //true_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //var_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //while_,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //eof,
        .{ .prefix = null,     .infix = null,   .precedence = .none },     //err,
    };
    // zig fmt: on

    pub fn init(allocator: *memory.ObjectAllocator, src: []const u8, function_type: FunctionType) !Self {
        var self: Self = .{
            .allocator = allocator,
            .scanner = scan.Scanner.init(src),
            .previous = undefined,
            .current = undefined,
            .had_error = false,
            .panic_mode = false,
            .locals = Locals.init(),
            .function = undefined,
            .function_type = function_type,
            .enclosing = null,
            .upvalues = undefined,
            .tmp_stack = undefined,
        };

        self.tmp_stack.reset();
        self.function = try self.allocator.newFunction();

        var local = &self.locals.locals[0];
        local.depth = 0;
        if (function_type != .function) {
            local.name.loc = "this";
        } else {
            local.name.loc = "";
        }
        local.captured = false;
        self.locals.count += 1;

        return self;
    }

    pub fn inherit(other: *Self, function_type: FunctionType) !Self {
        var self: Self = .{
            .allocator = other.allocator,
            .scanner = other.scanner,
            .previous = other.previous,
            .current = other.current,
            .had_error = false,
            .panic_mode = false,
            .locals = Locals.init(),
            .function = undefined,
            .function_type = function_type,
            .enclosing = other,
            .upvalues = undefined,
            .tmp_stack = undefined,
        };

        self.tmp_stack.reset();
        self.function = try self.allocator.newFunction();

        self.allocator.compiler = &self;

        if (self.function_type != .script)
            self.function.name = (try self.allocator.allocString(self.previous.loc orelse "")).toString();

        var local = &self.locals.locals[0];
        local.depth = 0;
        if (function_type != .function) {
            local.name.loc = "this";
        } else {
            local.name.loc = "";
        }
        self.locals.count += 1;

        return self;
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.function.chunk;
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

    fn check(self: *Self, typ: scan.TokenType) bool {
        return self.current.typ == typ;
    }

    fn match(self: *Self, typ: scan.TokenType) !bool {
        if (!self.check(typ)) return false;
        try self.advance();
        return true;
    }

    fn emit(self: *Self, bytes: []const u8) !void {
        try self.currentChunk().incLine(self.previous.line);
        for (bytes) |b| {
            try self.currentChunk().writeChunk(b);
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

    fn unary(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const typ = self.previous.typ;
        try self.parsePrecedence(.unary);
        switch (typ) {
            .minus => try self.emit(&.{@enumToInt(OpCode.negate)}),
            .bang => try self.emit(&.{@enumToInt(OpCode.not)}),
            else => unreachable,
        }
    }

    fn literal(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        switch (self.previous.typ) {
            .true_ => try self.emit(&.{@enumToInt(OpCode.true_)}),
            .false_ => try self.emit(&.{@enumToInt(OpCode.false_)}),
            .nil => try self.emit(&.{@enumToInt(OpCode.nil)}),
            else => unreachable,
        }
    }

    fn string(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const loc = self.previous.loc orelse unreachable;
        const obj = try self.allocator.allocString(loc[1 .. loc.len - 1]);
        try self.emit(&.{ @enumToInt(OpCode.constant), try self.currentChunk().*.addConstant(&self.tmp_stack, .{ .object = obj }) });
    }

    fn number(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const num = try std.fmt.parseFloat(f32, self.previous.loc orelse unreachable);
        try self.emit(&.{ @enumToInt(OpCode.constant), try self.currentChunk().*.addConstant(&self.tmp_stack, .{ .number = num }) });
    }

    fn identifierConstant(self: *Self, token: *const Token) !u8 {
        const obj = try self.allocator.allocString(token.loc.?);
        return try self.currentChunk().*.addConstant(&self.tmp_stack, .{ .object = obj });
    }

    fn resolveLocal(self: *Self, name: *const Token) !?u8 {
        if (self.locals.count == 0) return null;

        var i = @intCast(isize, self.locals.count - 1);
        while (i >= 0) : (i -= 1) {
            const local = &self.locals.locals[@intCast(usize, i)];
            if (identifiersEql(name, &local.name)) {
                if (local.depth == -1) {
                    try self.err("Can't read local variable in its own initialiser");
                }
                return @intCast(u8, i);
            }
        }
        return null;
    }

    fn addUpvalue(self: *Self, index: u8, is_local: bool) !u8 {
        const upvalue_count = self.function.upvalue_count;

        var i: u8 = 0;
        while (i < upvalue_count) : (i += 1) {
            const upval = &self.upvalues[i];
            if (upval.index == index and upval.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == 255) {
            try self.err("Too many closure variables in function");
            return 0;
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.function.upvalue_count += 1;
        return self.function.upvalue_count - 1;
    }

    fn resolveUpvalue(self: *Self, name: *const Token) anyerror!?u8 {
        if (self.enclosing == null) return null;

        if (try self.enclosing.?.resolveLocal(name)) |local| {
            self.enclosing.?.locals.locals[local].captured = true;
            return try self.addUpvalue(local, true);
        }

        if (try self.enclosing.?.resolveUpvalue(name)) |upval| {
            return try self.addUpvalue(upval, false);
        }

        return null;
    }

    fn namedVariable(self: *Self, name: *const Token, can_assign: bool) !void {
        var get: OpCode = undefined;
        var set: OpCode = undefined;

        var arg = try self.resolveLocal(name);
        if (arg != null) {
            get = .get_local;
            set = .set_local;
        } else {
            arg = try self.resolveUpvalue(name);
            if (arg != null) {
                get = .get_upvalue;
                set = .set_upvalue;
            } else {
                get = .get_global;
                set = .set_global;
                arg = try self.identifierConstant(name);
            }
        }

        if (can_assign and try self.match(.equal)) {
            try self.expression();
            try self.emit(&.{ @enumToInt(set), arg.? });
        } else {
            try self.emit(&.{ @enumToInt(get), arg.? });
        }
    }

    fn variable(self: *Self, can_assign: bool) !void {
        try self.namedVariable(&self.previous, can_assign);
    }

    fn binary(self: *Self, can_assign: bool) !void {
        _ = can_assign;
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

    fn dot(self: *Self, can_assign: bool) !void {
        try self.consume(.identifier, "Expected property name after '.'");
        const name = try self.identifierConstant(&self.previous);

        if (can_assign and (try self.match(.equal))) {
            try self.expression();
            try self.emit(&.{ @enumToInt(OpCode.set_property), name });
        } else {
            try self.emit(&.{ @enumToInt(OpCode.get_property), name });
        }
    }

    fn this(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        try self.variable(false);
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) !void {
        try self.advance();
        const rule = rules[@enumToInt(self.previous.typ)].prefix;
        if (rule == null) {
            try self.err("Expect expression");
            return;
        }

        const can_assign = @enumToInt(precedence) <= @enumToInt(Precedence.assignment);
        try rule.?(self, can_assign);

        while (@enumToInt(precedence) <= @enumToInt(rules[@enumToInt(self.current.typ)].precedence)) {
            try self.advance();
            const infix_rule = rules[@enumToInt(self.previous.typ)].infix;
            try infix_rule.?(self, can_assign);
        }

        if (can_assign and try self.match(.equal))
            try self.err("Invalid assignment target");
    }

    fn parseUnary(self: *Self) !void {
        try self.parsePrecedence(.unary);
    }

    fn expression(self: *Self) !void {
        try self.parsePrecedence(.assignment);
    }

    fn grouping(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        try self.expression();
        try self.consume(.right_paren, "Expect ')' after expression");
    }

    fn argumentList(self: *Self) !u8 {
        var arg_count: u8 = 0;
        if (!self.check(.right_paren)) {
            while (true) {
                try self.expression();
                if (arg_count == 255) {
                    try self.err("Can't have more than 255 arguments");
                }
                arg_count += 1;
                if (!(try self.match(.comma)))
                    break;
            }
        }
        try self.consume(.right_paren, "Expect ')' after arguments");
        return arg_count;
    }

    fn call(self: *Self, can_assign: bool) !void {
        _ = can_assign;
        const arg_count = try self.argumentList();
        try self.emit(&.{ @enumToInt(OpCode.call), arg_count });
    }

    fn printStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after value");
        try self.emit(&.{@enumToInt(OpCode.print)});
    }

    fn returnStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after value");
        try self.emit(&.{@enumToInt(OpCode.ret)});
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after expression");
        try self.emit(&.{@enumToInt(OpCode.pop)});
    }

    fn block(self: *Self) anyerror!void {
        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.declaration();
        }

        try self.consume(.right_brace, "Expect '}' after block");
    }

    fn beginScope(self: *Self) void {
        self.locals.depth += 1;
    }

    fn endScope(self: *Self) !void {
        self.locals.depth -= 1;

        while (self.locals.count > 0 and self.locals.locals[self.locals.count - 1].depth > self.locals.depth) {
            if (self.locals.locals[self.locals.count - 1].captured) {
                try self.emit(&.{@enumToInt(OpCode.close_upvalue)});
            } else {
                try self.emit(&.{@enumToInt(OpCode.pop)});
            }
            self.locals.count -= 1;
        }
    }

    fn emitJump(self: *Self, op: OpCode) !Jmp {
        try self.emit(&.{ @enumToInt(op), 0x00, 0x00 });
        return Jmp{
            .chunk = self.currentChunk(),
            .start = self.currentChunk().code.count - 2,
        };
    }

    fn ifStatement(self: *Self) !void {
        try self.consume(.left_paren, "Expected '(' after if");
        try self.expression();
        try self.consume(.right_paren, "Expected ')' after if condition");

        // Jump to the else (or the end of then) if we don't match the condition
        var else_jmp = try self.emitJump(.jump_false);

        // Then block
        try self.emit(&.{@enumToInt(OpCode.pop)});
        try self.statement();
        var then_jmp = try self.emitJump(.jump); // Jump over the else
        // end then block

        // Else block
        else_jmp.set();
        try self.emit(&.{@enumToInt(OpCode.pop)});
        if (try self.match(.else_))
            try self.statement();
        // End else block

        then_jmp.set();
    }

    fn whileStatement(self: *Self) !void {
        try self.consume(.left_paren, "Expected '(' after while");
        const condition_loc = self.currentChunk().code.count;
        try self.expression();
        try self.consume(.right_paren, "Expected ')' after while condition");

        var condition_not_met_jmp = try self.emitJump(.jump_false);

        // Loop body
        try self.emit(&.{@enumToInt(OpCode.pop)});
        try self.statement();
        const diff = @intCast(u16, self.currentChunk().code.count - condition_loc);
        try self.emit(&.{ @enumToInt(OpCode.loop), @intCast(u8, diff >> 8), @intCast(u8, diff & 0xff) });

        condition_not_met_jmp.set();
        try self.emit(&.{@enumToInt(OpCode.pop)});
    }

    fn forStatement(self: *Self) !void {
        self.beginScope();

        try self.consume(.left_paren, "Expected '(' after for");
        // Declaration
        if (!(try self.match(.semicolon))) {
            try self.declaration();
        }

        // Condition
        var condition_not_met_jmp: ?Jmp = null;
        const condition_loc = self.currentChunk().code.count;
        if (!(try self.match(.semicolon))) {
            try self.expression();
            condition_not_met_jmp = try self.emitJump(.jump_false);
            try self.consume(.semicolon, "Expected ';' after for condition");
        }
        var skip_increment_jmp = try self.emitJump(.jump);

        // Increment
        const increment_loc = self.currentChunk().code.count;
        if (!(try self.match(.right_paren))) {
            try self.expression();
            try self.consume(.right_paren, "Expected ')' after for condition");
            try self.emit(&.{@enumToInt(OpCode.pop)});
        }
        var diff = @intCast(u16, self.currentChunk().code.count - condition_loc);
        try self.emit(&.{ @enumToInt(OpCode.loop), @intCast(u8, diff >> 8), @intCast(u8, diff & 0xff) });

        // Body
        skip_increment_jmp.set();
        if (condition_not_met_jmp != null)
            try self.emit(&.{@enumToInt(OpCode.pop)});
        try self.statement();
        diff = @intCast(u16, self.currentChunk().code.count - increment_loc);
        try self.emit(&.{ @enumToInt(OpCode.loop), @intCast(u8, diff >> 8), @intCast(u8, diff & 0xff) });

        if (condition_not_met_jmp) |*j| {
            j.set();
            try self.emit(&.{@enumToInt(OpCode.pop)});
        }

        try self.endScope();
    }

    fn statement(self: *Self) anyerror!void {
        if (try self.match(.print)) {
            try self.printStatement();
        } else if (try self.match(.return_)) {
            try self.returnStatement();
        } else if (try self.match(.left_brace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else if (try self.match(.if_)) {
            try self.ifStatement();
        } else if (try self.match(.while_)) {
            try self.whileStatement();
        } else if (try self.match(.for_)) {
            try self.forStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn addLocal(self: *Self, token: Token) !void {
        if (self.locals.count >= LOCAL_COUNT) {
            try self.err("Too many local variables");
            return;
        }
        var local = &self.locals.locals[self.locals.count];
        self.locals.count += 1;
        local.depth = -1;
        local.name = token;
        local.captured = false;
    }

    fn declareVariable(self: *Self) !void {
        if (self.locals.depth == 0) return;
        var i: isize = @intCast(isize, self.locals.count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals.locals[@intCast(usize, i)];
            if (local.depth != -1 and local.depth < self.locals.depth)
                break;
            if (identifiersEql(&local.name, &self.previous)) {
                try self.err("Already a variable with this name in this scope");
                return;
            }
        }
        try self.addLocal(self.previous);
    }

    fn parseVariable(self: *Self, msg: []const u8) !u8 {
        try self.consume(.identifier, msg);

        try self.declareVariable();
        if (self.locals.depth > 0) return 0;

        return try self.identifierConstant(&self.previous);
    }

    fn markInitialised(self: *Self) void {
        if (self.locals.depth == 0) return;
        self.locals.locals[self.locals.count - 1].depth = self.locals.depth;
    }

    fn defineVariable(self: *Self, constant: u8) !void {
        if (self.locals.depth > 0) {
            self.markInitialised();
            return;
        }

        try self.emit(&.{ @enumToInt(OpCode.define_global), constant });
    }

    fn varDecl(self: *Self) !void {
        const global = try self.parseVariable("Expect variable name");

        if (try self.match(.equal)) {
            try self.expression();
        } else {
            try self.emit(&.{@enumToInt(OpCode.nil)});
        }

        try self.consume(.semicolon, "Expect ';' after variable declaration");

        try self.defineVariable(global);
    }

    fn func(self: *Self, typ: FunctionType) !void {
        var compiler = try Self.inherit(self, typ);
        self.allocator.compiler = &compiler;
        // defer compiler.deinit();

        compiler.beginScope();

        try compiler.consume(.left_paren, "Expected '(' after function name");
        if (!compiler.check(.right_paren)) {
            while (true) {
                if (compiler.function.arity == 255)
                    try compiler.errorAtCurrent("Can't have more thatn 255 parameters");

                compiler.function.arity += 1;

                const constant = try compiler.parseVariable("Expect parameter name");
                try compiler.defineVariable(constant);
                if (!(try compiler.match(.comma)))
                    break;
            }
        }
        try compiler.consume(.right_paren, "Expected ')' after parameters");
        try compiler.consume(.left_brace, "Expected '{' before function body");
        try compiler.block();

        const f = (try compiler.end()) orelse unreachable;
        try self.emit(&.{ @enumToInt(OpCode.closure), try self.currentChunk().addConstant(&self.tmp_stack, .{ .object = &f.base }) });

        var i: usize = 0;
        while (i < f.upvalue_count) : (i += 1) {
            try self.emit(&.{ @boolToInt(compiler.upvalues[i].is_local), compiler.upvalues[i].index });
        }

        self.allocator.compiler = self;
        self.scanner = compiler.scanner;
        self.previous = compiler.previous;
        self.current = compiler.current;
    }

    fn funcDecl(self: *Self) !void {
        const global = try self.parseVariable("Expect function name");
        self.markInitialised();
        try self.func(.function);
        try self.defineVariable(global);
    }

    fn method(self: *Self) !void {
        try self.consume(.identifier, "Expect method name");
        const c = try self.identifierConstant(&self.previous);

        const typ = .method;
        try self.func(typ);

        try self.emit(&.{ @enumToInt(OpCode.method), c });
    }

    fn classDeclaration(self: *Self) !void {
        try self.consume(.identifier, "Expect class name");
        const name = self.previous;
        const name_const = try self.identifierConstant(&self.previous);
        try self.declareVariable();

        try self.emit(&.{ @enumToInt(OpCode.class), name_const });
        try self.defineVariable(name_const);
        try self.namedVariable(&name, false);

        try self.consume(.left_brace, "Expect '{' before class body");

        while (!self.check(.right_brace) and !self.check(.eof)) {
            try self.method();
        }

        try self.consume(.right_brace, "Expect '}' after class body");
        try self.emit(&.{@enumToInt(OpCode.pop)});
    }

    fn synchronise(self: *Self) !void {
        while (self.current.typ != .eof) {
            if (self.previous.typ == .semicolon) return;
            switch (self.current.typ) {
                .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
                else => {},
            }
            try self.advance();
        }
    }

    fn declaration(self: *Self) anyerror!void {
        if (try self.match(.var_)) {
            try self.varDecl();
        } else if (try self.match(.fun)) {
            try self.funcDecl();
        } else if (try self.match(.class)) {
            try self.classDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) try self.synchronise();
    }

    fn end(self: *Self) !?*memory.Function {
        if (self.had_error)
            return null;

        try self.emit(&.{ @enumToInt(OpCode.nil), @enumToInt(OpCode.ret) });

        return self.function;
    }

    pub fn compile(self: *Self) !?*memory.Function {
        self.allocator.compiler = self;
        try self.advance();
        while (!(try self.match(.eof))) {
            try self.declaration();
        }

        self.allocator.compiler = self.enclosing;

        return self.end();
    }
};
