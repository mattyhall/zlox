const std = @import("std");
const ds = @import("ds.zig");

const Allocator = std.mem.Allocator;
const Value = ds.Value;

const DEBUG_TRACE_EXECUTION = true;

pub const OpCode = enum(u8) {
    ret,
    constant,
    nil,
    true_,
    false_,
    negate,
    not,
    add,
    subtract,
    multiply,
    divide,
    equal,
    not_equal,
    greater,
    greater_equal,
    less,
    less_equal,
};

const LineInfo = struct {
    line: usize,
    count: usize,
};

pub const Chunk = struct {
    code: ds.DynamicArray(u8),
    values: ds.DynamicArray(Value),
    lines: ds.DynamicArray(LineInfo),

    allocator: *Allocator,

    const Self = @This();

    const LineDissasemble = union(enum) { new: usize, old };

    pub fn init(allocator: *Allocator) Self {
        return Self{
            .allocator = allocator,
            .code = ds.DynamicArray(u8).init(allocator),
            .values = ds.DynamicArray(Value).init(allocator),
            .lines = ds.DynamicArray(LineInfo).init(allocator),
        };
    }

    pub fn addConstant(self: *Self, val: Value) !u8 {
        if (self.values.count == 256)
            return error.TooManyConstants;

        try self.values.append(val);
        return @intCast(u8, self.values.count - 1);
    }

    pub fn writeChunk(self: *Self, byte: u8, line: usize) !void {
        const line_infos = self.lines.count;
        if (line_infos == 0 or self.lines.data[self.lines.count - 1].line != line) {
            try self.lines.append(.{ .line = line, .count = 1 });
        } else {
            self.lines.data[line_infos - 1].count += 1;
        }
        try self.code.append(byte);
    }

    fn disassembleByteInstruction(stdout: anytype, offset: usize, instruction: OpCode) !usize {
        const s = switch (instruction) {
            .ret => "RET",
            .negate => "NEGATE",
            .not => "NOT",
            .add => "ADD",
            .subtract => "SUB",
            .multiply => "MUL",
            .divide => "DIV",
            .true_ => "TRUE",
            .false_ => "FALSE",
            .nil => "NIL",
            .equal => "EQ",
            .not_equal => "NEQ",
            .greater => "GT",
            .greater_equal => "GTE",
            .less => "LT",
            .less_equal => "LTE",
            else => unreachable,
        };
        try stdout.print(" {s:<5}\n", .{s});
        return offset + 1;
    }

    fn disassembleInstruction(self: *const Self, stdout: anytype, offset: usize, line: LineDissasemble) !usize {
        try stdout.print("{x:0>4} ", .{offset});
        switch (line) {
            .new => |l| try stdout.print("{:>4} ", .{l}),
            .old => try stdout.print("   | ", .{}),
        }
        const instruction = @intToEnum(OpCode, self.code.data[offset]);
        switch (instruction) {
            .ret,
            .negate,
            .add,
            .subtract,
            .multiply,
            .divide,
            .true_,
            .false_,
            .nil,
            .not,
            .equal,
            .not_equal,
            .greater,
            .greater_equal,
            .less,
            .less_equal,
            => return disassembleByteInstruction(stdout, offset, instruction),
            .constant => {
                const index = self.code.data[offset + 1];
                const val = self.values.data[index];
                try stdout.print(" {s:<5} {} ", .{ "CONST", index });
                try val.print(stdout);
                try stdout.print("\n", .{});
                return offset + 2;
            },
        }
    }

    pub fn disassemble(self: *const Self, name: []const u8) !void {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("== {s} ==\n", .{name});
        if (self.code.count == 0)
            return;

        var offset: usize = 0;
        var line_offset: usize = 0;
        var current_line_length: usize = self.lines.data[0].count;
        while (offset < self.code.count) {
            var line: LineDissasemble = if (offset == 0) .{ .new = self.lines.data[0].line } else .old;
            if (current_line_length == 0) {
                line_offset += 1;
                current_line_length = self.lines.data[line_offset].count;
                line = .{ .new = self.lines.data[line_offset].line };
            }
            offset = try self.disassembleInstruction(stdout, offset, line);
            current_line_length -= 1;
        }
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.values.deinit();
        self.lines.deinit();
    }
};

pub const InterpretError = error{
    compile_error,
    runtime_error,
};

pub const Vm = struct {
    allocator: *ds.ObjectAllocator,
    chunk: ?*const Chunk,
    ip: [*]const u8,
    stack: ds.Stack,

    const Self = @This();
    // TODO: be cleverer
    pub const Error = anyerror; //if (DEBUG_TRACE_EXECUTION) anyerror else InterpretError;

    pub fn init(allocator: *ds.ObjectAllocator) Self {
        return .{
            .allocator = allocator,
            .chunk = null,
            .ip = @intToPtr([*]const u8, 0x8),
            .stack = undefined,
        };
    }

    fn reset(self: *Self) void {
        self.stack.reset();
    }

    inline fn readByte(self: *Self) u8 {
        const v = self.ip[0];
        self.ip += 1;
        return v;
    }

    fn runBinaryOp(self: *Self, op: OpCode) Error!void {
        if (op == .add and self.stack.peek(1).isString() and self.stack.peek(0).isString()) {
            const b = self.stack.pop().toZigString();
            const a = self.stack.pop().toZigString();
            self.stack.push(.{ .object = try self.allocator.concatenateStrings(a, b) });
            return;
        }

        if (self.stack.peek(1) != .number or self.stack.peek(0) != .number) {
            try self.runtimeError("Operands must be numbers", .{});
            return Error.runtime_error;
        }
        const b = self.stack.pop().number;
        const a = self.stack.pop().number;
        const res = r: {
            switch (op) {
                .add => break :r a + b,
                .subtract => break :r a - b,
                .multiply => break :r a * b,
                .divide => break :r a / b,
                else => unreachable,
            }
        };
        self.stack.push(.{ .number = res });
    }

    fn runBoolBinaryOp(self: *Self, op: OpCode) Error!void {
        if (op == .equal or op == .not_equal) {
            const b = self.stack.pop();
            const a = self.stack.pop();
            if (@as(ds.Type, a) != @as(ds.Type, b)) {
                self.stack.push(.{ .boolean = if (op == .equal) false else true });
                return;
            }
            self.stack.push(.{ .boolean = switch (@as(ds.Type, a)) {
                .nil => true,
                .boolean => if (op == .equal) a.boolean == b.boolean else a.boolean != b.boolean,
                .number => if (op == .equal) a.number == b.number else a.number != b.number,
                .object => switch (a.object.typ) {
                    .string => b: {
                        const as = a.object.toZigString();
                        const bs = b.object.toZigString();
                        break :b std.mem.eql(u8, as, bs);
                    },
                },
            } });
            return;
        }
        if (self.stack.peek(1) != .number or self.stack.peek(0) != .number) {
            try self.runtimeError("Operands must be numbers", .{});
            return Error.runtime_error;
        }
        const b = self.stack.pop().number;
        const a = self.stack.pop().number;
        const res = switch (op) {
            .less => a < b,
            .less_equal => a <= b,
            .greater => a > b,
            .greater_equal => a >= b,
            else => unreachable,
        };
        self.stack.push(.{ .boolean = res });
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const stderr = std.io.getStdErr().writer();
        try stderr.print(fmt, args);
        try stderr.print("\n", .{});

        // TODO: print line number
        self.reset();
    }

    fn run(self: *Self) Error!Value {
        self.stack.reset();

        const chunk = self.chunk.?;

        const stdout = std.io.getStdOut().writer();
        var line_offset: usize = 0;
        var current_line_length: usize = chunk.lines.data[0].count;
        var first = true;

        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                var line: Chunk.LineDissasemble = if (first) .{ .new = chunk.lines.data[0].line } else .old;
                if (current_line_length == 0) {
                    line_offset += 1;
                    current_line_length = chunk.lines.data[line_offset].count;
                    line = .{ .new = chunk.lines.data[line_offset].line };
                }
                const offset = @ptrToInt(self.ip) - @ptrToInt(chunk.code.data);
                try self.stack.debugPrint(stdout);
                _ = try chunk.disassembleInstruction(stdout, offset, line);
            }

            const op = @intToEnum(OpCode, self.readByte());
            switch (op) {
                .constant => {
                    const index = self.readByte();
                    self.stack.push(chunk.values.data[index]);
                },
                .ret => {
                    const val = self.stack.pop();
                    try val.print(stdout);
                    try stdout.print("\n", .{});
                    return val;
                },
                .negate => {
                    if (self.stack.peek(0) != .number) {
                        try self.runtimeError("Operand must be a number", .{});
                        return Error.runtime_error;
                    }

                    const val = self.stack.pop();
                    self.stack.push(.{ .number = -(val.number) });
                },
                .not => self.stack.push(.{ .boolean = self.stack.pop().falsey() }),
                .true_ => self.stack.push(.{ .boolean = true }),
                .false_ => self.stack.push(.{ .boolean = false }),
                .nil => self.stack.push(.{ .nil = undefined }),
                .add, .subtract, .multiply, .divide => try self.runBinaryOp(op),
                .equal, .not_equal, .less, .less_equal, .greater, .greater_equal => try self.runBoolBinaryOp(op),
            }
            first = false;
        }
    }

    pub fn interpret(self: *Self, chunk: *const Chunk) Error!Value {
        self.chunk = chunk;
        self.ip = chunk.code.data;
        return try self.run();
    }
};
