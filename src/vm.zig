const std = @import("std");
const ds = @import("ds.zig");

const Allocator = std.mem.Allocator;
const Value = ds.Value;

const DEBUG_TRACE_EXECUTION = true;

pub const OpCode = enum(u8) {
    ret,
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
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
            .add => "ADD",
            .subtract => "SUB",
            .multiply => "MUL",
            .divide => "DIV",
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
            .ret, .negate, .add, .subtract, .multiply, .divide => return disassembleByteInstruction(stdout, offset, instruction),
            .constant => {
                const index = self.code.data[offset + 1];
                const val = self.values.data[index];
                try stdout.print(" {s:<5} {} ", .{ "CONST", index });
                try stdout.print("({d:.[precision]})\n", .{ .number = val, .precision = ds.FLOAT_PRECISION });
                return offset + 2;
            },
        }
    }

    fn disassemble(self: *const Self, name: []const u8) !void {
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
    chunk: ?*const Chunk,
    ip: [*]const u8,
    stack: ds.Stack,

    const Self = @This();
    const Error = if (DEBUG_TRACE_EXECUTION) anyerror else InterpretError;

    pub fn init() Self {
        return .{
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

    fn runBinaryOp(self: *Self, op: OpCode) void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        const res = r: {
            switch (op) {
                .add => break :r a + b,
                .subtract => break :r a - b,
                .multiply => break :r a * b,
                .divide => break :r a / b,
                else => unreachable,
            }
        };
        self.stack.push(res);
    }

    fn run(self: *Self) Error!void {
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
                    try stdout.print("{d:.[precision]}\n", .{ .number = self.stack.pop(), .precision = ds.FLOAT_PRECISION });
                    return;
                },
                .negate => {
                    const val = self.stack.pop();
                    self.stack.push(-val);
                },
                .add, .subtract, .multiply, .divide => self.runBinaryOp(op),
            }
            first = false;
        }
    }

    pub fn interpret(self: *Self, chunk: *const Chunk) Error!void {
        self.chunk = chunk;
        self.ip = chunk.code.data;
        return try self.run();
    }
};
