const std = @import("std");
const Allocator = std.mem.Allocator;

const DEBUG_TRACE_EXECUTION = true;

const FLOAT_PRECISION = 6;

const MAX_STACK = 256;

const OpCode = enum(u8) {
    ret,
    constant,
    negate,
    add,
    subtract,
    multiply,
    divide,
};

const Value = f32;

const LineInfo = struct {
    line: usize,
    count: usize,
};

fn DynamicArray(comptime T: type) type {
    return struct {
        data: [*]T,
        count: usize,
        capacity: usize,

        allocator: *Allocator,

        const Self = @This();

        fn init(allocator: *Allocator) Self {
            return Self{
                .allocator = allocator,
                // Need an aligned address so chose 64
                .data = @intToPtr([*]T, 0x40),
                .count = 0,
                .capacity = 0,
            };
        }

        fn items(self: *Self) []T {
            var s: []T = undefined;
            s.ptr = self.data;
            s.len = self.count;
            return s;
        }

        fn slice(self: *Self) []T {
            var s: []T = undefined;
            s.ptr = self.data;
            s.len = self.capacity;
            return s;
        }

        fn append(self: *Self, v: T) !void {
            if (self.capacity < self.count + 1) {
                if (self.capacity == 0) {
                    self.capacity = 8;
                    self.data = (try self.allocator.alloc(T, 8)).ptr;
                } else {
                    self.capacity *= 2;
                    self.data = (try self.allocator.realloc(self.slice(), self.capacity)).ptr;
                }
            }
            self.data[self.count] = v;
            self.count += 1;
        }

        fn deinit(self: *Self) void {
            self.allocator.deinit(self.slice());
        }
    };
}

const Chunk = struct {
    code: DynamicArray(u8),
    values: DynamicArray(Value),
    lines: DynamicArray(LineInfo),

    allocator: *Allocator,

    const Self = @This();

    const LineDissasemble = union(enum) { new: usize, old };

    fn init(allocator: *Allocator) Self {
        return Self{
            .allocator = allocator,
            .code = DynamicArray(u8).init(allocator),
            .values = DynamicArray(Value).init(allocator),
            .lines = DynamicArray(LineInfo).init(allocator),
        };
    }

    fn addConstant(self: *Self, val: Value) !u8 {
        if (self.values.count == 256)
            return error.TooManyConstants;

        try self.values.append(val);
        return @intCast(u8, self.values.count - 1);
    }

    fn writeChunk(self: *Self, byte: u8, line: usize) !void {
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
                try stdout.print("({d:.[precision]})\n", .{ .number = val, .precision = FLOAT_PRECISION });
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

    fn deinit(self: *Self) void {
        self.allocator.deinit(self.code);
        self.code.deinit();
        self.values.deinit();
        self.lines.deinit();
    }
};

const InterpretError = error{
    compile_error,
    runtime_error,
};

const Stack = struct {
    data: [MAX_STACK]Value,
    top: [*]Value,

    const Self = @This();

    fn reset(self: *Self) void {
        self.top = &self.data;
    }

    fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn debugPrint(self: *Self, stdout: anytype) !void {
        var current: [*]Value = &self.data;
        if (current == self.top) return;
        try stdout.print("    ", .{});
        while (current != self.top) : (current += 1) {
            try stdout.print("[{d:.[precision]}] ", .{ .number = current[0], .precision = FLOAT_PRECISION });
        }
        try stdout.print("\n", .{});
    }
};

const Vm = struct {
    chunk: ?*const Chunk,
    ip: [*]const u8,
    stack: Stack,

    const Self = @This();
    const Error = if (DEBUG_TRACE_EXECUTION) anyerror else InterpretError;

    fn init() Self {
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
                    try stdout.print("{d:.[precision]}\n", .{ .number = self.stack.pop(), .precision = FLOAT_PRECISION });
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

    fn interpret(self: *Self, chunk: *const Chunk) Error!void {
        self.chunk = chunk;
        self.ip = chunk.code.data;
        return try self.run();
    }
};

pub fn main() anyerror!void {
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var chunk = Chunk.init(&alloc.allocator);

    var constant = try chunk.addConstant(1.2);
    try chunk.writeChunk(@enumToInt(OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    constant = try chunk.addConstant(3.4);
    try chunk.writeChunk(@enumToInt(OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    try chunk.writeChunk(@enumToInt(OpCode.add), 123);

    constant = try chunk.addConstant(5.6);
    try chunk.writeChunk(@enumToInt(OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    try chunk.writeChunk(@enumToInt(OpCode.divide), 123);

    try chunk.writeChunk(@enumToInt(OpCode.negate), 123);
    try chunk.writeChunk(@enumToInt(OpCode.ret), 123);

    var vm = Vm.init();
    try vm.interpret(&chunk);
}
