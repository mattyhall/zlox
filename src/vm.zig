const std = @import("std");
const ds = @import("ds.zig");

const Allocator = std.mem.Allocator;
const Value = ds.Value;

const DEBUG_TRACE_EXECUTION = false;

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
    print,
    pop,
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,
    jump_false,
    jump,
    loop,
    call,
    closure,
    get_upvalue,
    set_upvalue,
    close_upvalue,
};

const LineInfo = struct {
    line: usize,
    count: usize,
};

const CallFrame = struct {
    closure: *ds.Closure,

    // Current ip of this function. If this function (a) calls another (b) then when b returns we'll reset the vm ip to a's.
    ip: [*]const u8,

    slots: [*]ds.Value, // Pointer into the stack to the first slot the function can use
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

    pub fn incLine(self: *Self, line: usize) !void {
        const line_infos = self.lines.count;
        if (line_infos == 0 or self.lines.data[self.lines.count - 1].line != line) {
            try self.lines.append(.{ .line = line, .count = 1 });
        } else {
            self.lines.data[line_infos - 1].count += 1;
        }
    }

    pub fn writeChunk(self: *Self, byte: u8) !void {
        try self.code.append(byte);
    }

    fn disassembleSimpleInstruction(stdout: anytype, offset: usize, instruction: OpCode) !usize {
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
            .print => "PRINT",
            .pop => "POP",
            .close_upvalue => "CLOSE",
            else => unreachable,
        };
        try stdout.print(" {s:<5}\n", .{s});
        return offset + 1;
    }

    fn disassembleConstantInstruction(self: *const Self, stdout: anytype, offset: usize, instruction: OpCode) !usize {
        const index = self.code.data[offset + 1];
        const val = self.values.data[index];
        const s = switch (instruction) {
            .constant => "CONST",
            .define_global => "DEFG",
            .get_global => "GETG",
            .set_global => "SETG",
            else => unreachable,
        };
        try stdout.print(" {s:<5} c{} (", .{ s, index });
        try val.print(stdout);
        try stdout.print(")\n", .{});
        return offset + 2;
    }

    fn disassembleByteInstruction(self: *const Self, stdout: anytype, offset: usize, instruction: OpCode) !usize {
        const index = self.code.data[offset + 1];
        const s = switch (instruction) {
            .get_local => "GETL ",
            .set_local => "SETL ",
            .call => "CALL ",
            .get_upvalue => "GETU ",
            .set_upvalue => "SETU ",
            else => unreachable,
        };
        try stdout.print(" {s:<7}{} ", .{ s, index });
        try stdout.print("\n", .{});
        return offset + 2;
    }

    fn disassembleJump(self: *const Self, stdout: anytype, offset: usize, instruction: OpCode, dir: enum { back, forward }) !usize {
        const jmp_offset = @intCast(u16, self.code.data[offset + 1]) << 8 | @intCast(u16, self.code.data[offset + 2]);
        const s = switch (instruction) {
            .jump_false => "JMPF",
            .jump => "JMP",
            .loop => "LOOP",
            else => unreachable,
        };

        var index = offset;
        if (dir == .back) {
            index -= jmp_offset;
        } else {
            index += jmp_offset + 3;
        }

        try stdout.print(" {s:<5} {} (@{x:0>4}) ", .{ s, jmp_offset, index });
        try stdout.print("\n", .{});
        return offset + 3;
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
            .print,
            .pop,
            .close_upvalue,
            => return disassembleSimpleInstruction(stdout, offset, instruction),
            .constant,
            .define_global,
            .get_global,
            .set_global,
            => return self.disassembleConstantInstruction(stdout, offset, instruction),
            .get_local,
            .set_local,
            .call,
            .get_upvalue,
            .set_upvalue,
            => return self.disassembleByteInstruction(stdout, offset, instruction),
            .jump_false,
            .jump,
            => return self.disassembleJump(stdout, offset, instruction, .forward),
            .loop => return self.disassembleJump(stdout, offset, instruction, .back),
            .closure => {
                const index = self.code.data[offset + 1];
                const val = self.values.data[index];
                try stdout.print(" {s:<5} c{} (", .{ "CLOSURE", index });
                try val.print(stdout);
                try stdout.print(")\n", .{});
                var increase: usize = 2;
                var i: u8 = 0;
                const func = val.object.toFunction();
                while (i < func.upvalue_count) : (i += 1) {
                    const is_local = self.code.data[offset + increase];
                    increase += 1;
                    const u_index = self.code.data[offset + increase];
                    increase += 1;
                    const label: []const u8 = if (is_local == 1) "local" else "upvalue";
                    try stdout.print("{x:0>4}    |       {s} {}\n", .{ offset + increase - 2, label, u_index });
                }
                return offset + increase;
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

var timer: std.time.Timer = undefined;

fn clockNative(arg_count: u8, args: [*]ds.Value) ds.Value {
    _ = arg_count;
    _ = args;
    const elapsed = timer.read();
    return .{ .number = @intToFloat(f32, elapsed) / @intToFloat(f32, std.time.ns_per_s) };
}

pub const Vm = struct {
    allocator: *ds.ObjectAllocator,
    chunk: ?*const Chunk,
    stack: ds.Stack,
    globals: ds.Table,

    frames: [ds.FRAMES_MAX]CallFrame,
    frame_count: usize,

    open_upvalues: ?*ds.Upvalue,

    const Self = @This();
    // TODO: be cleverer
    pub const Error = anyerror; //if (DEBUG_TRACE_EXECUTION) anyerror else InterpretError;

    pub fn init(allocator: *ds.ObjectAllocator) !Self {
        var self = Self{
            .allocator = allocator,
            .chunk = null,
            .stack = undefined,
            .globals = try ds.Table.init(allocator.allocator),
            .frames = undefined,
            .frame_count = 0,
            .open_upvalues = null,
        };
        return self;
    }

    fn reset(self: *Self) void {
        self.stack.reset();
    }

    inline fn readByte(self: *Self, frame: *CallFrame) u8 {
        _ = self;
        frame.ip += 1;
        return (frame.ip - 1)[0];
    }

    inline fn readShort(self: *Self, frame: *CallFrame) u16 {
        return @intCast(u16, self.readByte(frame)) << 8 | @intCast(u16, self.readByte(frame));
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
            self.stack.push(.{
                .boolean = switch (@as(ds.Type, a)) {
                    .nil => true,
                    .boolean => if (op == .equal) a.boolean == b.boolean else a.boolean != b.boolean,
                    .number => if (op == .equal) a.number == b.number else a.number != b.number,
                    .object => switch (a.object.typ) {
                        .string => if (op == .equal) a.object == b.object else a.object != b.object,
                        .function => false, // TODO: fix
                        .native => false,
                        .closure => false,
                        .upvalue => unreachable,
                    },
                },
            });
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

    fn call(self: *Self, closure: *ds.Closure, arg_count: usize) !void {
        if (arg_count != closure.function.arity) {
            try self.runtimeError("Expected {} arguments but got {}", .{ closure.function.arity, arg_count });
            return error.runtime_error;
        }
        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.data;
        frame.slots = self.stack.top - arg_count - 1;
    }

    fn callValue(self: *Self, callee: ds.Value, arg_count: u8) !void {
        if (callee == .object) {
            switch (callee.object.typ) {
                .closure => {
                    try self.call(callee.object.toClosure(), arg_count);
                    return;
                },
                .native => {
                    const f = callee.object.toNative();
                    const res = f.function(arg_count, self.stack.top - arg_count);
                    self.stack.top -= arg_count + 1;
                    self.stack.push(res);
                    return;
                },
                else => {},
            }
        }
        try self.runtimeError("Can only call functions and classes", .{});
        return error.runtime_error;
    }

    fn captureUpvalue(self: *Self, local: *Value) !*ds.Upvalue {
        var prev_upvalue: ?*ds.Upvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local)
            return upvalue.?;

        var created_upvalue = try self.allocator.newUpvalue(local);
        created_upvalue.next = upvalue;
        if (prev_upvalue == null) {
            self.open_upvalues = created_upvalue;
        } else {
            prev_upvalue.?.next = created_upvalue;
        }
        return created_upvalue;
    }

    fn closeUpvalues(self: *Self, last: *Value) void {
        while (self.open_upvalues != null and @ptrToInt(self.open_upvalues.?.location) >= @ptrToInt(last)) {
            const upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn defineNative(self: *Self, name: []const u8, f: ds.NativeFn) !void {
        self.stack.push(.{ .object = try self.allocator.allocString(name) });
        self.stack.push(.{ .object = &(try self.allocator.newNative(f)).base });

        _ = try self.globals.insert(self.stack.data[0].toString(), self.stack.data[1]);

        _ = self.stack.pop();
        _ = self.stack.pop();
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const stderr = std.io.getStdErr().writer();
        try stderr.print(fmt, args);
        try stderr.print("\n", .{});

        // TODO: print line number
        self.reset();
    }

    fn run(self: *Self) Error!Value {
        var frame = &self.frames[self.frame_count - 1];
        var chunk = frame.closure.function.chunk;

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
                const offset = @ptrToInt(frame.ip) - @ptrToInt(chunk.code.data);
                try self.stack.debugPrint(stdout);
                _ = try chunk.disassembleInstruction(stdout, offset, line);
            }

            const byte = self.readByte(frame);
            const op = @intToEnum(OpCode, byte);
            switch (op) {
                .constant => {
                    const index = self.readByte(frame);
                    self.stack.push(chunk.values.data[index]);
                },
                .ret => {
                    const res = self.stack.pop();
                    self.closeUpvalues(&frame.slots[0]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0)
                        return res;
                    self.stack.top = frame.slots;
                    self.stack.push(res);
                    frame = &self.frames[self.frame_count - 1];
                    chunk = frame.closure.function.chunk;
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
                .print => {
                    const val = self.stack.pop();
                    try val.print(stdout);
                    try stdout.print("\n", .{});
                },
                .pop => _ = self.stack.pop(),
                .define_global => {
                    const index = self.readByte(frame);
                    const name = chunk.values.data[index].object.toString();
                    _ = try self.globals.insert(name, self.stack.peek(0));
                    _ = self.stack.pop();
                },
                .get_global => {
                    const index = self.readByte(frame);
                    const name = chunk.values.data[index].object.toString();
                    const val = self.globals.find(name);
                    if (val) |v| {
                        self.stack.push(v.value);
                    } else {
                        try self.runtimeError("Undefined variable '{s}'", .{name.chars});
                        return error.runtime_error;
                    }
                },
                .set_global => {
                    const index = self.readByte(frame);
                    const name = chunk.values.data[index].object.toString();
                    if (!try self.globals.insert(name, self.stack.peek(0))) {
                        self.globals.delete(name);
                        try self.runtimeError("Undefined global variable '{s}'", .{name.chars});
                        return error.runtime_error;
                    }
                },
                .get_local => {
                    const index = self.readByte(frame);
                    self.stack.push(frame.slots[index]);
                },
                .set_local => {
                    const index = self.readByte(frame);
                    frame.slots[index] = self.stack.peek(0);
                },
                .jump_false => {
                    const offset = self.readShort(frame);
                    if (self.stack.peek(0).falsey()) {
                        frame.ip += offset;
                    }
                },
                .jump => {
                    const offset = self.readShort(frame);
                    frame.ip += offset;
                },
                .loop => {
                    frame.ip -= self.readShort(frame) + 1;
                },
                .call => {
                    const arg_count = self.readByte(frame);
                    try self.callValue(self.stack.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                    chunk = frame.closure.function.chunk;
                },
                .closure => {
                    const index = self.readByte(frame);
                    const func = chunk.values.data[index];
                    const closure = try self.allocator.newClosure(func.object.toFunction());
                    self.stack.push(.{ .object = &closure.base });

                    try closure.upvalues.reserve(closure.function.upvalue_count);
                    var i: usize = 0;
                    while (i < closure.function.upvalue_count) : (i += 1) {
                        const is_local = self.readByte(frame);
                        const s_index = self.readByte(frame);
                        if (is_local == 1) {
                            closure.upvalues.data[i] = try self.captureUpvalue(&frame.slots[s_index]);
                        } else {
                            closure.upvalues.data[i] = frame.closure.upvalues.data[s_index];
                        }
                    }
                },
                .get_upvalue => {
                    const slot = self.readByte(frame);
                    self.stack.push(frame.closure.upvalues.data[slot].location.*);
                },
                .set_upvalue => {
                    const slot = self.readByte(frame);
                    frame.closure.upvalues.data[slot].location.* = self.stack.peek(0);
                },
                .close_upvalue => {
                    self.closeUpvalues(&(self.stack.top - 1)[0]);
                    _ = self.stack.pop();
                },
            }
            first = false;
        }
    }

    pub fn interpret(self: *Self, function: *ds.Function) Error!Value {
        self.stack.reset();

        try self.defineNative("clock", clockNative);
        timer = try std.time.Timer.start();

        const closure = try self.allocator.newClosure(function);
        self.stack.push(.{ .object = &function.base });
        _ = self.stack.pop();
        self.stack.push(.{ .object = &closure.base });

        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.closure = closure;
        frame.ip = function.chunk.code.data;
        frame.slots = &self.stack.data;

        return try self.run();
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();
    }
};
