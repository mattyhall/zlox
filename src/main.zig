const std = @import("std");
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) {
    ret,
    constant,
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

    const LineDissasemble = union(enum) { new: usize, old };

    fn disassembleInstruction(self: *const Self, stdout: anytype, offset: usize, line: LineDissasemble) !usize {
        try stdout.print("{x:0>4} ", .{offset});
        switch (line) {
            .new => |l| try stdout.print("{:>4} ", .{l}),
            .old => try stdout.print("   | ", .{}),
        }
        switch (@intToEnum(OpCode, self.code.data[offset])) {
            .ret => {
                try stdout.print(" {s:<5}\n", .{"RET"});
                return offset + 1;
            },
            .constant => {
                const index = self.code.data[offset + 1];
                const val = self.values.data[index];
                try stdout.print(" {s:<5} {} ({})\n", .{ "CONST", index, val });
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

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var chunk = Chunk.init(&alloc.allocator);
    const constant = try chunk.addConstant(1.3);
    try chunk.writeChunk(@enumToInt(OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);
    try chunk.writeChunk(@enumToInt(OpCode.ret), 123);

    try chunk.disassemble("test chunk");
}
