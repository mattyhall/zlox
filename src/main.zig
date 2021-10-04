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

const Chunk = struct {
    code: [*]u8,
    count: usize,
    capacity: usize,

    values: std.ArrayList(Value),

    lines: std.ArrayList(LineInfo),

    allocator: *Allocator,

    const Self = @This();

    fn init(allocator: *Allocator) Self {
        return Self{
            .allocator = allocator,
            .code = @intToPtr([*]u8, 0xdeadbeef),
            .count = 0,
            .capacity = 0,
            .values = std.ArrayList(Value).init(allocator),
            .lines = std.ArrayList(LineInfo).init(allocator),
        };
    }

    fn codeSlice(self: *Self) ?[]u8 {
        if (self.count == 0)
            return null;
        var res: []u8 = undefined;
        res.ptr = self.code;
        res.len = self.count;
        return res;
    }

    fn addConstant(self: *Self, val: Value) !u8 {
        if (self.values.items.len == 256)
            return error.TooManyConstants;

        try self.values.append(val);
        return @intCast(u8, self.values.items.len - 1);
    }

    fn writeChunk(self: *Self, byte: u8, line: usize) !void {
        if (self.capacity < self.count + 1) {
            if (self.capacity == 0) {
                self.capacity = 8;
                self.code = (try self.allocator.alloc(u8, 8)).ptr;
            } else {
                self.capacity *= 2;
                self.code = (try self.allocator.realloc(self.codeSlice() orelse unreachable, self.capacity)).ptr;
            }
        }
        const line_infos = self.lines.items.len;
        if (line_infos == 0 or self.lines.items[self.lines.items.len - 1].line != line) {
            try self.lines.append(.{ .line = line, .count = 1 });
        } else {
            self.lines.items[line_infos - 1].count += 1;
        }
        self.code[self.count] = byte;
        self.count += 1;
    }

    const LineDissasemble = union(enum) { new: usize, old };

    fn disassembleInstruction(self: *const Self, stdout: anytype, offset: usize, line: LineDissasemble) !usize {
        try stdout.print("{x:0>4} ", .{offset});
        switch (line) {
            .new => |l| try stdout.print("{:>4} ", .{l}),
            .old => try stdout.print("   | ", .{}),
        }
        switch (@intToEnum(OpCode, self.code[offset])) {
            .ret => {
                try stdout.print(" {s:<5}\n", .{"RET"});
                return offset + 1;
            },
            .constant => {
                const index = self.code[offset + 1];
                const val = self.values.items[index];
                try stdout.print(" {s:<5} {} ({})\n", .{ "CONST", index, val });
                return offset + 2;
            },
        }
    }

    fn disassemble(self: *const Self, name: []const u8) !void {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("== {s} ==\n", .{name});
        if (self.count == 0)
            return;

        var offset: usize = 0;
        var line_offset: usize = 0;
        var current_line_length: usize = self.lines.items[0].count;
        while (offset < self.count) {
            var line: LineDissasemble = if (offset == 0) .{ .new = self.lines.items[0].line } else .old;
            if (current_line_length == 0) {
                line_offset += 1;
                current_line_length = self.lines.items[line_offset].count;
                line = .{ .new = self.lines.items[line_offset].line };
            }
            offset = try self.disassembleInstruction(stdout, offset, line);
            current_line_length -= 1;
        }
    }

    fn deinit(self: *Self) void {
        self.allocator.deinit(self.code);
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
