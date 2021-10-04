const std = @import("std");
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) {
    ret,
    constant,
};

const Value = f32;

const Chunk = struct {
    code: [*]u8,
    count: usize,
    capacity: usize,

    values: std.ArrayList(Value),

    allocator: *Allocator,

    const Self = @This();

    fn init(allocator: *Allocator) Self {
        return Self{
            .allocator = allocator,
            .code = @intToPtr([*]u8, 0xdeadbeef),
            .count = 0,
            .capacity = 0,
            .values = std.ArrayList(Value).init(allocator),
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

    fn writeChunk(self: *Self, byte: u8) !void {
        if (self.capacity < self.count + 1) {
            if (self.capacity == 0) {
                self.capacity = 8;
                self.code = (try self.allocator.alloc(u8, 8)).ptr;
            } else {
                self.capacity *= 2;
                self.code = (try self.allocator.realloc(self.codeSlice() orelse unreachable, self.capacity)).ptr;
            }
        }
        self.code[self.count] = byte;
        self.count += 1;
    }

    fn disassembleInstruction(self: *const Self, stdout: anytype, offset: usize) !usize {
        try stdout.print("{x:0>4} ", .{offset});
        switch (@intToEnum(OpCode, self.code[offset])) {
            .ret => {
                try stdout.print(" {s:<5}\n", .{"RET"});
                return offset + 1;
            },
            .constant => {
              const index = self.code[offset + 1];
              const val = self.values.items[index];
              try stdout.print(" {s:<5} {} ({})\n", .{"CONST", index, val});
              return offset + 2;
            },
        }
    }

    fn disassemble(self: *const Self, name: []const u8) !void {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("== {s} ==\n", .{name});
        var offset: usize = 0;
        while (offset < self.count) {
            offset = try self.disassembleInstruction(stdout, offset);
        }
    }

    fn deinit(self: *Self) void {
        self.allocator.deinit(self.code);
        self.values.deinit();
    }
};

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var chunk = Chunk.init(&alloc.allocator);
    const constant = try chunk.addConstant(1.3);
    try chunk.writeChunk(@enumToInt(OpCode.constant));
    try chunk.writeChunk(constant);
    try chunk.writeChunk(@enumToInt(OpCode.ret));

    try chunk.disassemble("test chunk");
}
