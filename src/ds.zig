const std = @import("std");
const Allocator = std.mem.Allocator;

const MAX_STACK = 256;
pub const FLOAT_PRECISION = 6;
pub const Value = f32;

pub fn DynamicArray(comptime T: type) type {
    return struct {
        data: [*]T,
        count: usize,
        capacity: usize,

        allocator: *Allocator,

        const Self = @This();

        pub fn init(allocator: *Allocator) Self {
            return Self{
                .allocator = allocator,
                // Need an aligned address so chose 64
                .data = @intToPtr([*]T, 0x40),
                .count = 0,
                .capacity = 0,
            };
        }

        pub fn items(self: *Self) []T {
            var s: []T = undefined;
            s.ptr = self.data;
            s.len = self.count;
            return s;
        }

        pub fn slice(self: *Self) []T {
            var s: []T = undefined;
            s.ptr = self.data;
            s.len = self.capacity;
            return s;
        }

        pub fn append(self: *Self, v: T) !void {
            if (self.capacity < self.count + 1) {
                if (self.capacity == 0) {
                    self.capacity = 8;
                    self.data = (try self.allocator.alloc(T, 8)).ptr;
                } else {
                    self.capacity *= 2;
                    self.data = (try self.allocator.realloc(self.items(), self.capacity)).ptr;
                }
            }
            self.data[self.count] = v;
            self.count += 1;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.slice());
        }
    };
}

pub const Stack = struct {
    data: [MAX_STACK]Value,
    top: [*]Value,

    const Self = @This();

    pub fn reset(self: *Self) void {
        self.top = &self.data;
    }

    pub fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    pub fn debugPrint(self: *Self, stdout: anytype) !void {
        var current: [*]Value = &self.data;
        if (current == self.top) return;
        try stdout.print("    ", .{});
        while (current != self.top) : (current += 1) {
            try stdout.print("[{d:.[precision]}] ", .{ .number = current[0], .precision = FLOAT_PRECISION });
        }
        try stdout.print("\n", .{});
    }
};

