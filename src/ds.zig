const std = @import("std");
const vm = @import("vm.zig");
const memory = @import("memory.zig");

const Allocator = std.mem.Allocator;
const String = memory.String;
const Value = memory.Value;

const INITIAL_TABLE_CAPACITY = 4;
const MAX_LOAD = 0.75;

pub const FRAMES_MAX = 64;

const MAX_STACK = FRAMES_MAX * 256;

pub fn hashBytes(bytes: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (bytes) |b| {
        hash ^= b;
        _ = @mulWithOverflow(u32, hash, 16777619, &hash);
    }
    return hash;
}

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

        pub fn reserve(self: *Self, n: usize) !void {
            if (self.capacity == 0) {
                self.data = (try self.allocator.alloc(T, n)).ptr;
            } else {
                self.data = (try self.allocator.realloc(self.items(), n)).ptr;
            }
            self.capacity = n;
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

    pub fn peek(self: *const Self, n: usize) Value {
        const ptr = self.top - 1 - n;
        return ptr[0];
    }

    pub fn debugPrint(self: *Self, stdout: anytype) !void {
        var current: [*]Value = &self.data;
        if (current == self.top) return;
        try stdout.print("    ", .{});
        while (current != self.top) : (current += 1) {
            try stdout.print("[", .{});
            try current[0].print(stdout);
            try stdout.print("] ", .{});
        }
        try stdout.print("\n", .{});
    }
};

pub const Entry = struct {
    key: ?*String,
    value: Value,

    const Self = @This();

    fn tombstone(self: *const Self) bool {
        return self.key == null and self.value == .boolean;
    }

    fn empty(self: *const Self) bool {
        return self.key == null and self.value == .nil;
    }
};

pub const Table = struct {
    allocator: *Allocator,
    count: usize,
    buckets: []Entry,

    const Self = @This();

    pub fn init(allocator: *Allocator) !Self {
        var buckets = try allocator.alloc(Entry, INITIAL_TABLE_CAPACITY);
        for (buckets) |*entry| {
            entry.key = null;
            entry.value = .nil;
        }

        return Self{
            .allocator = allocator,
            .count = 0,
            .buckets = buckets,
        };
    }

    fn findEntry(self: *const Self, key: *String) *Entry {
        var index = key.hash % self.buckets.len;
        var first_tombstone: ?*Entry = null;
        while (true) {
            const entry = &self.buckets[index];

            if (entry.key == key) return entry;

            if (entry.tombstone() and first_tombstone == null) {
                first_tombstone = entry;
            }

            if (entry.empty()) {
                return if (first_tombstone) |t| t else entry;
            }

            index = (index + 1) % self.buckets.len;
        }
        unreachable;
    }

    pub fn findString(self: *const Self, key: []const u8) ?*Entry {
        const hash = hashBytes(key);
        var index = hash % self.buckets.len;
        while (true) {
            const entry = &self.buckets[index];

            if (entry.empty()) return null;

            if (entry.key) |k| {
                if (k.hash == hash and k.chars.len == key.len and std.mem.eql(u8, k.chars, key))
                    return entry;
            }

            index = (index + 1) % self.buckets.len;
        }

        unreachable;
    }

    pub fn find(self: *const Self, key: *String) ?*Entry {
        const entry = self.findEntry(key);
        if (entry.key == null) return null;
        return entry;
    }

    fn realloc(self: *Self) !void {
        var new_buckets = try self.allocator.alloc(Entry, self.buckets.len * 2);
        for (new_buckets) |*entry| {
            entry.key = null;
            entry.value = .nil;
        }

        const old_buckets = self.buckets;

        self.buckets = new_buckets;
        self.count = 0;

        for (old_buckets) |entry| {
            if (entry.key) |k|
                _ = try self.insertUnchecked(k, entry.value);
        }

        self.allocator.free(old_buckets);
    }

    fn insertUnchecked(self: *Self, key: *String, value: Value) !bool {
        var entry = self.findEntry(key);
        var exists = entry.key != null;
        if (entry.key == null) {
            self.count += 1;
            entry.key = key;
        }
        entry.value = value;
        return exists;
    }

    pub fn insert(self: *Self, key: *String, value: Value) !bool {
        if (@intToFloat(f32, self.count + 1) / @intToFloat(f32, self.buckets.len) > MAX_LOAD)
            try self.realloc();

        return try self.insertUnchecked(key, value);
    }

    pub fn delete(self: *Self, key: *String) void {
        var entry = self.findEntry(key);
        if (entry.key != null) {
            entry.key = null;
            entry.value = .{ .boolean = true };
        }
    }

    pub fn print(self: *const Self) !void {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{{", .{});
        for (self.buckets) |entry| {
            if (entry.key) |key| {
                try stdout.print("\"{s}\" => ", .{key.chars});
                try entry.value.print(stdout);
                try stdout.print(", ", .{});
            }
        }
        try stdout.print("}}\n", .{});
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.buckets);
    }
};

test "table" {
    var object_allocator = try memory.ObjectAllocator.init(std.testing.allocator);
    defer object_allocator.deinit();
    var table = try Table.init(std.testing.allocator);
    defer table.deinit();

    var i: usize = 0;
    while (i < 10) : (i += 1) {
        const s = try std.fmt.allocPrint(std.testing.allocator, "{}", .{i});
        const o = try object_allocator.takeString(s);
        _ = try table.insert(o.toString(), .{ .number = @intToFloat(f32, i) });
    }

    i = 0;

    while (i < 10) : (i += 1) {
        const s = try std.fmt.allocPrint(std.testing.allocator, "{}", .{i});
        const o = try object_allocator.takeString(s);
        const e = table.find(o.toString());
        try std.testing.expect(e != null);
        try std.testing.expectEqual(e.?.value, .{ .number = @intToFloat(f32, i) });
    }

    i = 0;
    while (i < 10) : (i += 2) {
        const s = try std.fmt.allocPrint(std.testing.allocator, "{}", .{i});
        const o = try object_allocator.takeString(s);
        table.delete(o.toString());
    }

    i = 0;
    while (i < 10) : (i += 1) {
        const s = try std.fmt.allocPrint(std.testing.allocator, "{}", .{i});
        const o = try object_allocator.takeString(s);
        const e = table.find(o.toString());
        if (i % 2 == 0) {
            try std.testing.expectEqual(e, null);
        } else {
            try std.testing.expect(e != null);
            try std.testing.expectEqual(e.?.value, .{ .number = @intToFloat(f32, i) });
        }
    }
}
