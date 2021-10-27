const std = @import("std");
const vm = @import("vm.zig");
const Allocator = std.mem.Allocator;

const INITIAL_TABLE_CAPACITY = 4;
const MAX_LOAD = 0.75;

pub const FRAMES_MAX = 64;

const MAX_STACK = FRAMES_MAX * 256;

pub const FLOAT_PRECISION = 6;

fn hashBytes(bytes: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (bytes) |b| {
        hash ^= b;
        _ = @mulWithOverflow(u32, hash, 16777619, &hash);
    }
    return hash;
}

pub const Type = enum {
    number,
    boolean,
    nil,
    object,
};

pub const ObjectType = enum {
    string,
    function,
};

pub const Object = struct {
    typ: ObjectType,
    next: ?*Object,

    const Self = @This();

    pub fn toString(self: *Self) *String {
        return @fieldParentPtr(String, "base", self);
    }

    pub fn toFunction(self: *Self) *Function {
        return @fieldParentPtr(Function, "base", self);
    }

    pub fn toZigString(self: *Self) []u8 {
        return self.toString().chars;
    }
};

pub const String = struct {
    base: Object,
    chars: []u8,
    hash: u32,
};

pub const Function = struct {
    base: Object,
    arity: u8,
    chunk: vm.Chunk,
    name: ?*String,
};

pub const ObjectAllocator = struct {
    allocator: *Allocator,
    string_interner: Table,
    obj: ?*Object,

    const Self = @This();

    pub fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .obj = null,
            .string_interner = try Table.init(allocator),
        };
    }

    fn addObject(self: *Self, obj: *Object) void {
        if (self.obj) |old| {
            obj.next = old;
            self.obj = obj;
        } else {
            self.obj = obj;
        }
    }

    pub fn takeString(self: *Self, chars: []u8) !*Object {
        const entry = self.string_interner.findString(chars);
        if (entry) |e| {
            self.allocator.free(chars);
            return &e.key.?.base;
        }

        var obj = try self.allocator.create(String);
        obj.base.next = null;
        obj.base.typ = .string;
        obj.chars = chars;
        // TODO: this function hashes twice, once in the string interner and
        // once below. We should only do it once if possible
        obj.hash = hashBytes(chars);
        self.addObject(&obj.base);
        _ = try self.string_interner.insert(obj, .nil);
        return &obj.base;
    }

    pub fn concatenateStrings(self: *Self, a: []const u8, b: []const u8) !*Object {
        const s = try self.allocator.alloc(u8, a.len + b.len);
        std.mem.copy(u8, s[0 .. a.len + 1], a);
        std.mem.copy(u8, s[a.len..], b);
        return self.takeString(s);
    }

    pub fn allocString(self: *Self, chars: []const u8) !*Object {
        const s = try self.allocator.dupe(u8, chars);
        return self.takeString(s);
    }

    pub fn newFunction(self: *Self) !*Function {
        var f = try self.allocator.create(Function);
        f.base.next = null;
        f.base.typ = .function;
        f.arity = 0;
        f.name = null;
        f.chunk = vm.Chunk.init(self.allocator);
        self.addObject(&f.base);
        return f;
    }

    pub fn deinit(self: *Self) void {
        var obj = self.obj;
        while (obj) |o| {
            const next = o.next;
            switch (o.typ) {
                .string => {
                    const s = o.toString();
                    self.allocator.free(s.chars);
                    self.allocator.destroy(s);
                },
                .function => {
                    const f = o.toFunction();
                    f.chunk.deinit();
                    self.allocator.destroy(f);
                },
            }
            obj = next;
        }
        self.obj = null;
        self.string_interner.deinit();
    }
};

pub const Value = union(Type) {
    number: f32,
    boolean: bool,
    nil: u0,
    object: *Object,

    const Self = @This();

    pub fn print(self: Self, writer: anytype) !void {
        switch (self) {
            .number => |n| try writer.print("{d:.[precision]}", .{ .number = n, .precision = FLOAT_PRECISION }),
            .nil => try writer.print("nil", .{}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .object => |o| switch (o.typ) {
                .string => try writer.print("\"{s}\"", .{o.toZigString()}),
                .function => {
                    var name =  if (o.toFunction().name) |n| n.chars else "script";
                    try writer.print("<fn {s}>", .{name});
                }
            },
        }
    }

    pub fn falsey(self: Self) bool {
        return switch (self) {
            .number, .object => false,
            .nil => true,
            .boolean => |b| !b,
        };
    }

    pub fn isString(self: Self) bool {
        return switch (self) {
            .object => true,
            else => false,
        };
    }

    pub fn toString(self: Self) *String {
        return self.object.toString();
    }

    pub fn toZigString(self: Self) []u8 {
        return self.toString().chars;
    }
};

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

    fn findString(self: *const Self, key: []const u8) ?*Entry {
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
    var object_allocator = try ObjectAllocator.init(std.testing.allocator);
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
