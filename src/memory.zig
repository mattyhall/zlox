const std = @import("std");
const ds = @import("ds.zig");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Table = ds.Table;
const DynamicArray = ds.DynamicArray;

pub const FLOAT_PRECISION = 6;

const DEBUG_STRESS_GC = true;
const DEBUG_LOG_GC = true;

pub const Type = enum {
    number,
    boolean,
    nil,
    object,
};

pub const ObjectType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
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

    pub fn toNative(self: *Self) *Native {
        return @fieldParentPtr(Native, "base", self);
    }

    pub fn toClosure(self: *Self) *Closure {
        return @fieldParentPtr(Closure, "base", self);
    }

    pub fn toUpvalue(self: *Self) *Upvalue {
        return @fieldParentPtr(Upvalue, "base", self);
    }

    pub fn toZigString(self: *Self) []u8 {
        return self.toString().chars;
    }

    pub fn deinit(self: *Self, allocator: *Allocator) void {
        std.log.debug("0x{X} free {a}", .{ @ptrToInt(self), self.typ });
        switch (self.typ) {
            .string => {
                const s = self.toString();
                allocator.free(s.chars);
                allocator.destroy(s);
            },
            .function => {
                const f = self.toFunction();
                f.chunk.deinit();
                allocator.destroy(f);
            },
            .native => allocator.destroy(self.toNative()),
            .closure => {
                const closure = self.toClosure();
                closure.upvalues.deinit();
                allocator.destroy(closure);
            },
            .upvalue => allocator.destroy(self.toUpvalue()),
        }
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
    upvalue_count: u8,
    chunk: vm.Chunk,
    name: ?*String,
};

pub const Closure = struct {
    base: Object,
    function: *Function,
    upvalues: DynamicArray(*Upvalue),
};

pub const NativeFn = fn (arg_count: u8, args: [*]Value) Value;

pub const Native = struct {
    base: Object,
    function: NativeFn,
};

pub const Upvalue = struct {
    base: Object,
    location: *Value,
    next: ?*Upvalue,
    closed: Value,
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

    fn create(self: *Self, comptime T: type) Allocator.Error!*T {
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
        const res = try self.allocator.create(T);
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res), @sizeOf(T), @typeName(T) });
        }
        return res;
    }

    fn alloc(self: *Self, comptime T: type, count: usize) Allocator.Error![]T {
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
        const res = try self.allocator.alloc(T, count);
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res.ptr), @sizeOf(T) * count, @typeName(T) });
        }
        return res;
    }

    fn dupe(self: *Self, comptime T: type, v: []const T) Allocator.Error![]T {
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
        const res = try self.allocator.dupe(T, v);
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res.ptr), @sizeOf(T), @typeName(T) });
        }
        return res;
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

        var obj = try self.create(String);
        obj.base.next = null;
        obj.base.typ = .string;
        obj.chars = chars;
        // TODO: this function hashes twice, once in the string interner and
        // once below. We should only do it once if possible
        obj.hash = ds.hashBytes(chars);
        self.addObject(&obj.base);
        _ = try self.string_interner.insert(obj, .nil);
        return &obj.base;
    }

    pub fn concatenateStrings(self: *Self, a: []const u8, b: []const u8) !*Object {
        const s = try self.alloc(u8, a.len + b.len);
        std.mem.copy(u8, s[0 .. a.len + 1], a);
        std.mem.copy(u8, s[a.len..], b);
        return self.takeString(s);
    }

    pub fn allocString(self: *Self, chars: []const u8) !*Object {
        const s = try self.dupe(u8, chars);
        return self.takeString(s);
    }

    pub fn newFunction(self: *Self) !*Function {
        var f = try self.create(Function);
        f.base.next = null;
        f.base.typ = .function;
        f.arity = 0;
        f.name = null;
        f.upvalue_count = 0;
        f.chunk = vm.Chunk.init(self.allocator);
        self.addObject(&f.base);
        return f;
    }

    pub fn newNative(self: *Self, func: NativeFn) !*Native {
        var n = try self.create(Native);
        n.base.next = null;
        n.base.typ = .native;
        n.function = func;
        self.addObject(&n.base);
        return n;
    }

    pub fn newClosure(self: *Self, func: *Function) !*Closure {
        var c = try self.create(Closure);
        c.base.next = null;
        c.base.typ = .closure;
        c.function = func;
        c.upvalues = DynamicArray(*Upvalue).init(self.allocator);
        self.addObject(&c.base);
        return c;
    }

    pub fn newUpvalue(self: *Self, value: *Value) !*Upvalue {
        var u = try self.create(Upvalue);
        u.base.next = null;
        u.base.typ = .upvalue;
        u.location = value;
        u.next = null;
        u.closed = .nil;
        self.addObject(&u.base);
        return u;
    }

    fn collectGarbage(self: *Self) void {
        _ = self;
        if (DEBUG_LOG_GC) {
            std.log.debug("-- gc begin", .{});
        }

        if (DEBUG_LOG_GC) {
            std.log.debug("-- gc end", .{});
        }
    }

    pub fn deinit(self: *Self) void {
        var obj = self.obj;
        while (obj) |o| {
            const next = o.next;
            o.deinit(self.allocator);
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
                    var name = if (o.toFunction().name) |n| n.chars else "script";
                    try writer.print("<fn {s}>", .{name});
                },
                .native => try writer.print("<native fn>", .{}),
                .closure => {
                    var name = if (o.toClosure().function.name) |n| n.chars else "script";
                    try writer.print("<fn {s}>", .{name});
                },
                .upvalue => try writer.print("upvalue", .{}),
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
