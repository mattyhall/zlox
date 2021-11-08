const std = @import("std");
const ds = @import("ds.zig");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Table = ds.Table;
const DynamicArray = ds.DynamicArray;

pub const FLOAT_PRECISION = 6;

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
        obj.hash = ds.hashBytes(chars);
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
        f.upvalue_count = 0;
        f.chunk = vm.Chunk.init(self.allocator);
        self.addObject(&f.base);
        return f;
    }

    pub fn newNative(self: *Self, func: NativeFn) !*Native {
        var n = try self.allocator.create(Native);
        n.base.next = null;
        n.base.typ = .native;
        n.function = func;
        self.addObject(&n.base);
        return n;
    }

    pub fn newClosure(self: *Self, func: *Function) !*Closure {
        var c = try self.allocator.create(Closure);
        c.base.next = null;
        c.base.typ = .closure;
        c.function = func;
        c.upvalues = DynamicArray(*Upvalue).init(self.allocator);
        self.addObject(&c.base);
        return c;
    }

    pub fn newUpvalue(self: *Self, value: *Value) !*Upvalue {
        var u = try self.allocator.create(Upvalue);
        u.base.next = null;
        u.base.typ = .upvalue;
        u.location = value;
        u.next = null;
        u.closed = .nil;
        self.addObject(&u.base);
        return u;
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
                .native => self.allocator.destroy(o.toNative()),
                .closure => {
                    const closure = o.toClosure();
                    closure.upvalues.deinit();
                    self.allocator.destroy(closure);
                },
                .upvalue => self.allocator.destroy(o.toUpvalue()),
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

