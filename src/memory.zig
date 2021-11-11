const std = @import("std");
const ds = @import("ds.zig");
const vm = @import("vm.zig");
const compiler = @import("compiler.zig");

const Allocator = std.mem.Allocator;
const Table = ds.Table;
const DynamicArray = ds.DynamicArray;

pub const FLOAT_PRECISION = 6;

const DEBUG_STRESS_GC = false;
const DEBUG_LOG_GC = true;

const GC_HEAP_GROW_FACTOR: usize = 2;

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
    class,
    instance,
    bound_method,
};

pub const Object = struct {
    typ: ObjectType,
    next: ?*Object,
    marked: bool,

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

    pub fn toClass(self: *Self) *Class {
        return @fieldParentPtr(Class, "base", self);
    }

    pub fn toInstance(self: *Self) *Instance {
        return @fieldParentPtr(Instance, "base", self);
    }

    pub fn toBoundMethod(self: *Self) *BoundMethod {
        return @fieldParentPtr(BoundMethod, "base", self);
    }

    pub fn deinit(self: *Self, allocator: *ObjectAllocator) void {
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} free {a}", .{ @ptrToInt(self), self.typ });
        }
        switch (self.typ) {
            .string => {
                const s = self.toString();
                allocator.bytes_allocated -= @sizeOf(String) + s.chars.len;
                allocator.allocator.free(s.chars);
                allocator.allocator.destroy(s);
            },
            .function => {
                const f = self.toFunction();
                f.chunk.deinit();
                allocator.bytes_allocated -= @sizeOf(Function);
                allocator.allocator.destroy(f);
            },
            .native => {
                allocator.allocator.destroy(self.toNative());
                allocator.bytes_allocated -= @sizeOf(Native);
            },
            .closure => {
                const closure = self.toClosure();
                closure.upvalues.deinit();
                allocator.bytes_allocated -= @sizeOf(Closure);
                allocator.allocator.destroy(closure);
            },
            .upvalue => {
                allocator.bytes_allocated -= @sizeOf(Upvalue);
                allocator.allocator.destroy(self.toUpvalue());
            },
            .class => {
                allocator.bytes_allocated -= @sizeOf(Class);
                const class = self.toClass();
                class.methods.deinit();
                allocator.allocator.destroy(class);
            },
            .instance => {
                allocator.bytes_allocated -= @sizeOf(Instance);
                self.toInstance().fields.deinit();
                allocator.allocator.destroy(self.toInstance());
            },
            .bound_method => {
                allocator.bytes_allocated -= @sizeOf(BoundMethod);
                allocator.allocator.destroy(self.toBoundMethod());
            },
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

pub const Class = struct {
    base: Object,
    name: *String,
    methods: Table,
};

pub const Instance = struct {
    base: Object,
    class: *Class,
    fields: Table,
};

pub const BoundMethod = struct {
    base: Object,
    recv: Value,
    method: *Closure,
};

pub const ObjectAllocator = struct {
    allocator: *Allocator,
    string_interner: Table,
    obj: ?*Object,
    vm: ?*vm.Vm,
    compiler: ?*compiler.Parser,
    grey_stack: DynamicArray(*Object),
    bytes_allocated: usize,
    next_gc: usize,

    const Self = @This();

    pub fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .obj = null,
            .string_interner = try Table.init(allocator),
            .vm = null,
            .compiler = null,
            .grey_stack = DynamicArray(*Object).init(allocator),
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };
    }

    fn create(self: *Self, comptime T: type) Allocator.Error!*T {
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
        if (self.bytes_allocated > self.next_gc)
            self.collectGarbage();

        const res = try self.allocator.create(T);
        if (DEBUG_LOG_GC)
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res), @sizeOf(T), @typeName(T) });

        self.bytes_allocated += @sizeOf(T);

        return res;
    }

    fn alloc(self: *Self, comptime T: type, count: usize) Allocator.Error![]T {
        if (DEBUG_STRESS_GC)
            self.collectGarbage();
        if (self.bytes_allocated > self.next_gc)
            self.collectGarbage();

        const res = try self.allocator.alloc(T, count);
        if (DEBUG_LOG_GC)
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res.ptr), @sizeOf(T) * count, @typeName(T) });

        self.bytes_allocated += @sizeOf(T) * count;

        return res;
    }

    fn dupe(self: *Self, comptime T: type, v: []const T) Allocator.Error![]T {
        if (DEBUG_STRESS_GC)
            self.collectGarbage();
        if (self.bytes_allocated > self.next_gc)
            self.collectGarbage();

        const res = try self.allocator.dupe(T, v);

        if (DEBUG_LOG_GC)
            std.log.debug("0x{X} alloc {} for {s}", .{ @ptrToInt(res.ptr), @sizeOf(T) * v.len, @typeName(T) });

        self.bytes_allocated += @sizeOf(T) * v.len;

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
        obj.base.marked = false;
        obj.chars = chars;
        // TODO: this function hashes twice, once in the string interner and
        // once below. We should only do it once if possible
        obj.hash = ds.hashBytes(chars);

        self.addObject(&obj.base);

        if (self.compiler != null or self.vm != null) {
            var stack = if (self.compiler != null) &self.compiler.?.tmp_stack else &self.vm.?.stack;
            stack.push(.{ .object = &obj.base });
        }

        _ = try self.string_interner.insert(obj, .nil);

        if (self.compiler != null or self.vm != null) {
            var stack = if (self.compiler != null) &self.compiler.?.tmp_stack else &self.vm.?.stack;
            _ = stack.pop();
        }

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
        f.base.marked = false;
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
        n.base.marked = false;
        n.function = func;
        self.addObject(&n.base);
        return n;
    }

    pub fn newClosure(self: *Self, func: *Function) !*Closure {
        var c = try self.create(Closure);
        c.base.next = null;
        c.base.typ = .closure;
        c.base.marked = false;
        c.function = func;
        c.upvalues = DynamicArray(*Upvalue).init(self.allocator);
        try c.upvalues.reserve(func.upvalue_count);
        self.addObject(&c.base);
        return c;
    }

    pub fn newUpvalue(self: *Self, value: *Value) !*Upvalue {
        var u = try self.create(Upvalue);
        u.base.next = null;
        u.base.typ = .upvalue;
        u.base.marked = false;
        u.location = value;
        u.next = null;
        u.closed = .nil;
        self.addObject(&u.base);
        return u;
    }

    pub fn newClass(self: *Self, name: *String) !*Class {
        const c = try self.create(Class);
        c.base.next = null;
        c.base.typ = .class;
        c.base.marked = false;
        c.name = name;
        c.methods = try Table.init(self.allocator);
        self.addObject(&c.base);
        return c;
    }

    pub fn newInstance(self: *Self, class: *Class) !*Instance {
        const i = try self.create(Instance);
        i.base.next = null;
        i.base.typ = .instance;
        i.base.marked = false;
        i.class = class;
        i.fields = try Table.init(self.allocator);
        self.addObject(&i.base);
        return i;
    }

    pub fn newBoundMethod(self: *Self, recv: Value, method: *Closure) !*BoundMethod {
        const m = try self.create(BoundMethod);
        m.base.next = null;
        m.base.typ = .bound_method;
        m.base.marked = false;
        m.recv = recv;
        m.method = method;
        self.addObject(&m.base);
        return m;
    }

    fn collectGarbage(self: *Self) void {
        if (DEBUG_LOG_GC) {
            std.log.debug("-- gc begin", .{});
        }

        self.markRoots();
        self.traceReferences();
        self.tableRemoveUnmarked(&self.string_interner);
        self.sweep();

        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (DEBUG_LOG_GC) {
            std.log.debug("-- gc end", .{});
        }
    }

    fn markRoots(self: *Self) void {
        if (self.vm) |machine| {
            var slot: [*]Value = &machine.stack.data;
            while (@ptrToInt(slot) < @ptrToInt(machine.stack.top)) : (slot += 1) {
                self.markValue(slot[0]);
            }

            var i: usize = 0;
            while (i < machine.frame_count) : (i += 1) {
                self.markObject(&machine.frames[i].closure.base);
            }

            self.markTable(&machine.globals);

            var upval = machine.open_upvalues;
            while (upval != null) : (upval = upval.?.next) {
                self.markObject(&upval.?.base);
            }
        }

        if (self.compiler) |c| {
            var current: ?*compiler.Parser = c;
            while (current) |curr| {
                self.markObject(&curr.function.base);

                var slot: [*]Value = &curr.tmp_stack.data;
                while (@ptrToInt(slot) < @ptrToInt(curr.tmp_stack.top)) : (slot += 1) {
                    self.markValue(slot[0]);
                }

                current = curr.enclosing;
            }
        }
    }

    fn markValue(self: *Self, v: Value) void {
        if (v == .object) {
            self.markObject(v.object);
        }
    }

    fn markObject(self: *Self, obj: ?*Object) void {
        const o = obj orelse return;
        if (o.marked) return;

        o.marked = true;
        self.grey_stack.append(o) catch unreachable;
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} mark", .{@ptrToInt(o)});
        }
    }

    fn markTable(self: *Self, table: *ds.Table) void {
        for (table.buckets) |*entry| {
            const key = if (entry.key != null) &entry.key.?.base else null;
            self.markObject(key);
            self.markValue(entry.value);
        }
    }

    fn traceReferences(self: *Self) void {
        while (self.grey_stack.items().len > 0) {
            const obj = self.grey_stack.pop_back();
            self.blackenObject(obj);
        }
    }

    fn blackenObject(self: *Self, obj: *Object) void {
        if (DEBUG_LOG_GC) {
            std.log.debug("0x{X} blacken", .{@ptrToInt(obj)});
        }
        switch (obj.typ) {
            .string, .native => {},
            .upvalue => self.markValue(obj.toUpvalue().closed),
            .function => {
                const f = obj.toFunction();
                if (f.name) |n|
                    self.markObject(&n.base);
                const items = f.chunk.values.items();
                for (items) |v| {
                    self.markValue(v);
                }
            },
            .closure => {
                const c = obj.toClosure();
                self.markObject(&c.function.base);
                const items = c.upvalues.items();
                for (items) |up| {
                    self.markObject(&up.base);
                }
            },
            .class => {
                const c = obj.toClass();
                self.markObject(&c.name.base);
                self.markTable(&c.methods);
            },
            .instance => {
                const i = obj.toInstance();
                self.markObject(&i.class.base);
                self.markTable(&i.fields);
            },
            .bound_method => {
                const m = obj.toBoundMethod();
                self.markObject(&m.method.base);
                self.markValue(m.recv);
            },
        }
    }

    fn tableRemoveUnmarked(self: *Self, table: *Table) void {
        _ = self;
        for (table.buckets) |*entry| {
            if (entry.key != null and !entry.key.?.base.marked)
                table.delete(entry.key.?);
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Object = null;
        var object = self.obj;
        while (object) |o| {
            if (o.marked) {
                previous = o;
                object = o.next;
                o.marked = false;
            } else {
                const unreached = o;
                object = o.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    self.obj = object;
                }
                unreached.deinit(self);
            }
        }
    }

    pub fn deinit(self: *Self) void {
        var obj = self.obj;
        while (obj) |o| {
            const next = o.next;
            o.deinit(self);
            obj = next;
        }
        self.obj = null;
        self.string_interner.deinit();
        self.grey_stack.deinit();
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
                .class => try writer.print("<class {s}>", .{o.toClass().name.chars}),
                .instance => try writer.print("{s} instance", .{o.toInstance().class.name.chars}),
                .bound_method => try writer.print("<fn {s}>", .{(o.toBoundMethod().method.function.name orelse unreachable).chars}),
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
