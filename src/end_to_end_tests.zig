const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const ds = @import("ds.zig");
const memory = @import("memory.zig");
const Value = memory.Value;
const Parser = @import("compiler.zig").Parser;

fn runExpr(allocator: *memory.ObjectAllocator, comptime src: []const u8) Value {
    return run(allocator, "return " ++ src ++ ";");
}

fn failsExpr(allocator: *memory.ObjectAllocator, comptime src: []const u8) !void {
    return fails(allocator, "return " ++ src ++ ";");
}

fn run(allocator: *memory.ObjectAllocator, src: []const u8) Value {
    allocator.deinit();
    allocator.* = memory.ObjectAllocator.init(std.testing.allocator) catch unreachable;

    var parser = Parser.init(allocator, src, .script) catch unreachable;
    var func = (parser.compile() catch unreachable) orelse unreachable;

    var v = vm.Vm.init(allocator) catch unreachable;
    defer v.deinit();

    const value = v.interpret(func) catch unreachable;
    return value;
}

fn fails(allocator: *memory.ObjectAllocator, src: []const u8) !void {
    allocator.deinit();
    allocator.* = memory.ObjectAllocator.init(std.testing.allocator) catch unreachable;

    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(allocator, src, .script) catch unreachable;
    var func = parser.compile() catch unreachable;
    if (func == null) return;

    var v = try vm.Vm.init(allocator);
    defer v.deinit();

    try std.testing.expectError(vm.Vm.Error.runtime_error, v.interpret(func.?));
}

test "literals" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(Value{ .number = -2 }, runExpr(&alloc, "-2"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "true"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "false"));
    try std.testing.expectEqual(memory.Type.nil, @as(memory.Type, runExpr(&alloc, "nil")));

    try std.testing.expectEqualStrings("HELLO", runExpr(&alloc, "\"HELLO\"").toZigString());
}
test "sums" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();
    // Numbers
    try std.testing.expectEqual(Value{ .number = 10.0 }, runExpr(&alloc, "3 + 7"));
    try std.testing.expectEqual(Value{ .number = -4.0 }, runExpr(&alloc, "3 - 7"));
    try std.testing.expectEqual(Value{ .number = 9.0 }, runExpr(&alloc, "3 * (1+2)"));
    try std.testing.expectEqual(Value{ .number = 8.0 }, runExpr(&alloc, "-2 + 7 + 3"));
    try std.testing.expectEqual(Value{ .number = 0.0 }, runExpr(&alloc, "8 + 12 * -2 / 3")); //
    try failsExpr(&alloc, "false + false");
    try failsExpr(&alloc, "true * true");
    try failsExpr(&alloc, "nil - nil");
    try failsExpr(&alloc, "8 + 2 * false");
    // Strings
    try std.testing.expectEqualStrings("HELLOWORLD", runExpr(&alloc, "\"HELLO\" + \"WORLD\"").toZigString());
    try std.testing.expectEqualStrings("HELLO WORLD", runExpr(&alloc, "\"HELLO\" + \" \" + \"WORLD\"").toZigString()); //
    try failsExpr(&alloc, "\"HELLO\" + 2");
}

test "boolean logic" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // Not
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "!1"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "!-1"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "!42.0"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "!true"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "!false"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "!nil"));

    // (In)Equality
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1 == 1"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "-1 == -1"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1.123 == 1.123"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "true == true"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "false == false"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "!false == !false"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "!true == !true"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "nil == nil"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "\"hi\" == \"hi\""));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1 != 5"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1.1 != 1.2"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "true != false"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "!true != !false"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "\"hi\" != \"hello\""));

    // Gt/lt
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1.23 < 2.23"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1.23 <= 2.23"));
    try std.testing.expectEqual(Value{ .boolean = true }, runExpr(&alloc, "1 <= 2"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "1.23 > 2.23"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "1.23 >= 2.23"));
    try std.testing.expectEqual(Value{ .boolean = false }, runExpr(&alloc, "1 >= 2"));

    // Errors
    try failsExpr(&alloc, "1 > false");
    try failsExpr(&alloc, "nil < nil");
}

test "basic statements" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    _ = run(&alloc, "1;");
    _ = run(&alloc, "1 + 7 * 2;");
    _ = run(&alloc, "print \"hi\";");
}

test "global assignment" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(memory.Type.nil, run(&alloc, "var a; return a;"));
    try std.testing.expectEqual(Value{ .number = 10.0 }, run(&alloc, "var a = 10; return a;"));
    try std.testing.expectEqual(Value{ .number = 22.0 }, run(&alloc, "var a = 10 * 2 + 7 - 5; return a;"));
    try std.testing.expectEqual(Value{ .boolean = true }, run(&alloc, "var a = true; return a;"));
    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc, "var a = 10; a = 0; return a;"));
    try std.testing.expectEqualStrings("hi", run(&alloc, "var a = \"hi\"; return a;").toZigString());
    try std.testing.expectEqualStrings("hello, world", run(&alloc, "var a = \"hello\"; a = a + \", world\"; return a;").toZigString());

    try fails(&alloc, "a = 10;");
    try fails(&alloc, "1 * 2 = 3 + 4;");
}

test "local assignment" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(memory.Type.nil, run(&alloc, "{ var a; return a; }"));
    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc, "{ var a = true; a = 0; return a; }"));
    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc,
        \\ {
        \\   var a = true;
        \\   {
        \\     var a = 0;
        \\     return a;
        \\   }
        \\ }
    ));
    try std.testing.expectEqual(Value{ .boolean = true }, run(&alloc,
        \\ {
        \\   var a = true;
        \\   {
        \\     var a = 0;
        \\   }
        \\   return a;
        \\ }
    ));
    try std.testing.expectEqualStrings("good morning", run(&alloc,
        \\ {
        \\   var res;
        \\   var adjective = "good";
        \\   {
        \\     var time_of_day = "morning";
        \\     res = adjective + " " + time_of_day;
        \\   }
        \\   return res;
        \\ }
    ).toZigString());

    try fails(&alloc, "{ var a = 10; var a = 20; }");
    try fails(&alloc,
        \\ {
        \\   {
        \\     var a = 10;
        \\   }
        \\   return a;
        \\ }
    );
    try fails(&alloc,
        \\ {
        \\   var a = "outer";
        \\   {
        \\     var a = a;
        \\   }
        \\ }
    );
}
test "ifs" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(Value{ .number = 1.0 }, run(&alloc,
        \\ if (false) return 0.0;
        \\ return 1.0;
    ));
    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc,
        \\ if (true) return 0.0;
        \\ return 1.0;
    ));

    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc,
        \\ if (true) return 0.0;
        \\ else return 1.0;
    ));
    try std.testing.expectEqual(Value{ .number = 1.0 }, run(&alloc,
        \\ if (false) return 0.0;
        \\ else return 1.0;
    ));

    const body =
        \\ if (a == 0.0) return 0.0;
        \\ else if (a == 1.0) return 1.0;
        \\ else return 2.0;
    ;

    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc, "var a = 0.0;" ++ body));
    try std.testing.expectEqual(Value{ .number = 1.0 }, run(&alloc, "var a = 1.0;" ++ body));
    try std.testing.expectEqual(Value{ .number = 2.0 }, run(&alloc, "var a = 2.0;" ++ body));
}

test "while" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(Value{ .number = 10.0 }, run(&alloc,
        \\ var a = 10.0;
        \\ while (a < 10) a = a + 1;
        \\ return a;
    ));

    try std.testing.expectEqual(Value{ .number = 1.0 }, run(&alloc,
        \\ var a = 1.0;
        \\ while (a < 1) a = a + 1;
        \\ return a;
    ));

    try std.testing.expectEqual(Value{ .number = 0.0 }, run(&alloc,
        \\ var a = 30.0;
        \\ while (a > 0) {
        \\   var b = 1;
        \\   a = a - b;
        \\ }
        \\ return a;
    ));
}

test "for" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(Value{ .number = 55.0 }, run(&alloc,
        \\ var count = 0;
        \\ for (var i = 1; i <= 10; i = i + 1) {
        \\   count = count + i;
        \\ }
        \\ return count;
    ));
}

test "fun" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    _ = run(&alloc,
        \\ fun foo() {
        \\   print "hello";
        \\ }
        \\ foo();
        \\ print "world";
    );
    try std.testing.expectEqualStrings("hello world", run(&alloc,
        \\ fun foo() {
        \\   return "hello";
        \\ }
        \\ return foo() + " world";
    ).toZigString());
    try std.testing.expectEqual(Value{ .number = 10.0 }, run(&alloc,
        \\ var x = 10;
        \\ fun foo() {
        \\   var y = 2;
        \\   return x * 2;
        \\ }
        \\ return foo() / 2;
    ));
    try std.testing.expectEqual(Value{ .number = 20.0 }, run(&alloc,
        \\ fun multiply(a, sf) {
        \\   return a * sf;
        \\ }
        \\ return multiply(10, 2);
    ));

    try fails(&alloc,
        \\ fun foo(a) {
        \\ }
        \\ foo();
    );
    try fails(&alloc,
        \\ fun foo(a) {
        \\ }
        \\ foo(10, 20);
    );
}

test "closure" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    const range =
        \\ fun range(start, end) {
        \\   var current = start;
        \\   fun closure() {
        \\     if (current >= end) return nil;
        \\     var tmp = current;
        \\     current = current + 1;
        \\     return tmp;
        \\   }
        \\   return closure;
        \\ }
    ;

    try std.testing.expectEqual(Value{ .number = 3.0 }, run(&alloc, range ++
        \\ var gen = range(1, 10);
        \\ gen();
        \\ gen();
        \\ return gen();
    ));
    try std.testing.expectEqual(memory.Type.nil, @as(memory.Type, run(&alloc, range ++
        \\ var gen = range(1, 3);
        \\ gen();
        \\ gen();
        \\ return gen();
    )));
    try std.testing.expectEqual(Value{ .number = 6.0 }, run(&alloc, range ++
        \\ var gen = range(1, 4);
        \\ var count = 0;
        \\ var i = 0;
        \\ while (i = gen()) {
        \\   count = count + i;
        \\ }
        \\ return count;
    ));

    try std.testing.expectEqual(Value{ .number = 3.0 }, run(&alloc,
        \\ fun outer() {
        \\   var a = 1;
        \\   fun middle() {
        \\     a = a + 1;
        \\     fun inner() {
        \\       a = a + 1;
        \\       return a;
        \\     }
        \\     return inner;
        \\   }
        \\   return middle;
        \\ }
        \\ var middle = outer();
        \\ var inner = middle();
        \\ return inner();
    ));
}

test "class fields/methods" {
    var alloc = try memory.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class A {}
        \\ class B {}
        \\ var a = A();
        \\ a.b = B();
        \\ a.b.c = 42;
        \\ return a.b.c;
    ));
    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class A {
        \\   add(a, b) {
        \\     return a + b;
        \\   }
        \\ }
        \\ var a = A();
        \\ return a.add(30, 12);
    ));
    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class AddN {
        \\   add(a) {
        \\     return a + this.n;
        \\   }
        \\ }
        \\ var a = AddN();
        \\ a.n = 30;
        \\ return a.add(12);
    ));
    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class AddN {
        \\   add(a) {
        \\     fun inner() {
        \\       return a + this.n;
        \\     }
        \\     return inner();
        \\   }
        \\ }
        \\ var a = AddN();
        \\ a.n = 30;
        \\ return a.add(12);
    ));
    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class AddN {
        \\   init(n) {
        \\     this.n = n;
        \\   }
        \\   add(a) {
        \\     return a + this.n;
        \\   }
        \\ }
        \\ var a = AddN(30);
        \\ return a.add(12);
    ));
    try std.testing.expectEqual(Value{ .number = 42 }, run(&alloc,
        \\ class AddN {
        \\   init() {
        \\     fun add(a, b) {
        \\       return a + b;
        \\     }
        \\     this.add = add;
        \\   }
        \\ }
        \\ var a = AddN();
        \\ return a.add(30, 12);
    ));

    try fails(&alloc,
        \\ class A {};
        \\ return a.foo;
    );
    try fails(&alloc,
        \\ class A {};
        \\ return a.foo();
    );
    try fails(&alloc,
        \\ class A {
        \\   foo(a) {}
        \\ }
        \\ return a.foo();
    );
}
