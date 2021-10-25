const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const ds = @import("ds.zig");
const Parser = @import("compiler.zig").Parser;

fn runExpr(allocator: *ds.ObjectAllocator, comptime src: []const u8) ds.Value {
    return run(allocator, "return " ++ src ++ ";");
}

fn failsExpr(allocator: *ds.ObjectAllocator, comptime src: []const u8) !void {
    return fails(allocator, "return " ++ src ++ ";");
}

fn run(allocator: *ds.ObjectAllocator, src: []const u8) ds.Value {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(allocator, src);
    _ = parser.compile(&chunk) catch unreachable;

    var v = vm.Vm.init(allocator) catch unreachable;
    defer v.deinit();

    const value = v.interpret(&chunk) catch unreachable;
    return value;
}

fn fails(allocator: *ds.ObjectAllocator, src: []const u8) !void {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(allocator, src);
    if (parser.compile(&chunk) catch unreachable) return;

    var v = try vm.Vm.init(allocator);
    defer v.deinit();

    try std.testing.expectError(vm.Vm.Error.runtime_error, v.interpret(&chunk));
}

test "literals" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(ds.Value{ .number = -2 }, runExpr(&alloc, "-2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "true"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "false"));
    try std.testing.expectEqual(ds.Type.nil, @as(ds.Type, runExpr(&alloc, "nil")));

    try std.testing.expectEqualStrings("HELLO", runExpr(&alloc, "\"HELLO\"").toZigString());
}

test "sums" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // Numbers
    try std.testing.expectEqual(ds.Value{ .number = 10.0 }, runExpr(&alloc, "3 + 7"));
    try std.testing.expectEqual(ds.Value{ .number = -4.0 }, runExpr(&alloc, "3 - 7"));
    try std.testing.expectEqual(ds.Value{ .number = 9.0 }, runExpr(&alloc, "3 * (1+2)"));
    try std.testing.expectEqual(ds.Value{ .number = 8.0 }, runExpr(&alloc, "-2 + 7 + 3"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, runExpr(&alloc, "8 + 12 * -2 / 3"));

    try failsExpr(&alloc, "false + false");
    try failsExpr(&alloc, "true * true");
    try failsExpr(&alloc, "nil - nil");
    try failsExpr(&alloc, "8 + 2 * false");

    // Strings
    try std.testing.expectEqualStrings("HELLOWORLD", runExpr(&alloc, "\"HELLO\" + \"WORLD\"").toZigString());
    try std.testing.expectEqualStrings("HELLO WORLD", runExpr(&alloc, "\"HELLO\" + \" \" + \"WORLD\"").toZigString());

    try failsExpr(&alloc, "\"HELLO\" + 2");
}

test "boolean logic" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // Not
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "!1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "!-1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "!42.0"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "!true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "!false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "!nil"));

    // (In)Equality
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1 == 1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "-1 == -1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1.123 == 1.123"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "true == true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "false == false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "!false == !false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "!true == !true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "nil == nil"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "\"hi\" == \"hi\""));

    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1 != 5"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1.1 != 1.2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "true != false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "!true != !false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "\"hi\" != \"hello\""));

    // Gt/lt
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1.23 < 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1.23 <= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, runExpr(&alloc, "1 <= 2"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "1.23 > 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "1.23 >= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, runExpr(&alloc, "1 >= 2"));

    // Errors
    try failsExpr(&alloc, "1 > false");
    try failsExpr(&alloc, "nil < nil");
}

test "basic statements" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // return nil so that it terminates
    _ = run(&alloc, "1;            return nil;");
    _ = run(&alloc, "1 + 7 * 2;    return nil;");
    _ = run(&alloc, "print \"hi\"; return nil");
}

test "global assignment" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(ds.Type.nil, run(&alloc, "var a; return a;"));
    try std.testing.expectEqual(ds.Value{ .number = 10.0 }, run(&alloc, "var a = 10; return a;"));
    try std.testing.expectEqual(ds.Value{ .number = 22.0 }, run(&alloc, "var a = 10 * 2 + 7 - 5; return a;"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "var a = true; return a;"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc, "var a = 10; a = 0; return a;"));

    try std.testing.expectEqualStrings("hi", run(&alloc, "var a = \"hi\"; return a;").toZigString());
    try std.testing.expectEqualStrings("hello, world", run(&alloc, "var a = \"hello\"; a = a + \", world\"; return a;").toZigString());

    try fails(&alloc, "a = 10;");
    try fails(&alloc, "1 * 2 = 3 + 4;");
}

test "local assignment" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(ds.Type.nil, run(&alloc, "{ var a; return a; }"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc, "{ var a = true; a = 0; return a; }"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc,
        \\ {
        \\   var a = true;
        \\   {
        \\     var a = 0;
        \\     return a;
        \\   }
        \\ }
    ));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc,
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
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(ds.Value{ .number = 1.0 }, run(&alloc,
        \\ if (false) return 0.0;
        \\ return 1.0;
    ));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc,
        \\ if (true) return 0.0;
        \\ return 1.0;
    ));

    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc,
        \\ if (true) return 0.0;
        \\ else return 1.0;
    ));
    try std.testing.expectEqual(ds.Value{ .number = 1.0 }, run(&alloc,
        \\ if (false) return 0.0;
        \\ else return 1.0;
    ));

    const body =
        \\ if (a == 0.0) return 0.0;
        \\ else if (a == 1.0) return 1.0;
        \\ else return 2.0;
    ;

    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc, "var a = 0.0;" ++ body));
    try std.testing.expectEqual(ds.Value{ .number = 1.0 }, run(&alloc, "var a = 1.0;" ++ body));
    try std.testing.expectEqual(ds.Value{ .number = 2.0 }, run(&alloc, "var a = 2.0;" ++ body));
}
