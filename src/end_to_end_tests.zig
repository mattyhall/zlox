const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const ds = @import("ds.zig");
const Parser = @import("compiler.zig").Parser;

fn run(allocator: *ds.ObjectAllocator, src: []const u8) ds.Value {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(allocator, src);
    _ = parser.compile(&chunk) catch unreachable;

    var v = vm.Vm.init(allocator);
    const value = v.interpret(&chunk) catch unreachable;
    return value;
}

fn fails(allocator: *ds.ObjectAllocator, src: []const u8) !void {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(allocator, src);
    _ = parser.compile(&chunk) catch unreachable;

    var v = vm.Vm.init(allocator);
    try std.testing.expectError(vm.Vm.Error.runtime_error, v.interpret(&chunk));
}

test "literals" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    try std.testing.expectEqual(ds.Value{ .number = -2 }, run(&alloc, "-2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "true"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "false"));
    try std.testing.expectEqual(ds.Type.nil, @as(ds.Type, run(&alloc, "nil")));

    try std.testing.expectEqualStrings("HELLO", run(&alloc, "\"HELLO\"").toZigString());
}

test "sums" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // Numbers
    try std.testing.expectEqual(ds.Value{ .number = 10.0 }, run(&alloc, "3 + 7"));
    try std.testing.expectEqual(ds.Value{ .number = -4.0 }, run(&alloc, "3 - 7"));
    try std.testing.expectEqual(ds.Value{ .number = 9.0 }, run(&alloc, "3 * (1+2)"));
    try std.testing.expectEqual(ds.Value{ .number = 8.0 }, run(&alloc, "-2 + 7 + 3"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run(&alloc, "8 + 12 * -2 / 3"));

    try fails(&alloc, "false + false");
    try fails(&alloc, "true * true");
    try fails(&alloc, "nil - nil");
    try fails(&alloc, "8 + 2 * false");

    // Strings
    try std.testing.expectEqualStrings("HELLOWORLD", run(&alloc, "\"HELLO\" + \"WORLD\"").toZigString());
    try std.testing.expectEqualStrings("HELLO WORLD", run(&alloc, "\"HELLO\" + \" \" + \"WORLD\"").toZigString());

    try fails(&alloc, "\"HELLO\" + 2");
}

test "boolean logic" {
    var alloc = try ds.ObjectAllocator.init(std.testing.allocator);
    defer alloc.deinit();

    // Not
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "!1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "!-1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "!42.0"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "!true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "!false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "!nil"));

    // (In)Equality
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1 == 1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "-1 == -1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1.123 == 1.123"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "true == true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "false == false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "!false == !false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "!true == !true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "nil == nil"));

    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1 != 5"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1.1 != 1.2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "true != false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "!true != !false"));

    // Gt/lt
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1.23 < 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1.23 <= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run(&alloc, "1 <= 2"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "1.23 > 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "1.23 >= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run(&alloc, "1 >= 2"));

    // Errors
    try fails(&alloc, "1 > false");
    try fails(&alloc, "nil < nil");
}
