const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const ds = @import("ds.zig");
const Parser = @import("compiler.zig").Parser;

fn run(src: []const u8) ds.Value {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(src);
    _ = parser.compile(&chunk) catch unreachable;

    var v = vm.Vm.init();
    const value = v.interpret(&chunk) catch unreachable;
    return value;
}

fn fails(src: []const u8) !void {
    var chunk = vm.Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    var parser = Parser.init(src);
    _ = parser.compile(&chunk) catch unreachable;

    var v = vm.Vm.init();
    try std.testing.expectError(vm.Vm.Error.runtime_error, v.interpret(&chunk));
}

test "literals" {
    try std.testing.expectEqual(ds.Value{ .number = -2 }, run("-2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("true"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("false"));
    try std.testing.expectEqual(ds.Type.nil, @as(ds.Type, run("nil")));
}

test "sums" {
    try std.testing.expectEqual(ds.Value{ .number = 10.0 }, run("3 + 7"));
    try std.testing.expectEqual(ds.Value{ .number = -4.0 }, run("3 - 7"));
    try std.testing.expectEqual(ds.Value{ .number = 9.0 }, run("3 * (1+2)"));
    try std.testing.expectEqual(ds.Value{ .number = 8.0 }, run("-2 + 7 + 3"));
    try std.testing.expectEqual(ds.Value{ .number = 0.0 }, run("8 + 12 * -2 / 3"));

    try fails("false + false");
    try fails("true * true");
    try fails("nil - nil");
    try fails("8 + 2 * false");
}

test "boolean logic" {
    // Not
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("!1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("!-1"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("!42.0"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("!true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("!false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("!nil"));

    // (In)Equality
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1 == 1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("-1 == -1"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1.123 == 1.123"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("true == true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("false == false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("!false == !false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("!true == !true"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("nil == nil"));

    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1 != 5"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1.1 != 1.2"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("true != false"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("!true != !false"));

    // Gt/lt

    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1.23 < 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1.23 <= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = true }, run("1 <= 2"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("1.23 > 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("1.23 >= 2.23"));
    try std.testing.expectEqual(ds.Value{ .boolean = false }, run("1 >= 2"));

    // Errors
    try fails("1 > false");
    try fails("nil < nil");
}
