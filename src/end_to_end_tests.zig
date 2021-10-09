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
}
