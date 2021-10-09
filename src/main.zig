const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const Parser = @import("compiler.zig").Parser;

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var chunk = vm.Chunk.init(&alloc.allocator);
    defer chunk.deinit();

    const src = "1 + 3 * 2 / -7";
    std.log.debug("{s}", .{src});
    var parser = Parser.init(src);
    if (try parser.compile(&chunk))
        return;
    try chunk.disassemble("test");

    var v = vm.Vm.init();
    _ = try v.interpret(&chunk);
}

test {
    _ = @import("end_to_end_tests.zig");
    std.testing.refAllDecls(@This());
}
