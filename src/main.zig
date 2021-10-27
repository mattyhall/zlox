const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const Parser = @import("compiler.zig").Parser;
const ds = @import("ds.zig");

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var obj_allocator = try ds.ObjectAllocator.init(&alloc.allocator);
    defer obj_allocator.deinit();

    var table = try ds.Table.init(&alloc.allocator);
    defer table.deinit();

    const src =
      \\ var a = 0;
      \\ if (a == 10) a = 100;
      \\ else if (a == 20) a = 200;
      \\ else a = 0;
      \\ print a;
    ;
    std.log.debug("{s}", .{src});
    var scanner = scan.Scanner.init(src);
    while (true) {
        const tok = scanner.scanToken();
        if (tok.typ == .err) unreachable;
        if (tok.typ == .eof) break;

        std.log.debug("{} {}", .{tok.typ, tok.line});
    }
    var parser = try Parser.init(&obj_allocator, src, .script);
    const func = (try parser.compile()) orelse unreachable;
    try func.chunk.disassemble("script");

    var v = try vm.Vm.init(&obj_allocator);
    defer v.deinit();

    _ = try v.interpret(func);
}

test {
    _ = @import("end_to_end_tests.zig");
    std.testing.refAllDecls(@This());
}
