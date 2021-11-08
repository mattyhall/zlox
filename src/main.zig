const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const Parser = @import("compiler.zig").Parser;
const ds = @import("ds.zig");
const memory = @import("memory.zig");

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    const allocator = std.heap.c_allocator;

    var obj_allocator = try memory.ObjectAllocator.init(allocator);
    defer obj_allocator.deinit();

    var table = try ds.Table.init(allocator);
    defer table.deinit();

    var src: []const u8 =
        \\ fun fib(n) {
        \\   if (n < 2) return n;
        \\   return fib(n-1) + fib(n-2);
        \\ }
        \\ var start = clock();
        \\ print fib(5);
        \\ print clock() - start;
    ;

    src =
        \\ fun outer(n) {
        \\   var limit = n;
        \\   fun inner() {
        \\     var res = "";
        \\     for (var i = 0; i < limit; i = i + 1) {
        \\       res = res + "hi";
        \\     }
        \\     limit = limit + 1;
        \\     return res;
        \\   }
        \\   return inner;
        \\ }
        \\ var inner = outer(1);
        \\ print inner();
    ;

    src =
        \\ {
        \\   var res;
        \\   var adjective = "good";
        \\   {
        \\     var time_of_day = "morning";
        \\     res = adjective + " " + time_of_day;
        \\   }
        \\   return res;
        \\ }
    ;

    if (false) {
        std.log.debug("{s}", .{src});
        var scanner = scan.Scanner.init(src);
        while (true) {
            const tok = scanner.scanToken();
            if (tok.typ == .err) unreachable;
            if (tok.typ == .eof) break;

            std.log.debug("{} {}", .{ tok.typ, tok.line });
        }
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
