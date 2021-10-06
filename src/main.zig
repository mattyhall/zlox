const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    // var data: [16 * 1024]u8 = undefined;
    // var alloc = std.heap.FixedBufferAllocator.init(&data);

    const src = "print 1+ \"hello\" 1.2;";
    var line: usize = 0;
    var scanner = scan.Scanner.init(src);
    const stdout = std.io.getStdOut().writer();
    while (true) {
        const token = scanner.scanToken();

        if (token.typ == .err) {
            try stdout.print("{s} '{s}'\n on line {}", .{
                scanner.err orelse unreachable,
                token.loc,
                token.line,
            });
            break;
        }

        if (token.line != line) {
            try stdout.print("{:>4} ", .{token.line});
            line = token.line;
        } else {
            try stdout.print("   | ", .{});
        }
        try stdout.print("{} {s}\n", .{ token.typ, token.loc });

        if (token.typ == .eof) break;
    }
}
