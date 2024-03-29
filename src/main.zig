const std = @import("std");
const vm = @import("vm.zig");
const scan = @import("scanner.zig");
const Parser = @import("compiler.zig").Parser;
const ds = @import("ds.zig");
const memory = @import("memory.zig");

const Allocator = std.mem.Allocator;

fn readFile(gpa: std.mem.Allocator, path: []const u8) ![]const u8 {
    var abs_path = if (std.fs.path.isAbsolute(path))
        try gpa.dupe(u8, path)
    else
        try std.fs.cwd().realpathAlloc(gpa, path);

    defer gpa.free(abs_path);

    var f = try std.fs.openFileAbsolute(abs_path, .{});
    defer f.close();

    return try f.reader().readAllAlloc(gpa, 10 * 1024 * 1024);
}

pub fn main() anyerror!void {
    const allocator = std.heap.page_allocator;

    var obj_allocator = try memory.ObjectAllocator.init(allocator);
    defer obj_allocator.deinit();

    var table = try ds.Table.init(allocator);
    defer table.deinit();

    var src = try readFile(allocator, std.mem.span(std.os.argv[1]));

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
    // try func.chunk.disassemble("script");

    var v = try vm.Vm.init(&obj_allocator);
    defer v.deinit();

    _ = try v.interpret(func);
}

test {
    _ = @import("end_to_end_tests.zig");
    std.testing.refAllDecls(@This());
}
