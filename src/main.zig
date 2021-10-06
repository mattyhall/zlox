const std = @import("std");
const vm = @import("vm.zig");

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    var data: [16 * 1024]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&data);

    var chunk = vm.Chunk.init(&alloc.allocator);
    defer chunk.deinit();

    var constant = try chunk.addConstant(1.2);
    try chunk.writeChunk(@enumToInt(vm.OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    constant = try chunk.addConstant(3.4);
    try chunk.writeChunk(@enumToInt(vm.OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    try chunk.writeChunk(@enumToInt(vm.OpCode.add), 123);

    constant = try chunk.addConstant(5.6);
    try chunk.writeChunk(@enumToInt(vm.OpCode.constant), 123);
    try chunk.writeChunk(constant, 123);

    try chunk.writeChunk(@enumToInt(vm.OpCode.divide), 123);

    try chunk.writeChunk(@enumToInt(vm.OpCode.negate), 123);
    try chunk.writeChunk(@enumToInt(vm.OpCode.ret), 123);

    var m = vm.Vm.init();
    try m.interpret(&chunk);
}
