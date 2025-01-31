const std = @import("std");
const OpCode = @import("common.zig").OpCode;
const Chunk = @import("common.zig").Chunk;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const config = @import("config");
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var chunk = Chunk.init(allocator);
    var vm = VM{
        .chunk = undefined,
        .ip = undefined,
        .stack = undefined,
        .stackTop = undefined,
    };
    vm.init();
    defer vm.deinit();
    const c1 = try chunk.addConstant(1.2);
    try chunk.writeOp(.Constant, 123);
    try chunk.write(@intCast(c1), 123);
    const c2 = try chunk.addConstant(3.4);
    try chunk.writeOp(.Constant, 123);
    try chunk.write(@intCast(c2), 123);
    try chunk.writeOp(.Add, 123);
    const c3 = try chunk.addConstant(5.6);
    try chunk.writeOp(.Constant, 123);
    try chunk.write(@intCast(c3), 123);
    try chunk.writeOp(.Divide, 123);
    try chunk.writeOp(.Return, 123);
    if (comptime config.enable_debug) {
        debug.disassembleChunk(&chunk, "test chunk");
    }
    try vm.interpret(&chunk);
    chunk.deinit();
}
