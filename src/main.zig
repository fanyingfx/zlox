
const std = @import("std");
const OpCode = @import("common.zig").OpCode;
const Chunk= @import("common.zig").Chunk;
const debug = @import("debug.zig");
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _= gpa.deinit();
    const allocator = gpa.allocator();

    var chunk = Chunk.init(allocator);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(OpCode.toU8(.Constant),123);
    try chunk.write(@intCast(constant),123);
    try chunk.write(OpCode.toU8(.Return),123);
    debug.disassembleChunk(&chunk,"test chunk");
    chunk.deinit();
}




