const std = @import("std");
const Chunk = @import("common.zig").Chunk;
const OpCode = @import("common.zig").OpCode;
const value = @import("value.zig");
pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}
pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:<4} ", .{chunk.lines.items[offset]});
    }
    const instruction = OpCode.fromU8(chunk.code.items[offset]);
    switch (instruction) {
        .op_return,.op_negate,.op_add,.op_divide,.op_multiply,.op_substract => {
            return simpleInstruction(instruction.name(), offset);
        },
        .op_constant,
        => {
            return constantInstruction(@tagName(instruction), chunk, offset);
        },
    }
}
fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s:<16}\n", .{name});
    return offset + 1;
}
fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:4} '", .{ name, constant });
    value.printValue(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}
