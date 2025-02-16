const std = @import("std");
const Chunk = @import("chunk.zig");
const OpCode = @import("opCode.zig").OpCode;
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
    if (offset == 0 or (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1])) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:<4} ", .{chunk.lines.items[offset]});
    }
    const instruction = OpCode.fromU8(chunk.code.items[offset]);
    switch (instruction) {
        .op_return, .op_negate, .op_add, .op_concat, .op_divide, .op_multiply, .op_substract, .op_nil, .op_false, .op_true, .op_not, .op_equal, .op_greater, .op_less, .op_quit, .op_print, .op_pop => {
            return simpleInstruction(instruction.name(), offset);
        },
        .op_constant,
        .op_define_global,
        .op_get_global,
        .op_set_global,
        .op_close_upvalue,
        => {
            return constantInstruction(@tagName(instruction), chunk, offset);
        },
        .op_get_local,
        .op_set_local,
        .op_call,
        .op_get_upvalue,
        .op_set_upvalue,
        => {
            return byteInstruction(@tagName(instruction), chunk, offset);
        },
        .op_closure => {
            var _offset: usize = offset + 1;
            const constant: usize = @intCast(chunk.code.items[_offset]);
            _offset += 1;
            std.debug.print("{s:<16} {d:4} ", .{ "op_closure", constant });
            chunk.constants.items[constant].printValueLn();
            const function = chunk.constants.items[constant].as_function();
            var j: usize = 0;
            while (j < function.upvalueCount) : (j += 1) {
                const isLocal = chunk.code.items[_offset] != 0;
                _offset += 1;
                const index = chunk.code.items[_offset];
                _offset += 1;
                std.debug.print("{d:4}      |                     {s} {d}\n", .{ _offset - 2, if (isLocal) "local" else "upvalue", index });
            }
            return _offset;
        },
        .op_jump_if_false,
        .op_jump,
        => {
            return jumpInstruction(@tagName(instruction), 1, chunk, offset);
        },
        .op_loop => {
            return jumpInstruction(@tagName(instruction), -1, chunk, offset);
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
    chunk.constants.items[constant].printValueLn();
    return offset + 2;
}
fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:4} '", .{ name, slot });
    return offset + 2;
}
fn jumpInstruction(name: []const u8, sign: isize, chunk: *const Chunk, offset: usize) usize {
    const slice = chunk.code.items[offset + 1 .. offset + 3];
    const jump = std.mem.readInt(u16, @ptrCast(slice), .big);
    const jumpTo: isize = @as(isize, @intCast(offset)) + 3 + sign * @as(isize, jump);
    std.debug.print("{s:<16} {d:4} -> {d}\n", .{ name, offset, jumpTo });
    return offset + 3;
}

pub fn debug_log(comptime src:std.builtin.SourceLocation,msg:[]const u8)void{
    std.debug.print("{s}:{d} <fn {s}> {s}\n",.{src.file,src.line,src.fn_name,msg});
}
