const std = @import("std");
const value = @import("value.zig");
const OpCode = @import("opCode.zig").OpCode;
const Chunk = @This();

const CodeList = std.ArrayList(u8);
const LineList = std.ArrayList(usize);
code: CodeList,
lines: LineList,
constants: value.ValueArray,

pub fn init(allocator: std.mem.Allocator) Chunk {
    return .{
        .code = CodeList.init(allocator),
        .lines = LineList.init(allocator),
        .constants = value.ValueArray.init(allocator),
    };
}
pub fn write(self: *Chunk, byte: u8, line: usize) void {
    self.code.append(byte) catch unreachable;
    self.lines.append(line) catch unreachable;
}
pub fn writeOp(self: *Chunk, opCode: OpCode, line: usize) void {
    self.code.append(@intFromEnum(opCode)) catch unreachable;
    self.lines.append(line) catch unreachable;
}
pub fn addConstant(self: *Chunk, value_: value.Value) usize {
    self.constants.append(value_) catch unreachable;
    return self.constants.items.len - 1;
}
pub fn deinit(self: *Chunk) void {
    self.code.deinit();
    self.constants.deinit();
    self.lines.deinit();
}
