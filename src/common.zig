const std = @import("std");
const value = @import("value.zig");

pub const OpCode = enum(u8) {
    op_return,
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_equal,
    op_greater,
    op_less,
    op_negate,
    op_not,
    op_add,
    op_concat,
    op_substract,
    op_multiply,
    op_divide,
    pub fn fromU8(byte: u8) OpCode {
        return @enumFromInt(byte);
    }
    pub fn toU8(op: OpCode) u8 {
        return @intFromEnum(op);
    }
    pub fn name(op: OpCode) []const u8 {
        return @tagName(op);
    }
};
pub const Chunk = struct {
    const CodeList = std.ArrayList(u8);
    const LineList = std.ArrayList(usize);
    code: CodeList,
    lines: LineList,
    constants: value.ValueArray,
    // allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return .{
            .code = CodeList.init(allocator),
            .lines = LineList.init(allocator),
            .constants = value.ValueArray.init(allocator),
            // .allocator = allocator,
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
        // self.code = CodeList.init(self.allocator);
    }
};
