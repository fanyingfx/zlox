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
    op_print,
    op_jump,
    op_jump_if_false,
    op_loop,
    op_pop,
    op_call,
    op_closure,
    op_not,
    op_add,
    op_concat,
    op_substract,
    op_multiply,
    op_divide,
    op_define_global,
    op_get_global,
    op_set_global,
    op_get_upvalue,
    op_set_upvalue,
    op_close_upvalue,
    op_set_local,
    op_get_local,
    op_quit,
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