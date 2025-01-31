const std = @import("std");
const config = @import("config");
const debug = @import("debug.zig");
const InterpretResult = error{
    CompileError,
    RuntimeError,
};

const common = @import("common.zig");
const Chunk = common.Chunk;
const value = @import("value.zig");
const Value = value.Value;
const STACK_MAX = 256;
pub const VM = struct {
    chunk: *Chunk,
    ip: usize, // TODO change it to pointer later?
    stack: [STACK_MAX]Value,
    stackTop: usize,

    pub fn init(vm: *VM) void {
        vm.resetStack();
    }
    pub fn deinit(self: *VM) void {
        _ = self;
    }
    pub fn push(vm: *VM, value_: Value) void {
        vm.stack[vm.stackTop] = value_;
        vm.stackTop += 1;
    }
    pub fn pop(vm: *VM) Value {
        vm.stackTop -= 1;
        return vm.stack[vm.stackTop];
    }
    pub fn resetStack(vm: *VM) void {
        vm.stackTop = 0;
    }
    pub fn interpret(vm: *VM, chunk: *Chunk) !void {
        vm.chunk = chunk;
        vm.ip = 0;
        return vm.run();
    }
    inline fn read_byte(vm: *VM) u8 {
        defer vm.ip += 1;
        return vm.chunk.code.items[vm.ip];
    }
    inline fn read_constant(vm: *VM) value.Value {
        const idx: usize = @intCast(vm.read_byte());
        return vm.chunk.constants.items[idx];
    }
    inline fn binary_op(vm:*VM,op: common.OpCode, a: Value, b: Value) void {
        switch (op) {
            .Add => vm.push(a + b),
            .Multiply => vm.push(a * b),
            .Substract => vm.push(a - b),
            .Divide => vm.push(a / b),
            else => std.debug.panic("unknow binary operator",.{}),
        }
    }
    fn run(vm: *VM) !void {
        while (true) {
            if (comptime config.enable_debug) {
                std.debug.print("          ", .{});
                var slot: usize = 0;
                while (slot < vm.stackTop) : (slot += 1) {
                    std.debug.print("[ ", .{});
                    value.printValue(vm.stack[slot]);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(vm.chunk, vm.ip);
            }
            const instruction = common.OpCode.fromU8(vm.read_byte());
            switch (instruction) {
                .Constant => {
                    const constant = vm.read_constant();
                    vm.push(constant);
                    value.printValue(constant);
                    std.debug.print("\n", .{});
                },
                .Add, .Substract, .Multiply, .Divide => {
                    const op = instruction;
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.binary_op(op,a,b);
                },
                .Negate => {
                    vm.push(-vm.pop());
                },
                .Return => {
                    value.printValue(vm.pop());
                    std.debug.print("\n", .{});
                    return;
                },
            }
        }
    }
};
