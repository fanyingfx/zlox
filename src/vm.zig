const std = @import("std");
const config = @import("config");
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const InterpretResult = error{
    CompileError,
    RuntimeError,
};

const Chunk = @import("common.zig").Chunk;
const OpCode = @import("common.zig").OpCode;
const value = @import("value.zig");
const Value = value.Value;
const STACK_MAX = 256;
pub const VM = struct {
    chunk: *Chunk,
    ip: usize, // TODO change it to pointer later?
    stack: [STACK_MAX]Value,
    stackTop: usize,

    pub fn init(chunk: *Chunk) VM {
        return .{
            .chunk = chunk,
            .ip = 0,
            .stack = undefined,
            .stackTop = 0,
        };
        // vm.resetStack();
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
    pub fn interpret(vm: *VM, source: []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        var chunk = Chunk.init(arena_alloc);
        defer chunk.deinit();
        compiler.compile(source, &chunk);
        vm.chunk = &chunk;
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
    inline fn binary_op(vm: *VM, op: OpCode, a: Value, b: Value) void {
        switch (op) {
            .op_add => vm.push(a + b),
            .op_multiply => vm.push(a * b),
            .op_substract => vm.push(a - b),
            .op_divide => vm.push(a / b),
            else => std.debug.panic("unknow binary operator", .{}),
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
            const instruction =OpCode.fromU8(vm.read_byte());
            switch (instruction) {
                .op_constant => {
                    const constant = vm.read_constant();
                    vm.push(constant);
                },
                .op_add, .op_substract, .op_multiply, .op_divide => {
                    const op = instruction;
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.binary_op(op, a, b);
                },
                .op_negate => {
                    vm.push(-vm.pop());
                },
                .op_return => {
                    value.printValueLn(vm.pop());
                    return;
                },
            }
        }
    }
};
