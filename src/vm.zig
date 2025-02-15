const std = @import("std");
const config = @import("config");
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const as_bool = @import("value.zig").as_bool;
const as_number = @import("value.zig").as_number;
const number_val = @import("value.zig").number_val;
const nil_val = @import("value.zig").nil_val;
const bool_val = @import("value.zig").bool_val;
const isFalsey = @import("value.zig").isFalsey;
const ValueType = @import("value.zig").ValueType;
const valuesEqual = @import("value.zig").valuesEqual;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Obj = @import("object.zig").Obj;
const Collector = @import("collector.zig");
const Table = @import("table.zig");
const Compiler = @import("compiler.zig").Compiler;
const Chunk = @import("chunk.zig");
const OpCode = @import("opCode.zig").OpCode;
const Value = @import("value.zig").Value;

const InterpretResult = error{
    CompileError,
    RuntimeError,
    Quit,
};
const CallFrame = struct {
    closure: *ObjClosure,
    ip: usize,
    slots: []Value,

    fn read_byte(frame: *CallFrame) u8 {
        defer frame.ip += 1;
        return frame.closure.function.chunk.code.items[frame.ip];
    }
    fn read_u16(frame: *CallFrame) u16 {
        var buf: [2]u8 = undefined;
        frame.ip += 2;
        buf[0] = frame.closure.function.chunk.code.items[frame.ip - 2]; // high
        buf[1] = frame.closure.function.chunk.code.items[frame.ip - 1]; // low
        return std.mem.readInt(u16, &buf, .big);
    }
    fn read_constant(frame: *CallFrame) Value {
        const idx: usize = @intCast(frame.read_byte());
        return frame.closure.function.chunk.constants.items[idx];
    }
    fn read_string(vm: *CallFrame) *ObjString {
        return vm.read_constant().objTo(ObjString);
    }
};

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);
pub const VM = struct {
    chunk: *const Chunk,
    stack: [STACK_MAX]Value,
    collector: *Collector,
    openUpvalues:?*ObjUpvalue,
    stackTop: usize,
    globals: Table,
    base: usize = 0,
    frames: [FRAMES_MAX]CallFrame,
    frameCount: usize,

    pub fn init(collector: *Collector) VM {
        return .{
            .chunk = &Chunk.init(collector.allocator),
            .stack = undefined,
            .stackTop = 0,
            .frameCount = 0,
            .base = 0,
            .frames = undefined,
            .collector = collector,
            .globals = Table.init(collector.allocator),
            .openUpvalues = null,
        };
    }
    pub fn deinit(vm: *VM) void {
        vm.globals.deinit();
    }
    fn push(vm: *VM, value: Value) void {
        vm.stack[vm.stackTop] = value;
        vm.stackTop += 1;
    }
    fn pop(vm: *VM) Value {
        vm.stackTop -= 1;
        return vm.stack[vm.stackTop];
    }
    fn peek(vm: *const VM, distance: usize) Value {
        return vm.stack[vm.stackTop - 1 - distance];
    }
    fn callValue(vm: *VM, callee: Value, argCount: usize) !void {
        if (callee.is_obj()) {
            switch (callee.as_obj().type) {
                .obj_closure => {
                    vm.call(callee.as_closure(), argCount);
                    return;
                },
                .obj_function => {
                    vm.call(callee.as_closure(), argCount);
                    return;
                },
                else => {
                    std.debug.panic("Can not call on string.\n", .{});
                },
            }
        }
        std.debug.print("Can only call functions.", .{});
        return error.RuntimeError;
    }
    fn call(vm: *VM, closure: *ObjClosure, argCount: usize) void {
        if (argCount != closure.function.arity) {
            std.debug.panic("Expected {} arguments but got {}.", .{ closure.function.arity, argCount });
        }
        if (vm.frameCount == FRAMES_MAX) {
            std.debug.panic("stack overflow\n", .{});
        }
        const frame = &vm.frames[vm.frameCount];
        frame.closure = closure;
        vm.frameCount += 1;
        // frame.function = function;
        frame.ip = 0;
        vm.base = vm.stackTop - argCount - 1;
        frame.slots = vm.stack[vm.base..];
    }
    fn captureUpvalue(vm:*VM,local:*Value)*ObjUpvalue{
        var prevUpvalue:?*ObjUpvalue = null;
        var upvalue = vm.openUpvalues;
        while(upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local) ){
            prevUpvalue = upvalue;
            upvalue = upvalue.?.next;
        }
        if(upvalue != null and upvalue.?.location == local ){
            return upvalue.?;
        }
        
        const createdUpvalue= vm.collector.allocateUpvalue(local);
        createdUpvalue.next = upvalue;
        if(prevUpvalue == null){
            vm.openUpvalues = createdUpvalue;
        }else{
            prevUpvalue.?.next = createdUpvalue;
        }
        return createdUpvalue;

    }
    fn closeUpvalues(vm:*VM,last:*Value)void{
        while(vm.openUpvalues != null and @intFromPtr(vm.openUpvalues.?.location) >= @intFromPtr(last)){
            const upvalue = vm.openUpvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            vm.openUpvalues = upvalue.next;
        }
    }
    fn resetStack(vm: *VM) void {
        vm.stackTop = 0;
    }
    fn printStack(vm: *VM, comptime msg: []const u8) void {
        if (comptime config.print_stack) {
            std.debug.print("----" ++ msg ++ "--------\n", .{});
            defer std.debug.print("------------\n", .{});
            if (vm.stackTop == 0) return;
            var top = vm.stackTop - 1;
            while (top >= 0) {
                vm.stack[top].printValueLn();
                if (top == 0) break;
                top -= 1;
            }
        }
    }
    pub fn interpret(vm: *VM, source: []const u8) !void {
        var chunk = Chunk.init(vm.collector.allocator);
        defer chunk.deinit();
        const function = try compiler.compile(vm.collector, source, &chunk);
        vm.chunk = &chunk;
        vm.push(function.toValue());
        const closure = vm.collector.allocateClosure(function);
        _ = vm.pop();
        vm.push(closure.toValue());
        _ = vm.call(closure, 0);
        return vm.run();
    }
    inline fn binary_op(vm: *VM, comptime typ: type, valueType: fn (typ) Value, op: OpCode) !void {
        if (!vm.peek(0).is_number() or !vm.peek(1).is_number()) {
            std.debug.print("Operands must be numbers.\n", .{});
            vm.printStack("stack overflow");
            return error.RuntimeError;
        }
        const b = vm.pop().as_number();
        const a = vm.pop().as_number();
        const val = switch (typ) {
            f64 => switch (op) {
                .op_add => a + b,
                .op_multiply => a * b,
                .op_substract => a - b,
                .op_divide => a / b,
                else => std.debug.panic("unknow binary operator", .{}),
            },
            bool => switch (op) {
                .op_greater => a > b,
                .op_less => a < b,
                else => std.debug.panic("unknow binary operator", .{}),
            },
            else => unreachable,
        };
        vm.push(valueType(val));
    }
    fn run(vm: *VM) !void {
        var frame = &vm.frames[vm.frameCount - 1];
        while (true) {
            if (comptime config.print_stack and config.enable_debug) {
                std.debug.print("          ", .{});
                var slot: usize = 0;
                while (slot < vm.stackTop) : (slot += 1) {
                    std.debug.print("[ ", .{});
                    vm.stack[slot].printValueLn();
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
            }
            if (comptime config.enable_debug) {
                _ = debug.disassembleInstruction(&frame.closure.function.chunk, frame.ip);
            }
            const instruction = OpCode.fromU8(frame.read_byte());
            switch (instruction) {
                .op_constant => {
                    const constant = frame.read_constant();
                    vm.push(constant);
                },
                .op_nil => vm.push(nil_val()),
                .op_true => vm.push(bool_val(true)),
                .op_false => vm.push(bool_val(false)),
                .op_equal => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(bool_val(valuesEqual(a, b)));
                },
                .op_not => vm.push(bool_val(isFalsey(vm.pop()))),
                .op_add, .op_substract, .op_multiply, .op_divide => {
                    const op = instruction;
                    try vm.binary_op(f64, number_val, op);
                },
                .op_concat => {
                    if (!vm.peek(0).is_string() or !vm.peek(1).is_string()) {
                        std.debug.print("Operands must be two strings.", .{});
                        return error.RuntimeError;
                    }
                    var buf: [1024]u8 = undefined;
                    const b = vm.pop().as_string();
                    const a = vm.pop().as_string();
                    const new_str = std.fmt.bufPrint(&buf, "{s}{s}", .{ a, b }) catch unreachable;
                    const result = vm.collector.copyString(new_str);
                    vm.push(result.toValue());
                },
                .op_less, .op_greater => {
                    const op = instruction;
                    try vm.binary_op(bool, bool_val, op);
                },
                .op_negate => {
                    if (!vm.peek(0).is_number()) {
                        std.debug.print("Operand must b a number.\n", .{});
                        return error.RuntimeError;
                    }
                    vm.push(number_val(-vm.pop().as_number()));
                },
                .op_print => {
                    vm.pop().printValueLn();
                },
                .op_jump_if_false => {
                    const offset = frame.read_u16();
                    if (!vm.peek(0).as_bool()) {
                        frame.ip += offset;
                    }
                },
                .op_jump => {
                    const offset = frame.read_u16();
                    frame.ip += offset;
                },
                .op_loop => {
                    const offset = frame.read_u16();
                    frame.ip -= offset;
                    break;
                },
                .op_define_global => {
                    const name = frame.read_string();
                    _ = vm.globals.set(name, vm.peek(0));
                    _ = vm.pop();
                },
                .op_get_global => {
                    const name = frame.read_string();
                    if (vm.globals.get(name)) |v| {
                        vm.push(v);
                    } else {
                        std.debug.print("Undefined variable '{s}'.", .{name.chars});
                        return error.RuntimeError;
                    }
                },
                .op_set_global => {
                    const name = frame.read_string();
                    if (vm.globals.set(name, vm.peek(0))) {
                        _ = vm.globals.delete(name);
                        std.debug.print("Undefined variable '{s}'.", .{name.chars});
                        return error.RuntimeError;
                    }
                },
                .op_get_local => {
                    const slot: usize = @intCast(frame.read_byte());
                    vm.push(frame.slots[slot]);
                },
                .op_set_local => {
                    const slot: usize = @intCast(frame.read_byte());
                    frame.slots[slot] = vm.peek(0);
                },
                .op_pop => {
                    _ = vm.pop();
                },
                .op_call => {
                    const argCount: usize = @intCast(frame.read_byte());
                    try vm.callValue(vm.peek(argCount), argCount);
                    frame = &vm.frames[vm.frameCount - 1];
                },
                .op_closure => {
                    const func = frame.read_constant().as_function();
                    const closure = vm.collector.allocateClosure(func);
                    vm.push(closure.toValue());
                    for(0..closure.upvalueCount)|i|{
                        const isLocal = frame.read_byte() != 0;
                        const index:usize = @intCast(frame.read_byte());
                        if(isLocal){
                            closure.upvalues[i]=vm.captureUpvalue(&frame.slots[index]);
                        }else{
                            closure.upvalues[i]=frame.closure.upvalues[index].?;
                        }
                    }
                },
                .op_get_upvalue =>{
                    const slot:usize = @intCast(frame.read_byte());
                    vm.push(frame.closure.upvalues[slot].?.location.*);


                },
                .op_set_upvalue => {
                    const slot:usize =  @intCast(frame.read_byte());
                    frame.closure.upvalues[slot].?.location.* = vm.peek(0);
                },
                .op_close_upvalue=>{
                    vm.closeUpvalues(&vm.stack[vm.stackTop - 1]);
                    _= vm.pop();
                },
                .op_return => {
                    const result = vm.pop();
                    vm.closeUpvalues(&frame.slots[0]); // TODO
                    vm.frameCount -= 1;
                    if (vm.frameCount == 0) {
                        _ = vm.pop();
                        return;
                    }
                    vm.stackTop = vm.base;
                    vm.push(result);
                    frame = &vm.frames[vm.frameCount - 1];
                },
                .op_quit => {
                    std.debug.print("Bye~\n", .{});
                    return error.Quit;
                },
            }
        }
    }
};
