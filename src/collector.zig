const std = @import("std");
const Collector = @This();
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Chunk = @import("chunk.zig");
const hashString = @import("object.zig").hashString;
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const nil_val = @import("value.zig").nil_val;
allocator: std.mem.Allocator,
objects: ?*Obj,
table: *Table,

pub fn init(allocator: std.mem.Allocator, table: *Table) Collector {
    return .{ .allocator = allocator, .objects = null, .table = table };
}
pub fn allocateObject(collector: *Collector, comptime T: type) *T {
    const objectRaw = collector.allocator.create(T) catch unreachable;
    const obj: *Obj = objectRaw.asObjPtr();
    obj.type = switch (T) {
        ObjString => .obj_string,
        ObjFunction => .obj_function,
        ObjClosure => .obj_closure,
        ObjUpvalue => .obj_upvalue,
        else => unreachable,
    };
    obj.next = collector.objects;
    collector.objects = obj;
    return obj.to(T);
}
pub fn allocateFunction(collector: *Collector) *ObjFunction {
    const objFunc = collector.allocateObject(ObjFunction);
    objFunc.arity = 0;
    objFunc.name = null;
    objFunc.upvalueCount = 0;
    objFunc.chunk = Chunk.init(collector.allocator);
    return objFunc;
}
pub fn allocateClosure(collector: *Collector, function: *ObjFunction) *ObjClosure {
    const upvalues = collector.allocator.alloc(?*ObjUpvalue, function.upvalueCount) catch unreachable;
    for (upvalues) |*upv| {
        upv.* = null;
    }
    const objClosure = collector.allocateObject(ObjClosure);
    objClosure.function = function;
    objClosure.upvalues = upvalues;
    objClosure.upvalueCount = function.upvalueCount;
    return objClosure;
}
pub fn allocateObjString(collector: *Collector, str: []const u8, hash: u32) *ObjString {
    const objString = collector.allocateObject(ObjString);
    objString.hash = hash;
    objString.chars = collector.allocator.dupe(u8, str) catch unreachable;
    _ = collector.table.set(objString, nil_val());
    return objString;
}
pub fn allocateUpvalue(collector: *Collector, slot: *Value) *ObjUpvalue {
    const objUpvalue = collector.allocateObject(ObjUpvalue);
    objUpvalue.location = slot;
    objUpvalue.closed = nil_val();
    objUpvalue.next = null;
    return objUpvalue;
}
pub fn copyString(collector: *Collector, str: []const u8) *ObjString {
    const hash = hashString(str);
    const interned = collector.table.findString(str, hash);
    if (interned != null) return interned.?;
    return collector.allocateObjString(str, hash);
}
pub fn freeObject(collector: *Collector, obj: *Obj) void {
    switch (obj.type) {
        .obj_string => {
            const objString = obj.to(ObjString);
            objString.deinit(collector.allocator);
        },
        .obj_function => {
            const objFunc = obj.to(ObjFunction);
            objFunc.deinit(collector.allocator);
        },
        .obj_closure => {
            const objClosure = obj.to(ObjClosure);
            objClosure.deinit(collector.allocator);
        },
        .obj_upvalue => {
            const objUpvalue = obj.to(ObjUpvalue);
            objUpvalue.deinit(collector.allocator);
        },
    }
}
pub fn freeObjects(collector: *Collector) void {
    var obj_opt = collector.objects;
    while (obj_opt) |obj| {
        obj_opt = obj.next;
        collector.freeObject(obj);
    }
}
