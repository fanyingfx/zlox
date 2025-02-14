const std = @import("std");
const Collector = @This();
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const Chunk = @import("chunk.zig");
const hashString = @import("object.zig").hashString;
const Table = @import("table.zig");
const nil_val = @import("value.zig").nil_val;
allocator: std.mem.Allocator,
objects: ?*Obj,
table: *Table,

pub fn init(allocator: std.mem.Allocator, table: *Table) Collector {
    return .{ .allocator = allocator, .objects = null, .table = table };
}
pub fn allocateObject(collector: *Collector, comptime T: type) *Obj {
    const objectRaw = collector.allocator.create(T) catch unreachable;
    const obj: *Obj = objectRaw.obj_ptr();
    obj.type = switch (T) {
        ObjString => .obj_string,
        ObjFunction => .obj_function,
        else => unreachable,
    };
    obj.next = collector.objects;
    collector.objects = obj;
    return obj;
}
pub fn allocateFunction(collector: *Collector) *ObjFunction {
    const objFunc = collector.allocateObject(ObjFunction).toObjFunction();
    objFunc.arity = 0;
    objFunc.name = null;
    objFunc.chunk = Chunk.init(collector.allocator);
    return objFunc;
}
pub fn allocateObjString(collector: *Collector, str: []const u8, hash: u32) *ObjString {
    const objString = collector.allocateObject(ObjString).toObjString();
    objString.hash = hash;
    objString.chars = collector.allocator.dupe(u8, str) catch unreachable;
    _ = collector.table.set(objString, nil_val());
    return objString;
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
            const objString = obj.toObjString();
            objString.deinit(collector.allocator);
        },
        .obj_function => {
            const objFunc = obj.toObjFunction();
            objFunc.deinit(collector.allocator);
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
