const std = @import("std");
const mem = std.mem;
const Collector = @This();
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const hashString = @import("object.zig").hashString;
const Table = @import("table.zig");
const nil_val = @import("value.zig").nil_val;
allocator: mem.Allocator,
objects: ?*Obj,
table:*Table,

pub fn init(allocator: mem.Allocator,table:*Table) Collector {
    return .{ .allocator = allocator, .objects = null,.table=table };
}
pub fn allocateObject(collector: *Collector, comptime T: type) *Obj {
    const objectRaw = collector.allocator.create(T) catch unreachable;
    const obj: *Obj = objectRaw.obj_ptr();
    obj.type = switch (T) {
        ObjString => .obj_string,
        else => unreachable,
    };
    obj.next = collector.objects;
    collector.objects = obj;
    return obj;
}
pub fn allocateObjString(collector: *Collector, str: []const u8,hash:u32) *ObjString {
    const objString = collector.allocateObject(ObjString).toObjString();
    objString.hash=hash;
    objString.chars = collector.allocator.dupe(u8, str) catch unreachable;
    _=collector.table.set(objString,nil_val());
    return objString;
}
pub fn copyString(collector:*Collector,str:[]const u8)*ObjString{
    const hash=hashString(str);
    const interned = collector.table.findString(str,hash);
    if (interned != null) return interned.?;
    return collector.allocateObjString(str,hash);
}
pub fn freeObject(collector: *Collector, obj: *Obj) void {
    switch (obj.type) {
        .obj_string => {
            const objString = obj.toObjString();
            objString.deinit(collector.allocator);
        },
    }
}
pub fn freeObjects(collector: *Collector) void {
    var obj_opt = collector.objects;
    while (obj_opt) |obj| {
        const next = obj.next;
        collector.freeObject(obj);
        obj_opt = next;
    }
}
