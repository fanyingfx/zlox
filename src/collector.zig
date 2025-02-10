const std = @import("std");
const mem = std.mem;
const Collector = @This();
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
allocator: mem.Allocator,
objects: ?*Obj,

pub fn init(allocator: mem.Allocator) Collector {
    return .{ .allocator = allocator, .objects = null };
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
pub fn allocateObjString(collector: *Collector, str: []const u8) *ObjString {
    const objString = collector.allocateObject(ObjString).toObjString();
    objString.chars = collector.allocator.dupe(u8, str) catch unreachable;
    return objString;
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
