const std = @import("std");
const Obj = @import("object.zig").Obj;
const Chunk = @import("chunk.zig");
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const hashString = @import("object.zig").hashString;
const nil_val = @import("value.zig").nil_val;
const Table = @import("table.zig");
const Collector = @This();

allocator: std.mem.Allocator,
objects: ?*Obj,
table: *Table,

pub fn init(
    allocator: std.mem.Allocator,
    table: *Table,
) Collector {
    return .{ .allocator = allocator, .objects = null, .table = table };
}

pub fn allocateObject(collector: *Collector, comptime T: type) *Obj {
    const obj = collector.allocator.create(Obj) catch unreachable;
    switch (T) {
        ObjFunction => {
            obj.* = Obj{
                .function = .{
                    .arity = 0,
                    .name = null,
                    .chunk = undefined,
                    .next = collector.objects,
                },
            };
            obj.function.chunk = Chunk.init(collector.allocator);
        },
        ObjString => {
            obj.* = Obj{ .string = .{ .chars = undefined, .hash = undefined, .next = collector.objects } };
        },
        else => unreachable,
    }
    collector.objects = obj;
    return obj;
}
pub fn allocateFunction(collector: *Collector) *ObjFunction {
    const obj = collector.allocateObject(ObjFunction);
    return &obj.function;
}
pub fn allocateObjString(collector: *Collector, str: []const u8, hash: u32) *ObjString {
    const objString = &collector.allocateObject(ObjString).string;
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
pub fn freeObjects(collector: *Collector) void {
    var obj_opt = collector.objects;
    while (obj_opt) |obj| {
        switch (obj.*) {
            .function => |*func| {
                func.chunk.deinit();
                obj_opt = func.next;
            },
            .string => |*str| {
                obj_opt = str.next;
                collector.allocator.free(str.chars);
            },
        }
        collector.allocator.destroy(obj);
    }
}
