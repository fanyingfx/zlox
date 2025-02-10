const std = @import("std");

const Value = @import("value.zig").Value;
// const as_obj = @import("value.zig").as_obj;
pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
    pub fn toObjString(obj:*Obj) *ObjString {
        const objStr: *ObjString = @alignCast(@fieldParentPtr("obj", obj));
        return objStr;
    }
};
pub const ObjType = enum { obj_string };
pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
    pub fn obj_val(objString: *ObjString) Value {
        return .{ .type = .val_obj, .as = .{ .obj = &objString.obj } };
    }
    pub fn obj_ptr(objString: *ObjString) *Obj {
        return &objString.obj;
    }
    pub fn allocObjString(allocator: std.mem.Allocator, str: []u8) *ObjString {
        const objString = allocator.create(ObjString) catch unreachable;
        objString.* = .{ .obj = .{ .type = .obj_string ,.next=null}, .chars = str };
        return objString;
    }
    pub fn deinit(objString:*ObjString,allocator:std.mem.Allocator)void{
        allocator.free(objString.chars);
        allocator.destroy(objString);
    }
};
pub inline fn isObjType(value: Value, type_: ObjType) bool {
    return value.is_obj() and value.as_obj().type == type_;
}
pub fn printObject(value: Value) void {
    switch (value.as_obj().type) {
        .obj_string => std.debug.print("{s}", .{value.as_string()}),
    }
}
