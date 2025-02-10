const std = @import("std");

const Value = @import("value.zig").Value;
// const as_obj = @import("value.zig").as_obj;
pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
    pub fn toObjString(obj: *Obj) *ObjString {
        const objStr: *ObjString = @alignCast(@fieldParentPtr("obj", obj));
        return objStr;
    }
};
pub const ObjType = enum { obj_string };
pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
    hash: u32,
    pub fn obj_val(objString: *ObjString) Value {
        return .{ .type = .val_obj, .as = .{ .obj = &objString.obj } };
    }
    pub fn obj_ptr(objString: *ObjString) *Obj {
        return &objString.obj;
    }
    pub fn allocateObjString(allocator: std.mem.Allocator, str: []const u8) *ObjString {
        const objString = allocator.create(ObjString) catch unreachable;
        const allocateStr = allocator.dupe(u8,str) catch unreachable;
        const hash = hashString(str);
        objString.* = .{ .obj = .{ .type = .obj_string, .next = null }, .chars = allocateStr,.hash=hash };
        return objString;
    }
    pub fn deinit(objString: *ObjString, allocator: std.mem.Allocator) void {
        allocator.free(objString.chars);
        allocator.destroy(objString);
    }
};
pub fn hashString(str: []const u8) u32 {
    var hash: u32 = 2_166_136_261;
    for (str) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}
pub inline fn isObjType(value: Value, type_: ObjType) bool {
    return value.is_obj() and value.as_obj().type == type_;
}
pub fn printObject(value: Value) void {
    switch (value.as_obj().type) {
        .obj_string => std.debug.print("{s}", .{value.as_string()}),
    }
}
