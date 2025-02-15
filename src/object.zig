const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;

pub const ObjType = enum {
    obj_string,
    obj_function,
};
pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
    pub fn toObjString(obj: *Obj) *ObjString {
        const objStr: *ObjString = @alignCast(@fieldParentPtr("obj", obj));
        return objStr;
    }
    pub fn toObjFunction(obj: *Obj) *ObjFunction {
        const objFunc: *ObjFunction = @alignCast(@fieldParentPtr("obj", obj));
        return objFunc;
    }
};
pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    chunk: Chunk,
    name: ?*ObjString,
    pub fn deinit(self: *ObjFunction, allocator: std.mem.Allocator) void {
        self.chunk.deinit();
        allocator.destroy(self);
    }
    pub fn asObjPtr(objFunc: *ObjFunction) *Obj {
        return &objFunc.obj;
    }

    pub fn toValue(objFunction: *ObjFunction) Value {
        return .{ .type = .val_obj, .as = .{ .obj = &objFunction.obj } };
    }
};
pub const ObjString = struct {
    obj: Obj,
    chars: []u8,
    hash: u32,
    pub fn toValue(objString: *ObjString) Value {
        return .{ .type = .val_obj, .as = .{ .obj = &objString.obj } };
    }
    pub fn asObjPtr(objString: *ObjString) *Obj {
        return &objString.obj;
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
pub fn isObjType(value: Value, type_: ObjType) bool {
    return value.is_obj() and value.as_obj().type == type_;
}
pub fn printObject(value: Value) void {
    switch (value.as_obj().type) {
        .obj_string => std.debug.print("{s}", .{value.as_string()}),
        .obj_function => printFunction(value.as_function()),
    }
}
fn printFunction(function: *ObjFunction) void {
    if (function.name) |name| {
        std.debug.print("<fn {s}>", .{name.chars});
        return;
    }
    std.debug.print("<script>", .{});
}
