const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;

pub const ObjType = enum {
    obj_string,
    obj_function,
    obj_closure,
    obj_upvalue,
};
pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
    pub fn to(obj: *Obj, comptime T: type) *T {
        return @alignCast(@fieldParentPtr("obj", obj));
    }
};
pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    upvalueCount: usize,
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
pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues:[]?*ObjUpvalue,
    upvalueCount:usize,
    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
        allocator.destroy(self);
    }
    pub fn asObjPtr(objClosure: *ObjClosure) *Obj {
        return &objClosure.obj;
    }
    pub fn toValue(objClosure: *ObjClosure) Value {
        return .{ .type = .val_obj, .as = .{ .obj = &objClosure.obj } };
    }
};
pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed:Value,
    next:?*ObjUpvalue,
    pub fn asObjPtr(objUpvalue: *ObjUpvalue) *Obj {
        return &objUpvalue.obj;
    }
    pub fn deinit(self: *ObjUpvalue, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
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
        .obj_closure => printFunction(value.as_closure().function),
        .obj_upvalue => std.debug.print("upvalue",.{}),
    }
}
fn printFunction(function: *ObjFunction) void {
    if (function.name) |name| {
        std.debug.print("<fn {s}>", .{name.chars});
        return;
    }
    std.debug.print("<script>", .{});
}
