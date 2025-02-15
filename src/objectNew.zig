const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;

pub const Obj = struct {
    string: ObjString,
    function: ObjFunction,
    pub fn toObjString(obj: *Obj) *ObjString {
        return &obj.obj_string;
    }
    pub fn toObjFunction(obj: *Obj) *ObjFunction {
        return &obj.obj_function;
    }
    pub fn obj_val(obj: *Obj) Value {
        return .{ .type = .val_obj, .as = .{ .obj = obj } };
    }
};
pub const ObjFunction = struct {
    arity: usize,
    chunk: Chunk,
    name: ?*ObjString,
    next: ?*Obj,
    pub fn deinit(self: *ObjFunction, allocator: std.mem.Allocator) void {
        self.chunk.deinit();
        allocator.destroy(self);
    }
};
pub const ObjString = struct {
    chars: []u8,
    hash: u32,
    next: ?*Obj,
    pub fn allocateObjString(allocator: std.mem.Allocator, str: []const u8) *ObjString {
        const objString = allocator.create(ObjString) catch unreachable;
        const allocateStr = allocator.dupe(u8, str) catch unreachable;
        const hash = hashString(str);
        objString.* = .{ .next = null, .chars = allocateStr, .hash = hash };
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
// pub inline fn isObjType(value: Value, type_: ObjType) bool {
//     return value.is_obj() and value.as_obj().type == type_;
// }
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
