const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;

pub const Obj = union(enum) {
    string: ObjString,
    function: ObjFunction,
    pub fn toValue(obj: *Obj) Value {
        return .{ .obj = obj };
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
    pub fn asObj(objFunction: *ObjFunction) *Obj {
        return @fieldParentPtr("function", objFunction);
    }

    pub fn toValue(objFunction: *ObjFunction) Value {
        return objFunction.asObj().toValue();
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
    pub fn asObj(objString: *ObjString) *Obj {
        return @fieldParentPtr("string", objString);
    }
    pub fn toValue(objString: *ObjString) Value {
        return objString.asObj().toValue();
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
pub fn printObject(obj:*Obj) void {
    switch (obj.*) {
        .string => |*s| std.debug.print("{s}", .{s.chars}),
        .function => |*func| printFunction(func),
    }
}
fn printFunction(function: *ObjFunction) void {
    if (function.name) |name| {
        std.debug.print("<fn {s}>", .{name.chars});
        return;
    }
    std.debug.print("<script>", .{});
}
