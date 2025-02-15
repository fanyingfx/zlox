const std = @import("std");
const Obj = @import("object.zig").Obj;
const printObj = @import("object.zig").printObject;

pub const Value = union(enum) {
    boolean: bool,
    number: f64,
    obj: *Obj,
    nil,
    pub fn eql(self: Value, other: Value) bool {
        return std.meta.eql(self, other);
    }
    pub fn printValue(v: Value) void {
        switch (v) {
            .number => |n| std.debug.print("{d}", .{n}),
            .boolean => |b| std.debug.print("{}", .{b}),
            .nil => std.debug.print("nil", .{}),
            .obj => |o| printObj(o),
        }
    }
    pub fn printValueLn(v: Value) void {
        v.printValue();
        std.debug.print("\n", .{});
    }
    pub fn bool_val(value: bool) Value {
        return .{ .boolean = value };
    }
    pub fn number_val(value: f64) Value {
        return .{ .number = value };
    }
    pub fn obj_val(obj: *Obj) Value {
        return .{ .obj = obj };
    }
    pub fn is_bool(value: Value) bool {
        return value == .boolean;
    }
    pub fn is_nil(value: Value) bool {
        return value == .nil;
    }
    pub fn isFalsey(value: Value) bool {
        return value.is_nil() or (value.is_bool() and !value.boolean);
    }
    pub fn is_number(value: Value) bool {
        return value == .number;
    }
    pub fn is_string(value: Value) bool {
        return value.obj.* == .string;
    }
    pub fn is_function(value: Value) bool {
        return value.obj.* == .function;
    }
    pub fn is_obj(value: Value) bool {
        return value == .obj;
    }
};
pub const ValueArray = std.ArrayList(Value);