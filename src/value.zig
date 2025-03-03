const std = @import("std");
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjClosure = @import("object.zig").ObjClosure;
const printObj = @import("object.zig").printObject;

pub const ValueType = enum {
    val_bool,
    val_nil,
    val_number,
    val_obj,
};

pub const Value = struct {
    type: ValueType,
    as: union {
        boolean: bool,
        number: f64,
        obj: *Obj,
    },
    pub fn as_bool(value: *const Value) bool {
        return value.as.boolean;
    }
    pub fn as_number(value: *const Value) f64 {
        return value.as.number;
    }
    pub fn is_bool(value: *const Value) bool {
        return value.type == .val_bool;
    }
    pub fn is_nil(value: *const Value) bool {
        return value.type == .val_nil;
    }
    pub fn is_number(value: *const Value) bool {
        return value.type == .val_number;
    }
    pub fn is_string(value: * const Value) bool {
        return value.as_obj().type == .obj_string;
    }
    pub fn is_function(value: * const Value) bool {
        return value.as_obj().type == .obj_function;
    }
    pub fn is_closure(value:*const Value)bool{
        return value.as_obj().type == .obj_closure;
    }
    pub fn as_obj(value: Value) *Obj {
        return value.as.obj;
    }
    pub fn is_obj(value: Value) bool {
        return value.type == .val_obj;
    }
    // pub fn as_objString(value: Value) *ObjString {
    //     return value.as_obj().toObjString();
    // }
    pub fn objTo(value:Value,comptime T:type)*T{
        return value.as_obj().to(T);
    }
    pub fn as_string(value: Value) []u8 {
        return value.objTo(ObjString).chars;
    }
    pub fn as_function(value: Value) *ObjFunction {
        return value.objTo(ObjFunction);
    }
    pub fn as_closure(value:*const Value)*ObjClosure{
        return value.objTo(ObjClosure);

    }
    pub fn printValue(v: Value) void {
        switch (v.type) {
            .val_number => std.debug.print("{d}", .{v.as_number()}),
            .val_bool => std.debug.print("{}", .{v.as_bool()}),
            .val_nil => std.debug.print("nil", .{}),
            .val_obj => printObj(v),
        }
    }
    pub fn printValueLn(v: Value) void {
        printValue(v);
        std.debug.print("\n", .{});
    }
};
pub fn valuesEqual(a: Value, b: Value) bool {
    if (a.type != b.type) return false;
    return switch (a.type) {
        .val_bool => a.as_bool() == b.as_bool(),
        .val_nil => true,
        .val_number => a.as_number() == b.as_number(),
        .val_obj => a.as_obj() == b.as_obj(),
    };
}
pub const ValueArray = std.ArrayList(Value);

pub fn bool_val(value: bool) Value {
    return .{ .type = .val_bool, .as = .{ .boolean = value } };
}
pub fn nil_val() Value {
    return .{ .type = .val_nil, .as = .{ .number = 0 } };
}
pub fn number_val(value: f64) Value {
    return .{ .type = .val_number, .as = .{ .number = value } };
}
pub fn obj_val(obj: Obj) Value {
    return .{ .type = .val_obj, .as = .{ .obj = obj } };
}
pub fn isFalsey(value: Value) bool {
    return value.is_nil() or (value.is_bool() and !value.as_bool());
}
