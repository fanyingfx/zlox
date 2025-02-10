const std = @import("std");
const ValueType = @import("common.zig").ValueType;

pub const Value = struct {
    type: ValueType,
    as: union { boolean: bool, number: f64 },
    pub inline fn as_bool(value: Value) bool {
        return value.as.boolean;
    }
    pub inline fn as_number(value: Value) f64 {
        return value.as.number;
    }
    pub fn is_bool(value: Value) bool {
        return value.type == .val_bool;
    }
    pub fn is_nil(value: Value) bool {
        return value.type == .val_nil;
    }
    pub fn is_number(value: Value) bool {
        return value.type == .val_number;
    }
};
pub const ValueArray = std.ArrayList(Value);
pub fn valuesEqual(a:Value,b:Value)bool{
    if (a.type != b.type)return false;
    switch(a.type){
        .val_bool => return a.as_bool() == b.as_bool(),
        .val_nil => return true,
        .val_number => return a.as_number() == b.as_number(),
    }
}
pub fn printValue(v: Value) void {
    switch(v.type){
        .val_number => std.debug.print("{d}", .{v.as_number()}),
        .val_bool => std.debug.print("{}",.{v.as_bool()}),
        .val_nil => std.debug.print("nil",.{})
    }
}
pub fn printValueLn(v: Value) void {
    printValue(v);
    std.debug.print("\n", .{});
}
pub fn bool_val(value: bool) Value {
    return .{ .type = .val_bool, .as = .{ .boolean = value } };
}
pub fn nil_val() Value {
    return .{ .type = .val_nil, .as = .{ .number = 0 } };
}
pub fn number_val(value: f64) Value {
    return .{ .type = .val_number, .as = .{ .number = value } };
}
pub fn isFalsey(value:Value)bool{
    return value.is_nil() or (value.is_bool() and !value.as_bool());

}
