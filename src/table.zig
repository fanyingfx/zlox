const std = @import("std");
const Table = @This();
const nil_val = @import("value.zig").nil_val;
const bool_val = @import("value.zig").bool_val;
const Value = @import("value.zig").Value;
const printValueLn = @import("value.zig").printValueLn;
const ObjString = @import("object.zig").ObjString;
const hashString = @import("object.zig").hashString;
const MAX_LOAD = 0.75;

const Entry = struct {
    key: ?*ObjString,
    value: Value,
};
count: usize,
entries: []Entry,
allocator: std.mem.Allocator,
pub fn init(allocator: std.mem.Allocator) Table {
    return .{
        .entries = allocator.alloc(Entry, 0) catch unreachable,
        .allocator = allocator,
        .count = 0,
    };
}
pub fn deinit(table: *Table) void {
    table.allocator.free(table.entries);
}

fn maxLoad(table: *Table) usize {
    const cap: f32 = @floatFromInt(table.entries.len);
    const max = MAX_LOAD * cap;
    return @intFromFloat(max);
}
fn grow(table: *Table) void {
    const new_cap = if (table.entries.len < 8) 8 else table.entries.len * 2;
    const old_entries = table.entries;
    defer table.allocator.free(old_entries);
    table.entries = table.allocator.alloc(Entry, new_cap) catch unreachable;
    for (table.entries) |*entry| {
        entry.key = null;
        entry.value = nil_val();
    }
    table.count = 0;
    for (old_entries) |entry| {
        if (entry.key) |k| {
            var dest = table.findEntry(k);
            dest.key = k;
            dest.value = entry.value;
            table.count += 1;
        }
    }
}
pub fn findEntry(table: *Table, key: *ObjString) *Entry {
    var index: usize = @intCast(key.hash % table.entries.len);
    var tombstone: ?*Entry = null;
    while (true) {
        const entry = &table.entries[index];
        if (entry.key == null) {
            if (entry.value.is_nil()) {
                return if (tombstone != null) tombstone.? else entry;
            } else {
                if (tombstone == null) {
                    tombstone = entry;
                }
            }
        } else if (entry.key == key) {
            return entry;
        }
        index = @intCast((index + 1) % table.entries.len);
    }
}
pub fn set(table: *Table, key: *ObjString, value: Value) bool {
    if (table.count + 1 > table.maxLoad()) {
        table.grow();
    }
    const entry = table.findEntry(key);
    const isNewKey = entry.key == null;
    if (isNewKey and entry.value.is_nil()) table.count += 1;
    entry.key = key;
    entry.value = value;
    return isNewKey;
}
pub fn get(table: *Table, key: *ObjString) ?Value {
    if (table.count == 0) return null;
    const entry = table.findEntry(key);
    if (entry.key == null) return null;
    return entry.value;
}
pub fn delete(table: *Table, key: *ObjString) bool {
    if (table.count == 0) return false;
    const entry = table.findEntry(key);
    if (entry.key == null) return false;
    entry.key = null;
    entry.value = bool_val(true);
    return true;
}
pub fn findString(table: *Table, str: []const u8, hash: u32) ?*ObjString {
    if (table.count == 0) return null;
    var index: usize = @intCast(hash % table.entries.len);
    while (true) {
        const entry = &table.entries[index];
        if (entry.key) |key| {
            if (key.chars.len == str.len and key.hash == hash and std.mem.eql(u8, key.chars, str)) {
                return key;
            }
        } else if (entry.value.is_nil()) {
            return null;
        }

        index = (index + 1) % table.entries.len;
    }
}
pub fn addAll(from: *Table, to: *Table) void {
    for (from.entries) |*entry| {
        if (entry.key != null) {
            _ = to.set(entry.key, entry.value);
        }
    }
}
test "basic set/get" {
    const allocator = std.testing.allocator;
    const raw1 = "k1";
    const raw2 = "k2";
    const k1 = ObjString.allocateObjString(allocator, raw1);
    defer k1.deinit(allocator);
    const k2 = ObjString.allocateObjString(allocator, raw2);
    defer k2.deinit(allocator);
    var table = Table.init(allocator);
    defer table.deinit();
    const v1 = Value{ .type = .val_number, .as = .{ .number = 10 } };
    const v2 = Value{ .type = .val_bool, .as = .{ .boolean = true } };
    try std.testing.expect(table.set(k1, v1));
    try std.testing.expect(table.set(k2, v2));
    try std.testing.expectEqual(table.get(k1).?.as_number(), 10);
    try std.testing.expect(table.get(k2).?.as_bool());
}
test "interned string" {
    const allocator = std.testing.allocator;
    const raw1 = "k1";
    const raw2 = "k1";
    const obj1 = ObjString.allocateObjString(allocator, raw1);
    defer obj1.deinit(allocator);
    var table = Table.init(allocator);
    defer table.deinit();
    try std.testing.expect(table.set(obj1, nil_val()));
    const obj2 = table.findString(raw2, hashString(raw2)).?;
    try std.testing.expect(obj1 == obj2);
}
