const std = @import("std");
const OpCode = @import("common.zig").OpCode;
const Chunk = @import("common.zig").Chunk;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const config = @import("config");
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    var vm = VM.init(&chunk);
    defer vm.deinit();
    const args = try std.process.argsAlloc(allocator);
    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try runFile(&vm,args[1]);
    } else {
        std.debug.print("Usage: clox [path]\n", .{});
        std.posix.exit(64);
    }
}
fn repl(vm: *VM) !void {
    var line: [1024]u8 = undefined;
    while (true) {
        std.debug.print("> ", .{});
        const len = try std.io.getStdIn().read(&line);
        try vm.interpret(line[0..len]);
    }
}
fn runFile(vm:*VM,path:[]const u8)!void{
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const source = try std.fs.cwd().readFileAlloc(arena_alloc,path,4096);
    try vm.interpret(source);

}
