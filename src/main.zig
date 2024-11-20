const std = @import("std");
const repl = @import("repl.zig");
const chm = @import("comptime_hash_map");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    try repl.start(stdin, stdout, std.heap.page_allocator);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test {
    const lexer = @import("lexer.zig");
    _ = lexer;
    const parser = @import("parser.zig");
    _ = parser;
    const evaluator = @import("evaluator.zig");
    _ = evaluator;
    const code = @import("code.zig");
    _ = code;
    const compiler = @import("compiler.zig");
    _ = compiler;
    @import("std").testing.refAllDecls(@This());
}
