const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");
const object = @import("object.zig");

const PROMPT = ">> ";

pub fn start(in: std.fs.File, out: std.fs.File, galloc: std.mem.Allocator) !void {
    var scanner = std.io.bufferedReader(in.reader());
    var reader = scanner.reader();
    var env = object.Environment.init(galloc);
    defer env.deinit();

    const writer = out.writer();

    var buff = [_]u8{0} ** 1024;
    while (true) {
        var arena = std.heap.ArenaAllocator.init(galloc);
        const alloc = arena.allocator();
        try std.fmt.format(writer, "{s}", .{PROMPT});
        const line = try reader.readUntilDelimiter(&buff, '\n');

        const l = lexer.Lexer.init(line, alloc);
        var p = parser.Parser.init(l) catch unreachable;
        const program = p.parse_program() catch unreachable;
        if (p.errors.items.len != 0) {
            try printParseErrors(writer, p.errors);
            continue;
        }

        const evaluated = try evaluator.eval(program, &env);
        try writer.writeAll(try evaluated.inspect(alloc));
        try writer.writeAll("\n");

        arena.deinit();
    }
}

fn printParseErrors(
    writer: @TypeOf(std.io.getStdOut().writer()),
    errors: parser.Parser.Errors,
) !void {
    for (errors.items) |err| {
        try std.fmt.format(writer, "\t{s}\n", .{err});
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
