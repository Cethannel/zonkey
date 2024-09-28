const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const PROMPT = ">> ";

pub fn start(in: std.fs.File, out: std.fs.File, galloc: std.mem.Allocator) !void {
    var scanner = std.io.bufferedReader(in.reader());
    var reader = scanner.reader();

    const writer = out.writer();

    var buff = [_]u8{0} ** 1024;
    while (true) {
        var arena = std.heap.ArenaAllocator.init(galloc);
        const alloc = arena.allocator();
        try std.fmt.format(writer, "{s}", .{PROMPT});
        const line = try reader.readUntilDelimiter(&buff, '\n');

        var l = lexer.Lexer.init(line, alloc);

        var tok = l.next_token();

        while (!std.mem.eql(u8, @tagName(tok), @tagName(token.Token.EOF))) {
            try std.fmt.format(writer, "{s}\n", .{try tok.to_string()});

            tok = l.next_token();
        }

        arena.deinit();
    }
}
