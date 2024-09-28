const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: [:0]const u8,
    position: usize,
    readPosition: usize,
    ch: u8,
    allocator: std.mem.Allocator,

    pub fn init(input: [:0]const u8, alloc: std.mem.Allocator) Lexer {
        var l = Lexer{
            .input = input,
            .allocator = alloc,
            .position = 0,
            .readPosition = 0,
            .ch = 0,
        };

        l.read_char();

        return l;
    }

    pub fn deinit(self: *Lexer) !void {
        _ = self;
    }

    pub fn read_char(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    pub fn next_token(l: *Lexer) token.Token {
        var tok = token.Token {
            .ILLEGAL = "",
        };

        switch (l.ch) {
        }
    }
};

test "Next Token" {
    const Token = token.Token;

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const input = "=+(){},;";

    const tests = [_]Token{
        Token.ASSIGN,
    };

    var l = Lexer.init(input, alloc);

    for (tests, 0..) |tt, i| {
        const tok = l.next_token();
        _ = tok;
        _ = tt;
        _ = i;
    }

    try std.testing.expect(false);
}
