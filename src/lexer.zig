const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    readPosition: usize,
    ch: u8,
    allocator: std.mem.Allocator,

    pub fn init(input: []const u8, alloc: std.mem.Allocator) Lexer {
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
        const Token = token.Token;
        var tok = token.Token{
            .ILLEGAL = 0,
        };

        l.skip_whitespace();

        switch (l.ch) {
            '=' => {
                if (l.peek_char() == '=') {
                    l.read_char();
                    tok = .EQ;
                } else {
                    tok = .ASSIGN;
                }
            },
            '+' => {
                tok = .PLUS;
            },
            '-' => {
                tok = .MINUS;
            },
            '!' => {
                if (l.peek_char() == '=') {
                    l.read_char();
                    tok = .NOT_EQ;
                } else {
                    tok = .BANG;
                }
            },
            '/' => {
                tok = .SLASH;
            },
            '*' => {
                tok = .ASTERISK;
            },
            '<' => {
                tok = .LT;
            },
            '>' => {
                tok = .GT;
            },
            ';' => {
                tok = .SEMICOLON;
            },
            '(' => {
                tok = .LPAREN;
            },
            ')' => {
                tok = .RPAREN;
            },
            ',' => {
                tok = .COMMA;
            },
            '{' => {
                tok = .LBRACE;
            },
            '}' => {
                tok = .RBRACE;
            },
            '[' => {
                tok = .LBRACKET;
            },
            ']' => {
                tok = .RBRACKET;
            },
            '"' => {
                const str = l.read_string();
                const string = l.allocator.allocSentinel(u8, str.len, 0) catch @panic("Failed to alloc digit");
                @memcpy(string, str);
                tok = .{ .STRING = .{
                    .alloc = l.allocator,
                    .str = string,
                } };
            },
            0 => {
                tok = .EOF;
            },
            else => {
                if (is_letter(l.ch)) {
                    const ident = l.read_ident();
                    return Token.from_ident(ident, l.allocator) catch @panic("Failed to alloc ident");
                }
                if (is_digit(l.ch)) {
                    const int = l.read_number();
                    const digit = l.allocator.allocSentinel(u8, int.len, 0) catch @panic("Failed to alloc digit");
                    @memcpy(digit, int);
                    return Token{ .INT = .{
                        .alloc = l.allocator,
                        .int = digit,
                    } };
                } else {
                    tok = Token{ .ILLEGAL = l.ch };
                }
            },
        }

        l.read_char();
        return tok;
    }

    fn read_string(l: *Lexer) []const u8 {
        const position = l.position + 1;
        while (true) {
            l.read_char();
            if (l.ch == '"' or l.ch == 0) {
                break;
            }
        }
        return l.input[position..l.position];
    }

    fn read_ident(l: *Lexer) []const u8 {
        const position = l.position;
        while (is_letter(l.ch)) {
            l.read_char();
        }
        return l.input[position..l.position];
    }

    fn read_number(l: *Lexer) []const u8 {
        const position = l.position;
        while (is_digit(l.ch)) {
            l.read_char();
        }
        return l.input[position..l.position];
    }

    inline fn is_letter(ch: u8) bool {
        return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
    }

    inline fn is_digit(ch: u8) bool {
        return '0' <= ch and ch <= '9';
    }

    fn skip_whitespace(l: *Lexer) void {
        while (l.ch == ' ' or l.ch == '\t' or l.ch == '\n' or l.ch == '\r') {
            l.read_char();
        }
    }

    fn peek_char(l: *Lexer) u8 {
        if (l.readPosition >= l.input.len) {
            return 0;
        } else {
            return l.input[l.readPosition];
        }
    }
};

test "Next Token" {
    const Token = token.Token;

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const input =
        \\ let five = 5;
        \\ let ten = 10;
        \\
        \\ let add = fn(x, y) {
        \\   x + y;
        \\ };
        \\
        \\ let result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\ if (5 < 10) {
        \\   return true;
        \\ } else {
        \\   return false;
        \\ }
        \\
        \\ 10 == 10;
        \\ 10 != 9;
        \\ "foobar"
        \\ "foo bar"
        \\ [1, 2];
    ;

    const tests = [_]Token{
        .LET,
        try Token.test_new_ident("five", alloc),
        .ASSIGN,
        try Token.test_new_int("5", alloc),
        .SEMICOLON,
        .LET,
        try Token.test_new_ident("ten", alloc),
        .ASSIGN,
        try Token.test_new_int("10", alloc),
        .SEMICOLON,
        .LET,
        try Token.test_new_ident("add", alloc),
        .ASSIGN,
        .FUNCTION,
        .LPAREN,
        try Token.test_new_ident("x", alloc),
        .COMMA,
        try Token.test_new_ident("y", alloc),
        .RPAREN,
        .LBRACE,
        try Token.test_new_ident("x", alloc),
        .PLUS,
        try Token.test_new_ident("y", alloc),
        .SEMICOLON,
        .RBRACE,
        .SEMICOLON,
        .LET,
        try Token.test_new_ident("result", alloc),
        .ASSIGN,
        try Token.test_new_ident("add", alloc),
        .LPAREN,
        try Token.test_new_ident("five", alloc),
        .COMMA,
        try Token.test_new_ident("ten", alloc),
        .RPAREN,
        .SEMICOLON,
        .BANG,
        .MINUS,
        .SLASH,
        .ASTERISK,
        try Token.test_new_int("5", alloc),
        .SEMICOLON,
        try Token.test_new_int("5", alloc),
        .LT,
        try Token.test_new_int("10", alloc),
        .GT,
        try Token.test_new_int("5", alloc),
        .SEMICOLON,
        .IF,
        .LPAREN,
        try Token.test_new_int("5", alloc),
        .LT,
        try Token.test_new_int("10", alloc),
        .RPAREN,
        .LBRACE,
        .RETURN,
        .TRUE,
        .SEMICOLON,
        .RBRACE,
        .ELSE,
        .LBRACE,
        .RETURN,
        .FALSE,
        .SEMICOLON,
        .RBRACE,
        try Token.test_new_int("10", alloc),
        .EQ,
        try Token.test_new_int("10", alloc),
        .SEMICOLON,
        try Token.test_new_int("10", alloc),
        .NOT_EQ,
        try Token.test_new_int("9", alloc),
        .SEMICOLON,
        try Token.test_new_str("foobar", alloc),
        try Token.test_new_str("foo bar", alloc),
        .LBRACKET,
        try Token.test_new_int("1", alloc),
        .COMMA,
        try Token.test_new_int("2", alloc),
        .RBRACKET,
        .SEMICOLON,
        .EOF,
    };

    var l = Lexer.init(input, alloc);

    for (tests, 0..) |tt, i| {
        const tok = l.next_token();

        try tokenEqual(i, tt, tok);
    }
}

const TokenEqualErr = error{
    DiffIdent,
    NotIdent,
    DiffInt,
    NotInt,
    NotSame,
};

fn tokenEqual(i: usize, expected: token.Token, actual: token.Token) !void {
    switch (expected) {
        .IDENT => |expected_value| {
            switch (actual) {
                .IDENT => |actual_value| {
                    if (!std.mem.eql(u8, expected_value.ident, actual_value.ident)) {
                        std.debug.print("[{}]: ", .{i});
                        std.debug.print(
                            "Expected ident: '{s}', got ident: '{s}'\n",
                            .{ expected_value.ident, actual_value.ident },
                        );
                        return error.DiffIdent;
                    }
                },
                else => {
                    std.debug.print("[{}]: ", .{i});
                    std.debug.print(
                        "Expected IDENT({s}), got={s}\n",
                        .{ expected_value.ident, @tagName(actual) },
                    );
                    return error.NotIdent;
                },
            }
        },
        .INT => |expected_value| {
            switch (actual) {
                .INT => |actual_value| {
                    if (!std.mem.eql(u8, expected_value.int, actual_value.int)) {
                        std.debug.print("[{}]: ", .{i});
                        std.debug.print(
                            "Expected INT('{s}'), got INT('{s}')\n",
                            .{ expected_value.int, actual_value.int },
                        );
                        return error.DiffInt;
                    }
                },
                else => {
                    std.debug.print("[{}]: ", .{i});
                    std.debug.print(
                        "Expected INT({s}), got={s}\n",
                        .{ expected_value.int, @tagName(actual) },
                    );
                    return error.NotInt;
                },
            }
        },
        else => {
            if (!std.mem.eql(u8, @tagName(expected), @tagName(actual))) {
                std.debug.print("[{}]: ", .{i});
                std.debug.print(
                    "Expected token: {s}, got token: {s}\n",
                    .{ @tagName(expected), @tagName(actual) },
                );
                return error.NotSame;
            }
        },
    }
}
