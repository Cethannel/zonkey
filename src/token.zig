const std = @import("std");
const chm = @import("comptime_hash_map");

pub const Token = union(enum) {
    ILLEGAL: u8,
    EOF,

    IDENT: struct {
        ident: [:0]const u8,
        alloc: std.mem.Allocator,
    },
    INT: struct {
        int: [:0]const u8,
        alloc: std.mem.Allocator,
    },

    ASSIGN, // =
    PLUS, // +
    MINUS, // -
    BANG, // !
    ASTERISK, // *
    SLASH, // /

    LT, // <
    GT, // >

    COMMA, // ,
    SEMICOLON, // ;

    EQ,
    NOT_EQ,

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    pub fn from_ident(ident: []const u8, alloc: std.mem.Allocator) !Token {
        const toko = IdentMap.get(ident);
        if (toko) |tok| {
            return tok.*;
        }
        const val = try alloc.allocSentinel(u8, ident.len, 0);
        @memcpy(val, ident);
        return Token{ .IDENT = .{
            .ident = val,
            .alloc = alloc,
        } };
    }

    pub fn test_new_ident(ident: []const u8, alloc: std.mem.Allocator) !Token {
        const builtin = @import("builtin");
        if (!builtin.is_test) {
            @panic("This function cannot be used outside of test");
        }
        const val = try alloc.allocSentinel(u8, ident.len, 0);
        @memcpy(val, ident);
        return Token{ .IDENT = .{
            .ident = val,
            .alloc = alloc,
        } };
    }

    pub fn test_new_int(ident: []const u8, alloc: std.mem.Allocator) !Token {
        const builtin = @import("builtin");
        if (!builtin.is_test) {
            @panic("This function cannot be used outside of test");
        }
        const val = try alloc.allocSentinel(u8, ident.len, 0);
        @memcpy(val, ident);
        return Token{ .INT = .{
            .alloc = alloc,
            .int = val,
        } };
    }

    pub fn deinit(self: Token) !void {
        switch (self) {
            .IDENT => |value| {
                value.alloc.free(value.ident);
            },
            .INT => |value| {
                value.alloc.free(value.int);
            },
            else => {},
        }
    }

    /// If IDENT or INT returned string will be allocated with same
    /// allocator that was passed in when creating.
    pub fn to_string(self: Token) ![]const u8 {
        switch (self) {
            .IDENT => |ident| {
                var out = std.ArrayList(u8).init(ident.alloc);

                try std.fmt.format(
                    out.writer(),
                    "IDENT({s})",
                    .{ident.ident},
                );

                return out.items;
            },
            .INT => |int| {
                var out = std.ArrayList(u8).init(int.alloc);
                try std.fmt.format(
                    out.writer(),
                    "IDENT({s})",
                    .{int.int},
                );

                return out.items;
            },
            else => {
                return @tagName(self);
            },
        }
    }
};

const IdentMap = chm.ComptimeStringHashMap(Token, .{
    .{ "fn", .FUNCTION },
    .{ "let", .LET },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
});
