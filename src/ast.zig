const std = @import("std");
const token = @import("token.zig");

pub const Statement = union(enum) {
    Let: struct {
        token: token.Token,
        name: Identifier,
        value: ?Expression,
    },
    Return: struct {
        token: token.Token,
        returnValue: ?Expression,
    },
    Expression: struct {
        token: token.Token,
        expression: ?Expression,
    },
    Block: Block,

    pub fn deinit(self: *Statement) anyerror!void {
        switch (self.*) {
            .Let => |letStmt| {
                return letStmt.token.deinit();
            },
            .Return => |stmt| {
                return stmt.token.deinit();
            },
            .Expression => |stmt| {
                return stmt.token.deinit();
            },
            .Block => |stmt| {
                return stmt.deinit();
            },
        }
    }

    pub fn token_literal(self: *const Statement) anyerror![]const u8 {
        switch (self.*) {
            .Let => |letStmt| {
                return letStmt.token.token_literal();
            },
            .Return => |returnStmt| {
                return returnStmt.token.token_literal();
            },
            .Expression => |stmt| {
                return stmt.token.token_literal();
            },
            .Block => |b| {
                return b.token_literal();
            },
        }
    }

    pub fn string(self: *const Statement, alloc: std.mem.Allocator) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(alloc);
        switch (self.*) {
            .Let => |stmt| {
                try out.writer().writeAll("let ");
                try out.writer().writeAll(stmt.name.string());
                try out.writer().writeAll(" = ");

                if (stmt.value) |value| {
                    try out.writer().writeAll(try value.string(alloc));
                }

                try out.writer().writeAll(";");

                return out.items;
            },
            .Return => |stmt| {
                try out.writer().writeAll("return ");

                if (stmt.returnValue) |value| {
                    try out.writer().writeAll(try value.string(alloc));
                }

                try out.writer().writeAll(";");

                return out.items;
            },
            .Expression => |stmt| {
                if (stmt.expression) |value| {
                    return value.string(alloc);
                }
                return "";
            },
            .Block => |stmt| {
                return stmt.string(alloc);
            },
        }
    }
};

pub const Expression = union(enum) {
    Ident: Identifier,
    Integer: struct {
        token: token.Token,
        value: i64,

        pub fn token_literal(i: @This()) anyerror![]const u8 {
            return i.token.token_literal();
        }
    },
    Prefix: struct {
        token: token.Token,
        operator: []const u8,
        right: ?*Expression,

        pub fn token_literal(pe: @This()) anyerror![]const u8 {
            return pe.token.token_literal();
        }
    },
    Infix: struct {
        token: token.Token,
        left: ?*Expression,
        operator: []const u8,
        right: ?*Expression,

        pub fn token_literal(pe: @This()) anyerror![]const u8 {
            return pe.token.token_literal();
        }
    },
    Boolean: struct {
        token: token.Token,
        value: bool,

        pub fn token_literal(pe: @This()) anyerror![]const u8 {
            return pe.token.token_literal();
        }
    },
    If: struct {
        token: token.Token,
        condition: ?*Expression,
        consequest: Block,
        alternative: ?Block,

        pub fn token_literal(ie: @This()) anyerror![]const u8 {
            return ie.token.token_literal();
        }
    },
    FunctionLiteral: struct {
        token: token.Token,
        parameters: std.ArrayList(Identifier),
        body: Block,

        pub fn token_literal(fl: @This()) anyerror![]const u8 {
            return fl.token.token_literal();
        }
    },
    Call: struct {
        token: token.Token,
        function: ?*Expression,
        arguments: std.ArrayList(Expression),

        pub fn token_literal(c: @This()) anyerror![]const u8 {
            return c.token.token_literal();
        }
    },
    String: struct {
        token: token.Token,
        value: []const u8,

        pub fn token_literal(c: @This()) anyerror![]const u8 {
            return c.token.token_literal();
        }
    },
    Array: struct {
        token: token.Token,
        elements: std.ArrayList(Expression),
    },
    Index: struct {
        token: token.Token,
        left: ?*Expression,
        index: ?*Expression,
    },

    pub fn string(
        self: *const Expression,
        alloc: std.mem.Allocator,
    ) anyerror![]const u8 {
        var innerArenaAlloc = std.heap.ArenaAllocator.init(alloc);
        defer innerArenaAlloc.deinit();
        const innerAlloc = innerArenaAlloc.allocator();
        switch (self.*) {
            .Ident => |expr| {
                return expr.string();
            },
            .Integer => |expr| {
                return expr.token.token_literal();
            },
            .Prefix => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("(");
                try writer.writeAll(expr.operator);
                try writer.writeAll(try expr.right.?.string(innerAlloc));
                try writer.writeAll(")");

                return out.items;
            },
            .Infix => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("(");
                try writer.writeAll(try expr.left.?.string(innerAlloc));
                try writer.writeAll(" ");
                try writer.writeAll(expr.operator);
                try writer.writeAll(" ");
                try writer.writeAll(try expr.right.?.string(innerAlloc));
                try writer.writeAll(")");

                return out.items;
            },
            .Boolean => |expr| {
                return expr.token_literal();
            },
            .If => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("if");
                if (expr.condition) |cond| {
                    try writer.writeAll(try cond.string(innerAlloc));
                }
                try writer.writeAll(" ");
                try writer.writeAll(try expr.consequest.string(innerAlloc));

                if (expr.alternative) |b| {
                    try writer.writeAll("else ");
                    try writer.writeAll(try b.string(innerAlloc));
                }

                return out.items;
            },
            .FunctionLiteral => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll(try expr.token_literal());
                try writer.writeAll("(");
                for (expr.parameters.items, 0..) |value, i| {
                    if (i != 0) {
                        try writer.writeAll(",");
                    }
                    try writer.writeAll(value.string());
                }
                try writer.writeAll(")");
                try writer.writeAll(try expr.body.string(innerAlloc));

                return out.items;
            },
            .Call => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll(try expr.function.?.string(innerAlloc));
                try writer.writeAll("(");
                for (expr.arguments.items, 0..) |value, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.writeAll(try value.string(innerAlloc));
                }
                try writer.writeAll(")");

                return out.items;
            },
            .String => |expr| {
                return expr.token.token_literal();
            },
            .Array => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("[");
                for (expr.elements.items, 0..) |value, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.writeAll(try value.string(innerAlloc));
                }
                try writer.writeAll("]");

                return out.items;
            },
            .Index => |expr| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("(");
                try writer.writeAll(try expr.left.?.string(innerAlloc));
                try writer.writeAll("[");
                try writer.writeAll(try expr.index.?.string(innerAlloc));
                try writer.writeAll("])");

                return out.items;
            },
        }
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn token_literal(i: *const Identifier) anyerror![]const u8 {
        return i.value;
    }

    pub fn string(i: *const Identifier) []const u8 {
        return i.value;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Program {
        return Program{
            .statements = std.ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: *Program) anyerror!void {
        for (self.statements.items) |value| {
            value.deinit();
        }
        self.statements.deinit();
    }

    pub fn String(self: *const Program, alloc: std.mem.Allocator) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(alloc);

        for (self.statements.items) |stmt| {
            try out.writer().writeAll(try stmt.string(alloc));
        }

        return out.items;
    }
};

test "String" {
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    var statements = std.ArrayList(Statement).init(alloc);
    try statements.append(.{
        .Let = .{
            .token = .LET,
            .name = .{
                .token = .{
                    .IDENT = .{
                        .ident = "myVar",
                        .alloc = alloc,
                    },
                },
                .value = "myVar",
            },
            .value = .{
                .Ident = .{
                    .token = .{
                        .IDENT = .{
                            .ident = "anotherVar",
                            .alloc = alloc,
                        },
                    },
                    .value = "anotherVar",
                },
            },
        },
    });

    const program = Program{
        .statements = statements,
    };

    try std.testing.expectEqualStrings(
        "let myVar = anotherVar;",
        try program.String(alloc),
    );
}

pub const Block = struct {
    token: token.Token,
    statements: std.ArrayList(Statement),

    pub fn token_literal(b: *const Block) anyerror![]const u8 {
        return b.token.token_literal();
    }

    pub fn string(b: *const Block, alloc: std.mem.Allocator) anyerror![]const u8 {
        var innerArenaAlloc = std.heap.ArenaAllocator.init(alloc);
        defer innerArenaAlloc.deinit();
        const innerAlloc = innerArenaAlloc.allocator();
        var out = std.ArrayList(u8).init(alloc);
        var writer = out.writer();

        for (b.statements.items) |s| {
            try writer.writeAll(try s.string(innerAlloc));
        }

        return out.items;
    }

    pub fn deinit(b: *const Block) !void {
        try b.token.deinit();
        for (b.statements.items) |stmt| {
            try stmt.deinit();
        }
        try b.statements.deinit();
    }
};
