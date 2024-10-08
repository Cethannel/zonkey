const std = @import("std");
const ast = @import("ast.zig");

pub const Object = union(enum) {
    Integer: struct {
        value: i64,
    },
    Boolean: struct {
        value: bool,
    },
    Null: void,
    ReturnValue: struct {
        value: *Object,
    },
    Error: struct {
        message: []const u8,
    },
    Function: struct {
        parameters: std.ArrayList(ast.Identifier),
        body: ast.Block,
        env: *Environment,
    },

    pub fn inspect(o: *const Object, alloc: std.mem.Allocator) ![]const u8 {
        switch (o.*) {
            .Integer => |obj| {
                return std.fmt.allocPrint(alloc, "{}", obj);
            },
            .Boolean => |obj| {
                return std.fmt.allocPrint(alloc, "{}", obj);
            },
            .ReturnValue => |obj| {
                return obj.value.inspect(alloc);
            },
            .Null => {
                return "null";
            },
            .Error => |obj| {
                return std.fmt.allocPrint(alloc, "ERROR: {s}", obj);
            },
            .Function => |obj| {
                var out = std.ArrayList(u8).init(alloc);
                var writer = out.writer();

                try writer.writeAll("fn(");
                for (obj.parameters.items, 0..) |param, i| {
                    if (i != 0) {
                        try writer.writeAll(" ,");
                    }
                    try writer.writeAll(param.string());
                }
                try writer.writeAll(") {\n");
                try writer.writeAll(try obj.body.string(alloc));
                try writer.writeAll("\n}");

                return out.items;
            },
        }
    }
};

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) Environment {
        return Environment{
            .store = std.StringHashMap(Object).init(alloc),
            .alloc = alloc,
            .outer = null,
        };
    }

    pub fn init_enclosed(outer: *Environment) Environment {
        var env = Environment.init(outer.alloc);
        env.outer = outer;
        return env;
    }

    pub fn deinit(env: *Environment) void {
        env.store.deinit();
    }

    pub fn get(env: *const Environment, name: []const u8) ?Object {
        var obj = env.store.get(name);
        if (obj == null and env.outer != null) {
            obj = env.outer.?.get(name);
        }

        return obj;
    }

    pub inline fn set(
        env: *Environment,
        name: []const u8,
        value: Object,
    ) !void {
        try env.store.put(name, value);
    }
};
