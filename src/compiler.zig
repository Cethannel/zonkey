const std = @import("std");
const code = @import("code.zig");
const ast = @import("ast.zig");
const utils = @import("utils.zig");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Constants = std.ArrayList(object.Object);

const Compiler = struct {
    instructions: code.Instructions,
    constants: Constants,

    pub fn init(alloc: std.mem.Allocator) @This() {
        return @This(){
            .instructions = code.Instructions.init(alloc),
            .constants = Constants.init(alloc),
        };
    }

    pub fn compile(self: *@This(), node: anytype) !void {
        _ = self;
        _ = node;
    }

    pub fn bytecode(self: *const @This(), alloc: std.mem.Allocator) !Bytecode {
        var instructions: code.Instructions = try utils.clone_new_alloc(self.instructions, alloc);
        errdefer instructions.clearAndFree();
        var constants: Constants = try utils.clone_new_alloc(self.constants, alloc);
        errdefer constants.clearAndFree();

        return Bytecode{
            .instructions = instructions,
            .constants = constants,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.constants.deinit();
        self.instructions.deinit();
    }
};

const Bytecode = struct {
    instructions: code.Instructions,
    constants: Constants,

    pub fn deinit(self: *@This()) void {
        self.constants.deinit();
        self.instructions.deinit();
    }
};

const expectedConstant = union(enum) {
    Int: i64,
};

const testCase = struct {
    input: []const u8,
    expectedConstants: []const expectedConstant,
    expectedInstructions: []const []const u8,
};

test "Integer Arithmetic" {
    var testArena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer testArena.deinit();
    const testAlloc = testArena.allocator();

    const tests = [_]testCase{
        testCase{
            .input = "1 + 2",
            .expectedConstants = &.{
                .{ .Int = 1 },
                .{ .Int = 2 },
            },
            .expectedInstructions = &.{
                try (code.Operation{
                    .Constant = 0,
                }).to_bytes(testAlloc),
                try (code.Operation{
                    .Constant = 1,
                }).to_bytes(testAlloc),
            },
        },
    };

    try runCompilerTests(&tests);
}

fn runCompilerTests(tests: []const testCase) !void {
    for (tests) |tt| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        const alloc = arena.allocator();

        const program: ast.Program = try parse(tt.input, alloc);

        var compiler = Compiler.init(std.testing.allocator);
        defer compiler.deinit();
        try compiler.compile(program);

        var bytecode = try compiler.bytecode(std.testing.allocator);
        defer bytecode.deinit();

        try testInstructions(tt.expectedInstructions, bytecode.instructions, alloc);

        try testConstants(tt.expectedConstants, bytecode.constants);

        arena.deinit();
    }
}

fn parse(input: []const u8, alloc: std.mem.Allocator) !ast.Program {
    const l = lexer.Lexer.init(input, alloc);
    var p = try parser.Parser.init(l);
    return p.parse_program();
}

fn testInstructions(
    expected: []const []const u8,
    actual: code.Instructions,
    alloc: std.mem.Allocator,
) !void {
    const concatted = try std.mem.concat(alloc, u8, expected);
    defer alloc.free(concatted);

    try std.testing.expectEqualSlices(u8, concatted, actual.items);
}

fn testConstants(
    expected: []const expectedConstant,
    actual: Constants,
) !void {
    try std.testing.expectEqual(expected.len, actual.items.len);

    for (expected, 0..) |constant, i| {
        switch (constant) {
            .Int => |int| {
                try testIntegerObject(int, actual.items[i]);
            },
        }
    }
}

fn testIntegerObject(expected: i64, actual: object.Object) !void {
    const integer = actual.Integer;

    try std.testing.expectEqual(expected, integer.value);
}
