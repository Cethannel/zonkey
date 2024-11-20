const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const utils = @import("utils.zig");

const evalError = error{
    UnkownExpression,
    UnkownStatement,
};

const TRUE = object.Object{ .Boolean = .{ .value = true } };
const FALSE = object.Object{ .Boolean = .{ .value = false } };

pub fn eval(
    node: anytype,
    env: *object.Environment,
) anyerror!object.Object {
    switch (@TypeOf(node)) {
        ast.Program => {
            return evalProgram(node, env);
        },
        ast.Block => {
            return evalBlockStatements(node, env);
        },
        ast.Statement => {
            switch (@as(ast.Statement, node)) {
                .Expression => |expr| {
                    return eval(expr.expression, env);
                },
                .Block => |expr| {
                    return evalBlockStatements(expr, env);
                },
                .Return => |expr| {
                    const val = try env.alloc.create(object.Object);
                    errdefer env.alloc.destroy(val);
                    val.* = try eval(expr.returnValue, env);
                    if (isError(val.*)) {
                        defer env.alloc.destroy(val);
                        return val.*;
                    }
                    return object.Object{
                        .ReturnValue = .{ .value = val },
                    };
                },
                .Let => |stmt| {
                    const val = try eval(stmt.value, env);
                    if (isError(val)) {
                        return val;
                    }
                    try env.set(stmt.name.value, val);
                    return .Null;
                },
            }
        },
        ast.Expression, ?ast.Expression, ?*ast.Expression => {
            const expOut = removePointer(node);
            switch (@as(ast.Expression, expOut.?)) {
                .Integer => |int| {
                    return object.Object{ .Integer = .{ .value = int.value } };
                },
                .Boolean => |boolean| {
                    return object.Object{ .Boolean = .{ .value = boolean.value } };
                },
                .Prefix => |prefix| {
                    const right = try eval(prefix.right, env);
                    if (isError(right)) {
                        return right;
                    }
                    return evalPrefixExpression(prefix.operator, right, env);
                },
                .Infix => |infix| {
                    const left = try eval(infix.left, env);
                    if (isError(left)) {
                        return left;
                    }
                    const right = try eval(infix.right, env);
                    if (isError(right)) {
                        return right;
                    }
                    return evalInfixExpression(infix.operator, left, right, env);
                },
                .If => {
                    return evalIfExpression(expOut.?, env);
                },
                .Ident => |ident| {
                    return evalIdentifier(ident, env);
                },
                .FunctionLiteral => |func| {
                    const params = try utils.clone_new_alloc(func.parameters, env.alloc);
                    const body = try utils.clone_new_alloc(func.body, env.alloc);
                    return object.Object{
                        .Function = .{
                            .parameters = params,
                            .body = body,
                            .env = env,
                        },
                    };
                },
                .Call => |call| {
                    const function = try eval(call.function, env);
                    if (isError(function)) {
                        return function;
                    }
                    var args = try evalExpressions(call.arguments, env);
                    defer args.clearAndFree();
                    if (args.items.len == 1 and isError(args.items[0])) {
                        return args.items[0];
                    }
                    const out = try applyFunction(function, args, env);
                    return utils.clone_new_alloc(out, env.alloc);
                },
                .String => |str| {
                    return object.Object{
                        .String = .{ .value = str.value },
                    };
                },
                .Array => |arr| {
                    var elements = try evalExpressions(arr.elements, env);
                    if (elements.items.len == 1 and isError(elements.items[0])) {
                        defer elements.clearAndFree();
                        return elements.items[0];
                    }
                    return object.Object{ .Array = .{ .elements = elements } };
                },
                .Index => |idx| {
                    const left = try eval(idx.left, env);
                    if (isError(left)) {
                        return left;
                    }
                    const index = try eval(idx.index, env);
                    if (isError(index)) {
                        return index;
                    }
                    return evalIndexExpression(left, index, env);
                },
                //else => unreachable,
            }
        },
        else => {
            @panic("Unkown type: " ++ @typeName(@TypeOf(node)));
        },
    }
}

fn evalIndexExpression(
    left: object.Object,
    index: object.Object,
    env: *object.Environment,
) !object.Object {
    if (equalTag(left, .Array) and equalTag(index, .Integer)) {
        return evalArrayIndexExpression(left, index);
    } else {
        return newError(
            env,
            "index operation not supported: {s}",
            .{@tagName(left)},
        );
    }
}

fn evalArrayIndexExpression(
    array: object.Object,
    index: object.Object,
) object.Object {
    const arrayObject = array.Array;
    const idx = index.Integer.value;
    const max = @as(i64, @intCast(arrayObject.elements.items.len - 1));

    if (idx < @as(i64, 0) or idx > max) {
        return .Null;
    }

    return arrayObject.elements.items[@intCast(idx)];
}

fn applyFunction(
    node: object.Object,
    args: std.ArrayList(object.Object),
    env: *object.Environment,
) !object.Object {
    switch (node) {
        .Function => {
            var extendedEnv = try extendFunctionEnv(node, args);
            defer extendedEnv.deinit();
            const evaluatod = try eval(node.Function.body, &extendedEnv);
            std.log.info("Returning: {any}\n", .{evaluatod});
            return unwrapReturnValue(evaluatod);
        },
        .Builtin => |fun| {
            return try fun.fun(args.items, env);
        },
        else => return newError(env, "not a function: {s}", .{@tagName(node)}),
    }
}

fn extendFunctionEnv(
    funcution: object.Object,
    args: std.ArrayList(object.Object),
) !object.Environment {
    const func = funcution.Function;
    var env = @as(object.Environment, func.env.init_enclosed());

    for (func.parameters.items, 0..) |param, i| {
        try env.set(param.value, args.items[i]);
    }
    return env;
}

fn unwrapReturnValue(obj: object.Object) object.Object {
    switch (obj) {
        .ReturnValue => |val| {
            return val.value.*;
        },
        else => {
            return obj;
        },
    }
}

fn evalIfExpression(expr: ast.Expression, env: *object.Environment) !object.Object {
    const ifExpr = expr.If;
    const condition = try eval(ifExpr.condition, env);
    if (isError(condition)) {
        return condition;
    }

    if (isTruthy(condition)) {
        return eval(ifExpr.consequest, env);
    } else if (ifExpr.alternative) |alt| {
        return eval(alt, env);
    } else {
        return .Null;
    }
}

fn isTruthy(obj: object.Object) bool {
    switch (obj) {
        .Null => return false,
        .Boolean => |boolean| return boolean.value,
        else => return true,
    }
}

const chm = @import("comptime_hash_map");

fn GenBuiltins() type {
    const funcs = struct {
        fn len(args: []object.Object, env: *object.Environment) !object.Object {
            if (args.len != 1) {
                return newError(
                    env,
                    "wrong number of arguments. got={}, want=1",
                    .{args.len},
                );
            }

            switch (args[0]) {
                .String => |str| {
                    return object.Object{ .Integer = .{
                        .value = @intCast(str.value.len),
                    } };
                },
                .Array => |arr| {
                    return object.Object{ .Integer = .{
                        .value = @intCast(arr.elements.items.len),
                    } };
                },
                else => {
                    return newError(
                        env,
                        "argument to 'len' not supported, got {s}",
                        .{@tagName(args[0])},
                    );
                },
            }
        }

        fn first(args: []object.Object, env: *object.Environment) !object.Object {
            if (args.len != 1) {
                return newError(
                    env,
                    "wrong number of arguments. got={}, want=1",
                    .{args.len},
                );
            }

            switch (args[0]) {
                .Array => |arr| {
                    if (arr.elements.items.len > 0) {
                        return arr.elements.items[0];
                    }
                    return .Null;
                },
                else => {
                    return newError(
                        env,
                        "argument to 'first' myst be Array, got {s}",
                        .{@tagName(args[0])},
                    );
                },
            }
        }

        fn last(args: []object.Object, env: *object.Environment) !object.Object {
            if (args.len != 1) {
                return newError(
                    env,
                    "wrong number of arguments. got={}, want=1",
                    .{args.len},
                );
            }

            switch (args[0]) {
                .Array => |arr| {
                    const length = arr.elements.items.len;
                    if (length > 0) {
                        return arr.elements.items[length - 1];
                    }
                    return .Null;
                },
                else => {
                    return newError(
                        env,
                        "argument to 'last' myst be Array, got {s}",
                        .{@tagName(args[0])},
                    );
                },
            }
        }

        fn rest(args: []object.Object, env: *object.Environment) !object.Object {
            if (args.len != 1) {
                return newError(
                    env,
                    "wrong number of arguments. got={}, want=1",
                    .{args.len},
                );
            }

            switch (args[0]) {
                .Array => |arr| {
                    const length = arr.elements.items.len;
                    if (length > 0) {
                        var newElems = std.ArrayList(object.Object) //
                            .init(env.alloc);
                        try newElems.resize(length - 1);
                        @memcpy(newElems.items, arr.elements.items[1..]);
                        return object.Object{
                            .Array = .{ .elements = newElems },
                        };
                    }
                    return .Null;
                },
                else => {
                    return newError(
                        env,
                        "argument to 'last' myst be Array, got {s}",
                        .{@tagName(args[0])},
                    );
                },
            }
        }

        fn push(args: []object.Object, env: *object.Environment) !object.Object {
            if (args.len != 2) {
                return newError(
                    env,
                    "wrong number of arguments. got={}, want=2",
                    .{args.len},
                );
            }

            switch (args[0]) {
                .Array => |arr| {
                    const length = arr.elements.items.len;
                    if (length > 0) {
                        var newElems = std.ArrayList(object.Object) //
                            .init(env.alloc);
                        try newElems.resize(length);
                        @memcpy(newElems.items, arr.elements.items);
                        try newElems.append(args[1]);
                        return object.Object{
                            .Array = .{ .elements = newElems },
                        };
                    }
                    return .Null;
                },
                else => {
                    return newError(
                        env,
                        "argument to 'push' myst be Array, got {s}",
                        .{@tagName(args[0])},
                    );
                },
            }
        }
    };

    return chm.ComptimeStringHashMap(object.Object, .{
        .{
            "len",
            object.Object{
                .Builtin = .{ .fun = funcs.len },
            },
        },
        .{
            "first",
            object.Object{
                .Builtin = .{ .fun = funcs.first },
            },
        },
        .{
            "last",
            object.Object{
                .Builtin = .{ .fun = funcs.last },
            },
        },
        .{
            "rest",
            object.Object{
                .Builtin = .{ .fun = funcs.rest },
            },
        },
        .{
            "push",
            object.Object{
                .Builtin = .{ .fun = funcs.push },
            },
        },
    });
}
const BuiltIns = GenBuiltins();

const InfixCase = enum {
    @"+",
    @"-",
    @"*",
    @"/",
    @"<",
    @">",
    @"==",
    @"!=",
};

inline fn equalTag(first: anytype, second: anytype) bool {
    return std.mem.eql(u8, @tagName(first), @tagName(second));
}

fn evalInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    if (equalTag(left, .Integer) and equalTag(right, .Integer)) {
        return evalIntegerInfixExpression(operator, left, right, env);
    } else if (equalTag(left, .String) and equalTag(right, .String)) {
        return evalStringInfixExpression(operator, left, right, env);
    } else if (strEql(operator, "==")) {
        if (std.meta.eql(left, right)) {
            return TRUE;
        } else {
            return FALSE;
        }
    } else if (strEql(operator, "!=")) {
        if (!std.meta.eql(left, right)) {
            return TRUE;
        } else {
            return FALSE;
        }
    } else if (!equalTag(left, right)) {
        return newError(env, "type missmatch: {s} {s} {s}", .{
            @tagName(left),
            operator,
            @tagName(right),
        });
    } else {
        return newError(env, "unkown operator: {s} {s} {s}", .{
            @tagName(left),
            operator,
            @tagName(right),
        });
    }
}

inline fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn evalStringInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    if (!strEql(operator, "+")) {
        return newError(env, "unkown operator: {s} {s} {s}", .{
            @tagName(left),
            operator,
            @tagName(right),
        });
    }
    const leftVal = left.String.value;
    const rightVal = right.String.value;

    const outStr = try std.mem.concat(env.alloc, u8, &.{ leftVal, rightVal });
    return object.Object{ .String = .{ .value = outStr } };
}

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    const leftVal = left.Integer.value;
    const rightVal = right.Integer.value;

    const case = std.meta.stringToEnum(InfixCase, operator) //
    orelse return newError(env, "unkown operator: {s} {s} {s}", .{
        @tagName(left),
        operator,
        @tagName(right),
    });
    switch (case) {
        .@"+" => {
            const out = object.Object{ .Integer = .{ .value = leftVal + rightVal } };
            std.log.info("Reurning add: {any}\n", .{out});
            return out;
        },
        .@"-" => {
            return object.Object{ .Integer = .{ .value = leftVal - rightVal } };
        },
        .@"*" => {
            return object.Object{ .Integer = .{ .value = leftVal * rightVal } };
        },
        .@"/" => {
            return object.Object{
                .Integer = .{ .value = @divTrunc(leftVal, rightVal) },
            };
        },
        .@"<" => {
            if (leftVal < rightVal) {
                return TRUE;
            } else {
                return FALSE;
            }
        },
        .@">" => {
            if (leftVal > rightVal) {
                return TRUE;
            } else {
                return FALSE;
            }
        },
        .@"==" => {
            if (leftVal == rightVal) {
                return TRUE;
            } else {
                return FALSE;
            }
        },
        .@"!=" => {
            if (leftVal != rightVal) {
                return TRUE;
            } else {
                return FALSE;
            }
        },
    }
}

fn removePointer(input: anytype) ?ast.Expression {
    switch (@TypeOf(input)) {
        ?ast.Expression, ast.Expression => return input,
        ?*ast.Expression => {
            if (input) |val| {
                return val.*;
            } else {
                return null;
            }
        },
        else => {
            @panic("Unkown expression type: " ++ @typeName(@TypeOf(input)));
        },
    }
}

fn evalExpressions(
    exps: std.ArrayList(ast.Expression),
    env: *object.Environment,
) !std.ArrayList(object.Object) {
    var out = std.ArrayList(object.Object).init(env.alloc);

    for (exps.items) |e| {
        const evaluated = try eval(e, env);
        if (isError(evaluated)) {
            out.clearAndFree();
            try out.append(evaluated);
            return out;
        }
        try out.append(evaluated);
    }

    return out;
}

fn evalPrefixExpression(
    operator: []const u8,
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    const Case = enum { @"!", @"-" };
    const case = std.meta.stringToEnum(Case, operator) //
    orelse return newError(
        env,
        "type missmatch: {s} {s}",
        .{
            operator,
            @tagName(right),
        },
    );
    switch (case) {
        .@"!" => {
            return evalBangOperatorExpression(right);
        },
        .@"-" => {
            return evalMinusOperatorExpression(right, env);
        },
    }
}

fn evalBangOperatorExpression(right: object.Object) object.Object {
    switch (right) {
        .Boolean => |boolean| {
            return object.Object{ .Boolean = .{ .value = !boolean.value } };
        },
        .Null => {
            return TRUE;
        },
        else => {
            return FALSE;
        },
    }
}

fn evalMinusOperatorExpression(
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    switch (right) {
        .Integer => |int| {
            return object.Object{ .Integer = .{ .value = -int.value } };
        },
        else => return newError(env, "unkown operator: -{s}", .{@tagName(right)}),
    }
}
fn evalProgram(
    program: ast.Program,
    env: *object.Environment,
) !object.Object {
    var result: object.Object = undefined;

    for (program.statements.items) |stmt| {
        result = try eval(stmt, env);

        if (equalTag(result, object.Object.ReturnValue)) {
            const out = result.ReturnValue.value.*;
            env.alloc.destroy(result.ReturnValue.value);

            return out;
        } else if (equalTag(result, .Error)) {
            return result;
        }
    }

    return result;
}

fn evalBlockStatements(
    block: ast.Block,
    env: *object.Environment,
) !object.Object {
    var result: object.Object = undefined;

    for (block.statements.items) |stmt| {
        result = try eval(stmt, env);

        if (equalTag(result, object.Object.ReturnValue) or equalTag(result, .Error)) {
            return result;
        }
    }

    return result;
}

fn isError(obj: object.Object) bool {
    return equalTag(obj, .Error);
}

fn newError(
    env: *object.Environment,
    comptime fmt: []const u8,
    args: anytype,
) !object.Object {
    const message = try std.fmt.allocPrint(env.alloc, fmt, args);
    return object.Object{ .Error = .{ .message = message } };
}

fn evalIdentifier(
    node: ast.Identifier,
    env: *object.Environment,
) !object.Object {
    if (env.get(node.value)) |val| {
        return val;
    } else if (BuiltIns.get(node.value)) |builtin| {
        return builtin.*;
    } else {
        return newError(env, "identifier not found: {s}", .{node.value});
    }
}

test "Eval Integer Expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{
            .input = "5",
            .expected = 5,
        },
        .{
            .input = "10",
            .expected = 10,
        },
        .{
            .input = "-5",
            .expected = -5,
        },
        .{
            .input = "-10",
            .expected = -10,
        },
        .{
            .input = "5 + 5 + 5 + 5 - 10",
            .expected = 10,
        },
        .{
            .input = "2 * 2 * 2 * 2 * 2",
            .expected = 32,
        },
        .{
            .input = "-50 + 100 + -50",
            .expected = 0,
        },
        .{
            .input = "5 * 2 + 10",
            .expected = 20,
        },
        .{
            .input = "5 + 2 * 10",
            .expected = 25,
        },
        .{
            .input = "20 + 2 * -10",
            .expected = 0,
        },
        .{
            .input = "50 / 2 * 2 + 10",
            .expected = 60,
        },
        .{
            .input = "2 * (5 + 10)",
            .expected = 30,
        },
        .{
            .input = "3 * 3 * 3 + 10",
            .expected = 37,
        },
        .{
            .input = "3 * (3 * 3) + 10",
            .expected = 37,
        },
        .{
            .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            .expected = 50,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testIntegerObject(evaluated, tt.expected);
    }
}

fn testEval(input: []const u8, alloc: std.mem.Allocator) !object.Object {
    const l = lexer.Lexer.init(input, alloc);
    var p = try parser.Parser.init(l);
    const program = try p.parse_program();
    var env = object.Environment.init(alloc);
    return eval(program, &env);
}

fn testIntegerObject(obj: object.Object, expected: i64) !void {
    const result = obj.Integer;

    try std.testing.expectEqual(expected, result.value);
}

test "Eval Boolean Expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{
            .input = "true",
            .expected = true,
        },
        .{
            .input = "false",
            .expected = false,
        },
        .{
            .input = "1 < 2",
            .expected = true,
        },
        .{
            .input = "1 > 2",
            .expected = false,
        },
        .{
            .input = "1 < 1",
            .expected = false,
        },
        .{
            .input = "1 > 1",
            .expected = false,
        },
        .{
            .input = "1 == 1",
            .expected = true,
        },
        .{
            .input = "1 != 1",
            .expected = false,
        },
        .{
            .input = "1 == 2",
            .expected = false,
        },
        .{
            .input = "1 != 2",
            .expected = true,
        },
        .{
            .input = "true == true",
            .expected = true,
        },
        .{
            .input = "false == false",
            .expected = true,
        },
        .{
            .input = "true == false",
            .expected = false,
        },
        .{
            .input = "true != false",
            .expected = true,
        },
        .{
            .input = "false != true",
            .expected = true,
        },
        .{
            .input = "(1 < 2) == true",
            .expected = true,
        },
        .{
            .input = "(1 < 2) == false",
            .expected = false,
        },
        .{
            .input = "(1 > 2) == true",
            .expected = false,
        },
        .{
            .input = "(1 > 2) == false",
            .expected = true,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testBooleanObject(evaluated, tt.expected);
    }
}

fn testBooleanObject(obj: object.Object, expected: bool) !void {
    const result = obj.Boolean;

    try std.testing.expectEqual(expected, result.value);
}

test "Eval Bang Operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{
            .input = "!true",
            .expected = false,
        },
        .{
            .input = "!false",
            .expected = true,
        },
        .{
            .input = "!5",
            .expected = false,
        },
        .{
            .input = "!!true",
            .expected = true,
        },
        .{
            .input = "!!false",
            .expected = false,
        },
        .{
            .input = "!!5",
            .expected = true,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testBooleanObject(evaluated, tt.expected);
    }
}

test "If Else Expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{
            .input = "if (true) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (false) { 10 }",
            .expected = null,
        },
        .{
            .input = "if (1) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (1 < 2) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (1 > 2) { 10 }",
            .expected = null,
        },
        .{
            .input = "if (1 > 2) { 10 } else { 20 }",
            .expected = 20,
        },
        .{
            .input = "if (1 < 2) { 10 } else { 20 }",
            .expected = 10,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        if (tt.expected) |int| {
            try testIntegerObject(evaluated, int);
        } else {
            try testNullObject(evaluated);
        }
    }
}

fn testNullObject(obj: object.Object) !void {
    try std.testing.expectEqualStrings(@tagName(.Null), @tagName(obj));
}

test "Return Statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{
            .input = "return 10;",
            .expected = 10,
        },
        .{
            .input = "return 10; 9;",
            .expected = 10,
        },
        .{
            .input = "return 2 * 5; 9;",
            .expected = 10,
        },
        .{
            .input = "9; return 2 * 5; 9",
            .expected = 10,
        },
        .{
            .input =
            \\ if (10 > 1) {
            \\   if (10 > 1) {
            \\      return 10;
            \\   }
            \\
            \\   return 1;
            \\ }
            ,
            .expected = 10,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testIntegerObject(evaluated, tt.expected);
    }
}

test "Error Handling" {
    const tests = [_]struct {
        input: []const u8,
        expectedMessage: []const u8,
    }{
        .{
            .input = "5 + true",
            .expectedMessage = "type missmatch: Integer + Boolean",
        },
        .{
            .input = "5 + true; 5;",
            .expectedMessage = "type missmatch: Integer + Boolean",
        },
        .{
            .input = "-true",
            .expectedMessage = "unkown operator: -Boolean",
        },
        .{
            .input = "true + false;",
            .expectedMessage = "unkown operator: Boolean + Boolean",
        },
        .{
            .input = "5; true + false; 5",
            .expectedMessage = "unkown operator: Boolean + Boolean",
        },
        .{
            .input = "if (10 > 1) { true + false; }",
            .expectedMessage = "unkown operator: Boolean + Boolean",
        },
        .{
            .input =
            \\ if (10 > 1) {
            \\   if (10 > 1) {
            \\      return true + false;
            \\   }
            \\
            \\   return 1;
            \\ }
            ,
            .expectedMessage = "unkown operator: Boolean + Boolean",
        },
        .{
            .input = "foobar",
            .expectedMessage = "identifier not found: foobar",
        },
        .{
            .input =
            \\ "hello" - "world"
            ,
            .expectedMessage = "unkown operator: String - String",
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        const errorObj = evaluated.Error;
        try std.testing.expectEqualStrings(tt.expectedMessage, errorObj.message);
    }
}

test "Let Statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{
            .input = "let a = 5; a;",
            .expected = 5,
        },
        .{
            .input = "let a = 5 * 5; a;",
            .expected = 25,
        },
        .{
            .input = "let a = 5; let b = a; b;",
            .expected = 5,
        },
        .{
            .input = "let a = 5; let b = a; let c = a + b + 5; c;",
            .expected = 15,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testIntegerObject(evaluated, tt.expected);
    }
}

test "Function Object" {
    const input = "fn(x) { x + 2; };";
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const evaluated = try testEval(input, alloc);

    const fun = evaluated.Function;

    try std.testing.expectEqual(1, fun.parameters.items.len);
    try std.testing.expectEqualStrings("x", fun.parameters.items[0].string());

    const expectedBody = "(x + 2)";

    try std.testing.expectEqualStrings(expectedBody, try fun.body.string(alloc));
}

test "Function Application" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{
            .input = "let identity = fn(x) { x; }; identity(5)",
            .expected = 5,
        },
        .{
            .input = "let identity = fn(x) { return x; }; identity(5)",
            .expected = 5,
        },
        .{
            .input = "let double = fn(x) { x * 2; }; double(5)",
            .expected = 10,
        },
        .{
            .input = "let add = fn(x, y) { x + y; }; add(5, 5)",
            .expected = 10,
        },
        .{
            .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5))",
            .expected = 20,
        },
        .{
            .input = "fn(x) { x; }(5)",
            .expected = 5,
        },
        .{
            .input = "let a = fn(a, b) { a + b; }; a(1, 3)",
            .expected = 4,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testIntegerObject(evaluated, tt.expected);
    }
}

test "String Literal" {
    const input =
        \\"Hello World!"
    ;
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();
    const evaluated = try testEval(input, alloc);
    const str = evaluated.String;
    try std.testing.expectEqualStrings("Hello World!", str.value);
}

test "String Concatination" {
    const input =
        \\"Hello" + " " + "World!"
    ;
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();
    const evaluated = try testEval(input, alloc);
    const str = evaluated.String;
    try std.testing.expectEqualStrings("Hello World!", str.value);
}

const valType = union(enum) {
    int: i64,
    str: []const u8,
};

test "Builtin Function" {
    const tests = [_]struct {
        input: []const u8,
        expected: valType,
    }{
        .{
            .input =
            \\len("")
            ,
            .expected = .{ .int = 0 },
        },
        .{
            .input =
            \\len("four")
            ,
            .expected = .{ .int = 4 },
        },
        .{
            .input =
            \\len("hello world")
            ,
            .expected = .{ .int = 11 },
        },
        .{
            .input =
            \\len(1)
            ,
            .expected = .{ .str = "argument to 'len' not supported, got Integer" },
        },
        .{
            .input =
            \\len("one", "two")
            ,
            .expected = .{ .str = "wrong number of arguments. got=2, want=1" },
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        switch (tt.expected) {
            .int => |expected| {
                try testIntegerObject(evaluated, expected);
            },
            .str => |expected| {
                const errorObj = evaluated.Error;
                try std.testing.expectEqualStrings(expected, errorObj.message);
            },
        }
    }
}

test "Array Literals" {
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();
    const input = "[1, 2 * 2, 3 + 3]";
    const evaluated = try testEval(input, alloc);
    const result = evaluated.Array;

    try std.testing.expectEqual(3, result.elements.items.len);
    try testIntegerObject(result.elements.items[0], 1);
    try testIntegerObject(result.elements.items[1], 4);
    try testIntegerObject(result.elements.items[2], 6);
}

test "Array Index Expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{
            .input = "[1, 2, 3][0]",
            .expected = 1,
        },
        .{
            .input = "[1, 2, 3][1]",
            .expected = 2,
        },
        .{
            .input = "[1, 2, 3][2]",
            .expected = 3,
        },
        .{
            .input = "let i = 0; [1][i];",
            .expected = 1,
        },
        .{
            .input = "[1, 2, 3][1 + 1];",
            .expected = 3,
        },
        .{
            .input = "let myArray = [1, 2, 3]; myArray[2];",
            .expected = 3,
        },
        .{
            .input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            .expected = 6,
        },
        .{
            .input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            .expected = 2,
        },
        .{
            .input = "[1, 2, 3][3]",
            .expected = null,
        },
        .{
            .input = "[1, 2, 3][-1]",
            .expected = null,
        },
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        if (tt.expected) |expected| {
            try testIntegerObject(evaluated, expected);
        } else {
            try testNullObject(evaluated);
        }
    }
}

test "Environment Free" {
    const input =
        \\ let a = fn(a,b) {a+b;};
        \\ a(1,2)
    ;

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const galloc = arenaAlloc.allocator();
    var env = object.Environment.init(galloc);
    defer env.deinit();

    var lines = std.mem.split(u8, input, "\n");
    var last: object.Object = object.Object.Null;

    while (lines.next()) |line| {
        var arena = std.heap.ArenaAllocator.init(galloc);
        const alloc = arena.allocator();

        const l = lexer.Lexer.init(line, alloc);
        var p = parser.Parser.init(l) catch unreachable;
        const program = p.parse_program() catch unreachable;
        if (p.errors.items.len != 0) {
            printParseErrors(p.errors);
            @panic("Had errors");
        }

        const evaluated = try eval(program, &env);
        switch (evaluated) {
            .Error => |err| {
                @panic(err.message);
            },
            else => {
                last = evaluated;
            },
        }
    }

    try testIntegerObject(last, 3);
}

fn printParseErrors(
    errors: parser.Parser.Errors,
) void {
    for (errors.items) |err| {
        std.debug.print("\t{s}\n", .{err});
    }
}
