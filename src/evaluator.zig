const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

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
                    const params = func.parameters;
                    const body = func.body;
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
                    if (args.items.len == 1 and isError(args.items[0])) {
                        defer args.clearAndFree();
                        return args.items[0];
                    }
                    return applyFunction(function, args, env);
                },
            }
        },
        else => {
            @panic("Unkown type: " ++ @typeName(@TypeOf(node)));
        },
    }
}

fn applyFunction(
    node: object.Object,
    args: std.ArrayList(object.Object),
    env: *object.Environment,
) !object.Object {
    switch (node) {
        .Function => {},
        else => return newError(env, "not a function: {s}", .{@tagName(node)}),
    }

    var extendedEnv = try extendFunctionEnv(node, args);
    defer extendedEnv.deinit();
    const evaluatod = try eval(node.Function.body, &extendedEnv);
    return unwrapReturnValue(evaluatod);
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

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: object.Object,
    right: object.Object,
    env: *object.Environment,
) !object.Object {
    const leftVal = left.Integer.value;
    const rightVal = right.Integer.value;

    const case = std.meta.stringToEnum(InfixCase, operator) //
    orelse return newError(env, "type missmatch: {s} {s} {s}", .{
        @tagName(left),
        operator,
        @tagName(right),
    });
    switch (case) {
        .@"+" => {
            return object.Object{ .Integer = .{ .value = leftVal + rightVal } };
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
    };

    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    for (tests) |tt| {
        const evaluated = try testEval(tt.input, alloc);
        try testIntegerObject(evaluated, tt.expected);
    }
}