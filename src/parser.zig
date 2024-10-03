const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const chm = @import("comptime_hash_map");

const prefixParseFn = *const fn (*Parser) ?ast.Expression;
const infixParseFn = *const fn (*Parser, ?ast.Expression) ?ast.Expression;

const Precedence = enum(u8) {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

fn getPrecedence(tok: token.Token) ?Precedence {
    return switch (tok) {
        .EQ => .EQUALS,
        .NOT_EQ => .EQUALS,
        .LT => .LESSGREATER,
        .GT => .LESSGREATER,
        .PLUS => .SUM,
        .MINUS => .SUM,
        .SLASH => .PRODUCT,
        .ASTERISK => .PRODUCT,
        .LPAREN => .CALL,
        else => null,
    };
}

pub const Parser = struct {
    pub const Errors = std.ArrayList([]const u8);

    l: lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,

    errors: Errors,

    prefixParseFns: std.StringHashMap(prefixParseFn),
    infixParseFns: std.StringHashMap(infixParseFn),

    pub fn init(l: lexer.Lexer) !Parser {
        var p = Parser{
            .l = l,
            .cur_token = .EOF,
            .peek_token = .EOF,
            .errors = Errors.init(l.allocator),
            .prefixParseFns = std.StringHashMap(prefixParseFn).init(l.allocator),
            .infixParseFns = std.StringHashMap(infixParseFn).init(l.allocator),
        };

        try p.registerPrefix(token.Token.IDENT, parseIdentifier);
        try p.registerPrefix(token.Token.INT, parseInteger);
        try p.registerPrefix(token.Token.BANG, parsePrefixExpression);
        try p.registerPrefix(token.Token.MINUS, parsePrefixExpression);
        try p.registerPrefix(token.Token.TRUE, parseBoolean);
        try p.registerPrefix(token.Token.FALSE, parseBoolean);
        try p.registerPrefix(token.Token.LPAREN, parseGroupedExpression);
        try p.registerPrefix(token.Token.IF, parseIfExpression);
        try p.registerPrefix(token.Token.FUNCTION, parseFunctionExpression);

        try p.registerInfix(token.Token.PLUS, parseInfixExpression);
        try p.registerInfix(token.Token.MINUS, parseInfixExpression);
        try p.registerInfix(token.Token.SLASH, parseInfixExpression);
        try p.registerInfix(token.Token.ASTERISK, parseInfixExpression);
        try p.registerInfix(token.Token.EQ, parseInfixExpression);
        try p.registerInfix(token.Token.NOT_EQ, parseInfixExpression);
        try p.registerInfix(token.Token.LT, parseInfixExpression);
        try p.registerInfix(token.Token.GT, parseInfixExpression);
        try p.registerInfix(token.Token.LPAREN, parseCallExpression);

        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(p: *const Parser) !void {
        defer p.l.deinit();
        defer p.errors.deinit();
        for (p.errors) |value| {
            p.l.allocator.free(value);
        }
    }

    fn next_token(p: *Parser) void {
        p.cur_token = p.peek_token;
        p.peek_token = p.l.next_token();
    }

    pub fn parse_program(p: *Parser) !ast.Program {
        var program = ast.Program.init(p.l.allocator);

        while (p.cur_token != .EOF) {
            const stmt = p.parseStatement();
            if (stmt) |stmtS| {
                try program.statements.append(stmtS);
            }
            p.next_token();
        }

        return program;
    }

    pub fn get_errors(p: *const Parser) Errors {
        return p.errors;
    }

    fn parseStatement(p: *Parser) ?ast.Statement {
        switch (p.cur_token) {
            .LET => {
                return p.parseLetStatement();
            },
            .RETURN => {
                return p.parseReturnStatement();
            },
            else => {
                return p.parseExpressionStatement();
            },
        }
    }

    fn parseLetStatement(p: *Parser) ?ast.Statement {
        const cur_token = p.cur_token;

        if (!p.expectPeek(token.Token.IDENT)) {
            return null;
        }

        const name = ast.Identifier{
            .token = p.cur_token,
            .value = p.cur_token.IDENT.ident,
        };

        if (!p.expectPeek(token.Token.ASSIGN)) {
            return null;
        }

        p.next_token();

        const value = p.parseExpression(.LOWEST);

        if (p.peekTokenIs(token.Token.SEMICOLON)) {
            p.next_token();
        }

        return ast.Statement{ .Let = .{
            .token = cur_token,
            .name = name,
            .value = value,
        } };
    }

    fn parseReturnStatement(p: *Parser) ?ast.Statement {
        const cur_token = p.cur_token;

        p.next_token();

        const returnValue = p.parseExpression(.LOWEST);

        if (p.peekTokenIs(.SEMICOLON)) {
            p.next_token();
        }

        return ast.Statement{
            .Return = .{
                .token = cur_token,
                .returnValue = returnValue,
            },
        };
    }

    fn parseExpressionStatement(p: *Parser) ?ast.Statement {
        const cur_token = p.cur_token;

        const expr = p.parseExpression(.LOWEST);

        if (p.peekTokenIs(token.Token.SEMICOLON)) {
            p.next_token();
        }

        return ast.Statement{
            .Expression = .{
                .token = cur_token,
                .expression = expr,
            },
        };
    }

    fn expectPeek(p: *Parser, comptime other: anytype) bool {
        if (p.peekTokenIs(other)) {
            p.next_token();
            return true;
        } else {
            p.peekError(other);
            return false;
        }
    }

    fn peekTokenIs(p: *const Parser, comptime other: anytype) bool {
        const equ = std.mem.eql(u8, @tagName(p.peek_token), @tagName(other));

        return equ;
    }

    fn curTokenIs(p: *const Parser, comptime other: anytype) bool {
        const equ = std.mem.eql(u8, @tagName(p.cur_token), @tagName(other));

        return equ;
    }

    fn peekError(p: *Parser, comptime t: anytype) void {
        const msg = std.fmt.allocPrint(
            p.l.allocator,
            "expected next token to be {s}, got {s} instead",
            .{
                @tagName(t),
                @tagName(p.cur_token),
            },
        ) catch @panic("Failed to alloc error message");
        p.errors.append(msg) catch @panic("Failed to push error");
    }

    fn registerPrefix(
        p: *Parser,
        comptime tokenType: anytype,
        fun: prefixParseFn,
    ) !void {
        try p.prefixParseFns.put(@tagName(tokenType), fun);
    }

    fn registerInfix(
        p: *Parser,
        comptime tokenType: anytype,
        fun: infixParseFn,
    ) !void {
        try p.infixParseFns.put(@tagName(tokenType), fun);
    }

    fn parseExpression(p: *Parser, precedence: Precedence) ?ast.Expression {
        const prefix = p.prefixParseFns.get(@tagName(p.cur_token));
        if (prefix == null) {
            p.noPrefixParseFnError(p.cur_token);
            return null;
        }
        var left_exp = prefix.?(p);

        while (!p.peekTokenIs(token.Token.SEMICOLON) and //
            @intFromEnum(precedence) < @intFromEnum(p.peekPrecedence()))
        {
            const infix = p.infixParseFns.get(@tagName(p.peek_token));
            if (infix == null) {
                return left_exp;
            }

            p.next_token();

            left_exp = infix.?(p, left_exp);
        }

        return left_exp;
    }

    fn parseIdentifier(p: *Parser) ?ast.Expression {
        return ast.Expression{
            .Ident = .{
                .token = p.cur_token,
                .value = p.cur_token.token_literal() //
                catch @panic("Failed to get token_literal"),
            },
        };
    }

    fn parseInteger(p: *Parser) ?ast.Expression {
        return ast.Expression{
            .Integer = .{
                .token = p.cur_token,
                .value = std.fmt.parseInt(i64, p.cur_token.INT.int, 10) //
                catch @panic("Failed to parse int"),
            },
        };
    }

    fn noPrefixParseFnError(p: *Parser, t: token.Token) void {
        const msg = std.fmt.allocPrint(
            p.l.allocator,
            "no prefix parse function for {s} found",
            .{
                @tagName(t),
            },
        ) catch @panic("Failed to alloc error message");
        p.errors.append(msg) catch @panic("Failed to push error");
    }

    fn parsePrefixExpression(p: *Parser) ?ast.Expression {
        var expression = ast.Expression{
            .Prefix = .{
                .token = p.cur_token,
                .operator = p.cur_token.token_literal() //
                catch @panic("Failed to get token literal"),
                .right = undefined,
            },
        };
        expression.Prefix.right = p.l.allocator.create(ast.Expression) //
        catch unreachable;

        p.next_token();

        expression.Prefix.right.?.* = p.parseExpression(.PREFIX) orelse unreachable;

        return expression;
    }

    fn peekPrecedence(p: *const Parser) Precedence {
        return getPrecedence(p.peek_token) orelse .LOWEST;
    }

    fn curPrecedence(p: *const Parser) Precedence {
        return getPrecedence(p.cur_token) orelse .LOWEST;
    }

    fn parseInfixExpression(p: *Parser, left: ?ast.Expression) ?ast.Expression {
        var expression = ast.Expression{
            .Infix = .{
                .token = p.cur_token,
                .left = p.l.allocator.create(ast.Expression) catch unreachable,
                .operator = p.cur_token.token_literal() catch unreachable,
                .right = p.l.allocator.create(ast.Expression) catch unreachable,
            },
        };
        const precedence = p.curPrecedence();
        if (left) |lft| {
            expression.Infix.left.?.* = lft;
        } else {
            expression.Infix.left = null;
        }
        p.next_token();
        expression.Infix.right.?.* = p.parseExpression(precedence).?;
        return expression;
    }

    fn parseBoolean(p: *Parser) ?ast.Expression {
        return ast.Expression{
            .Boolean = .{
                .token = p.cur_token,
                .value = p.curTokenIs(token.Token.TRUE),
            },
        };
    }

    fn parseGroupedExpression(p: *Parser) ?ast.Expression {
        p.next_token();

        const exp = p.parseExpression(.LOWEST);

        if (!p.expectPeek(.RPAREN)) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(p: *Parser) ?ast.Expression {
        var expression = ast.Expression{ .If = .{
            .token = p.cur_token,
            .condition = null,
            .consequest = undefined,
            .alternative = null,
        } };

        if (!p.expectPeek(token.Token.LPAREN)) {
            return null;
        }

        p.next_token();
        const expr = p.parseExpression(.LOWEST);
        if (expr) |ex| {
            expression.If.condition = p.l.allocator.create(ast.Expression) catch unreachable;
            expression.If.condition.?.* = ex;
        }

        if (!p.expectPeek(token.Token.RPAREN)) {
            return null;
        }

        if (!p.expectPeek(token.Token.LBRACE)) {
            return null;
        }

        expression.If.consequest = p.parseBlockStatement();

        if (p.peekTokenIs(.ELSE)) {
            p.next_token();

            if (!p.expectPeek(.LBRACE)) {
                return null;
            }

            expression.If.alternative = p.parseBlockStatement();
        }

        return expression;
    }

    fn parseBlockStatement(p: *Parser) ast.Block {
        var block = ast.Block{
            .token = p.cur_token,
            .statements = std.ArrayList(ast.Statement).init(p.l.allocator),
        };

        p.next_token();

        while (!p.curTokenIs(.RBRACE) and !p.curTokenIs(.EOF)) {
            const stmt = p.parseStatement();
            if (stmt) |st| {
                block.statements.append(st) catch unreachable;
            }
            p.next_token();
        }

        return block;
    }

    fn parseFunctionExpression(p: *Parser) ?ast.Expression {
        var lit = ast.Expression{ .FunctionLiteral = .{
            .token = p.cur_token,
            .body = undefined,
            .parameters = undefined,
        } };

        if (!p.expectPeek(.LPAREN)) {
            return null;
        }

        lit.FunctionLiteral.parameters = p.parseFunctionParameters();

        if (!p.expectPeek(.LBRACE)) {
            return null;
        }

        lit.FunctionLiteral.body = p.parseBlockStatement();

        return lit;
    }

    fn parseFunctionParameters(p: *Parser) std.ArrayList(ast.Identifier) {
        var idents = std.ArrayList(ast.Identifier).init(p.l.allocator);

        if (p.peekTokenIs(.RPAREN)) {
            p.next_token();
            return idents;
        }

        p.next_token();

        const ident = ast.Identifier{
            .token = p.cur_token,
            .value = p.cur_token.token_literal() catch unreachable,
        };

        idents.append(ident) catch unreachable;

        while (p.peekTokenIs(.COMMA)) {
            p.next_token();
            p.next_token();
            const iIdent = ast.Identifier{
                .token = p.cur_token,
                .value = p.cur_token.token_literal() catch unreachable,
            };

            idents.append(iIdent) catch unreachable;
        }

        if (!p.expectPeek(.RPAREN)) {
            idents.clearAndFree();
            return idents;
        }

        return idents;
    }

    fn parseCallExpression(p: *Parser, function: ?ast.Expression) ?ast.Expression {
        var exp = ast.Expression{ .Call = .{
            .token = p.cur_token,
            .function = null,
            .arguments = undefined,
        } };

        if (function) |func| {
            exp.Call.function = p.l.allocator.create(ast.Expression) catch null;
            exp.Call.function.?.* = func;
        }

        exp.Call.arguments = p.parseCallArguments();

        return exp;
    }

    fn parseCallArguments(p: *Parser) std.ArrayList(ast.Expression) {
        var args = std.ArrayList(ast.Expression).init(p.l.allocator);

        if (p.peekTokenIs(.RPAREN)) {
            p.next_token();
            return args;
        }

        p.next_token();
        if (p.parseExpression(.LOWEST)) |exp| {
            args.append(exp) catch unreachable;
        }

        while (p.peekTokenIs(.COMMA)) {
            p.next_token();
            p.next_token();
            if (p.parseExpression(.LOWEST)) |exp| {
                args.append(exp) catch unreachable;
            }
        }

        if (!p.expectPeek(.RPAREN)) {
            args.clearAndFree();
        }

        return args;
    }
};

test "Let Statement" {
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 838383;
    ;
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const l = lexer.Lexer.init(input, alloc);
    var p = try Parser.init(l);

    const program = try p.parse_program();
    try checkParserErrors(&p);
    if (program.statements.items.len != 3) {
        try fatalPrint(
            "program.statements does not contain 3 statements. got={}\n",
            .{program.statements.items.len},
        );
    }

    const tests = [_]struct {
        expectedIdentifier: []const u8,
        expectedValue: ValType,
    }{
        .{ .expectedIdentifier = "x", .expectedValue = .{ .int = 5 } },
        .{ .expectedIdentifier = "y", .expectedValue = .{ .int = 10 } },
        .{ .expectedIdentifier = "foobar", .expectedValue = .{ .int = 838383 } },
    };

    for (tests, 0..) |tt, i| {
        const stmt = program.statements.items[i];
        try testLetStatement(stmt, tt.expectedIdentifier);

        try testLiteralExpression(stmt.Let.value.?, tt.expectedValue, alloc);
    }
}

fn fatalPrint(comptime fmt: []const u8, args: anytype) !void {
    std.debug.print(fmt, args);
    try std.testing.expect(false);
}

fn testLetStatement(s: ast.Statement, name: []const u8) !void {
    try std.testing.expectEqualStrings("LET", try s.token_literal());

    try std.testing.expectEqualStrings("Let", @tagName(s));

    try std.testing.expectEqualStrings(name, s.Let.name.value);

    try std.testing.expectEqualStrings(name, try s.Let.name.token_literal());
}

fn checkParserErrors(p: *const Parser) !void {
    if (p.errors.items.len == 0) {
        return;
    }

    std.debug.print("parser has {} errors\n", .{p.errors.items.len});
    for (p.errors.items) |err| {
        std.debug.print("parser error: {s}\n", .{err});
    }
    try std.testing.expect(false);
}

test "Return Statements" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 993322;
    ;
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const l = lexer.Lexer.init(input, alloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    if (program.statements.items.len != 3) {
        try fatalPrint(
            "program.Statements does not contain 3 statements, got={}\n",
            .{program.statements.items.len},
        );
    }

    for (program.statements.items) |stmt| {
        try std.testing.expectEqualStrings(
            @tagName(ast.Statement.Return),
            @tagName(stmt),
        );
        try std.testing.expectEqualStrings(
            "RETURN",
            try stmt.token_literal(),
        );
    }
}

test "Indentifier Expression" {
    const input = "foobar;";
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const l = lexer.Lexer.init(input, alloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    if (program.statements.items.len != 1) {
        try fatalPrint(
            "program has not enough statments. got={}",
            .{program.statements.items.len},
        );
    }
    try std.testing.expectEqualStrings(
        "Expression",
        @tagName(program.statements.items[0]),
    );
    const stmt = program.statements.items[0].Expression;
    try std.testing.expectEqualStrings(
        "Ident",
        @tagName(stmt.expression.?),
    );
    const ident = stmt.expression.?.Ident;
    try std.testing.expectEqualStrings(
        "foobar",
        ident.value,
    );
    try std.testing.expectEqualStrings(
        "foobar",
        try ident.token_literal(),
    );
}

test "Integer Expression" {
    const input = "5;";
    var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const l = lexer.Lexer.init(input, alloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    if (program.statements.items.len != 1) {
        try fatalPrint(
            "program has not enough statments. got={}",
            .{program.statements.items.len},
        );
    }
    try std.testing.expectEqualStrings(
        "Expression",
        @tagName(program.statements.items[0]),
    );
    const stmt = program.statements.items[0].Expression;
    try std.testing.expectEqualStrings(
        "Integer",
        @tagName(stmt.expression.?),
    );
    const ident = stmt.expression.?.Integer;
    try std.testing.expectEqual(
        5,
        ident.value,
    );
    try std.testing.expectEqualStrings(
        "5",
        try ident.token_literal(),
    );
}

test "Parsing Prefix Expression" {
    const profixTests = [_]struct {
        input: []const u8,
        operator: []const u8,
        value: ValType,
    }{
        .{
            .input = "!5",
            .operator = "!",
            .value = .{ .int = 5 },
        },
        .{
            .input = "-15",
            .operator = "-",
            .value = .{ .int = 15 },
        },
        .{
            .input = "!true;",
            .operator = "!",
            .value = .{ .boolean = true },
        },
        .{
            .input = "!false;",
            .operator = "!",
            .value = .{ .boolean = false },
        },
    };
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    for (profixTests) |tt| {
        var arenaAlloc = std.heap.ArenaAllocator.init(galloc);
        const alloc = gArenaAlloc.allocator();
        const l = lexer.Lexer.init(tt.input, alloc);
        var p = try Parser.init(l);
        const program = try p.parse_program();
        try checkParserErrors(&p);

        if (program.statements.items.len != 1) {
            try fatalPrint(
                "program has not enough statments. got={}",
                .{program.statements.items.len},
            );
        }
        try std.testing.expectEqualStrings(
            "Expression",
            @tagName(program.statements.items[0]),
        );
        const stmt = program.statements.items[0].Expression;
        try std.testing.expectEqualStrings(
            "Prefix",
            @tagName(stmt.expression.?),
        );
        const exp = stmt.expression.?.Prefix;
        try std.testing.expectEqualStrings(
            tt.operator,
            exp.operator,
        );

        try testLiteralExpression(exp.right.?.*, tt.value, alloc);

        arenaAlloc.deinit();
    }
}

fn testIntegerLiteral(
    il: ast.Expression,
    value: i64,
    alloc: std.mem.Allocator,
) !void {
    const integ = il.Integer;

    const valstr = try std.fmt.allocPrint(alloc, "{}", .{value});
    defer alloc.free(valstr);

    try std.testing.expectEqual(value, integ.value);
    try std.testing.expectEqualStrings(valstr, try integ.token_literal());
}

const ValType = union(enum) {
    int: i64,
    ident: []const u8,
    boolean: bool,
};

test "Parsing Infix Expressions" {
    const infixTests = [_]struct {
        input: []const u8,
        leftValue: ValType,
        operator: []const u8,
        rightValue: ValType,
    }{
        .{
            .input = "5 + 5;",
            .leftValue = .{ .int = 5 },
            .operator = "+",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 - 5;",
            .leftValue = .{ .int = 5 },
            .operator = "-",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 * 5;",
            .leftValue = .{ .int = 5 },
            .operator = "*",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 / 5;",
            .leftValue = .{ .int = 5 },
            .operator = "/",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 > 5;",
            .leftValue = .{ .int = 5 },
            .operator = ">",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 < 5;",
            .leftValue = .{ .int = 5 },
            .operator = "<",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 == 5;",
            .leftValue = .{ .int = 5 },
            .operator = "==",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "5 != 5;",
            .leftValue = .{ .int = 5 },
            .operator = "!=",
            .rightValue = .{ .int = 5 },
        },
        .{
            .input = "true == true",
            .leftValue = .{ .boolean = true },
            .operator = "==",
            .rightValue = .{ .boolean = true },
        },
        .{
            .input = "true != false",
            .leftValue = .{ .boolean = true },
            .operator = "!=",
            .rightValue = .{ .boolean = false },
        },
        .{
            .input = "false == false",
            .leftValue = .{ .boolean = false },
            .operator = "==",
            .rightValue = .{ .boolean = false },
        },
    };
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    for (infixTests) |tt| {
        var arenaAlloc = std.heap.ArenaAllocator.init(galloc);
        const alloc = gArenaAlloc.allocator();
        const l = lexer.Lexer.init(tt.input, alloc);
        var p = try Parser.init(l);
        const program = try p.parse_program();
        try checkParserErrors(&p);

        if (program.statements.items.len != 1) {
            try fatalPrint(
                "program has not enough statments. got={}",
                .{program.statements.items.len},
            );
        }
        try std.testing.expectEqualStrings(
            "Expression",
            @tagName(program.statements.items[0]),
        );
        const stmt = program.statements.items[0].Expression;
        try testInfixExpression(
            stmt.expression.?,
            tt.leftValue,
            tt.operator,
            tt.rightValue,
            alloc,
        );

        arenaAlloc.deinit();
    }
}

test "Operator Precedence Parsing" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4; -5 * 5",
            .expected = "(3 + 4)((-5) * 5)",
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) + 2",
            .expected = "((5 + 5) + 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
        .{
            .input = "a + add(b * c) + d",
            .expected = "((a + add((b * c))) + d)",
        },
        .{
            .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        .{
            .input = "add(a + b + c * d / f + g)",
            .expected = "add((((a + b) + ((c * d) / f)) + g))",
        },
    };
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    for (tests) |tt| {
        var arenaAlloc = std.heap.ArenaAllocator.init(galloc);
        const alloc = gArenaAlloc.allocator();
        const l = lexer.Lexer.init(tt.input, alloc);
        var p = try Parser.init(l);
        const program = try p.parse_program();
        try checkParserErrors(&p);

        try std.testing.expectEqualStrings(tt.expected, try program.String(alloc));

        arenaAlloc.deinit();
    }
}

fn testIdentifier(exp: ast.Expression, value: []const u8) !void {
    const ident = exp.Ident;

    try std.testing.expectEqualStrings(value, ident.value);
    try std.testing.expectEqualStrings(value, try ident.token_literal());
}

fn testLiteralExpression(
    exp: ast.Expression,
    expected: ValType,
    alloc: std.mem.Allocator,
) !void {
    switch (expected) {
        .int => |i| {
            try testIntegerLiteral(exp, i, alloc);
        },
        .ident => |ident| {
            try testIdentifier(exp, ident);
        },
        .boolean => |boolean| {
            try testBooleanLiteral(exp, boolean);
        },
    }
}

fn testInfixExpression(
    exp: ast.Expression,
    left: ValType,
    operator: []const u8,
    right: ValType,
    alloc: std.mem.Allocator,
) !void {
    const opExp = exp.Infix;

    try testLiteralExpression(opExp.left.?.*, left, alloc);

    try std.testing.expectEqualStrings(operator, opExp.operator);

    try testLiteralExpression(opExp.right.?.*, right, alloc);
}

fn testBooleanLiteral(exp: ast.Expression, value: bool) !void {
    const bo = exp.Boolean;

    try std.testing.expectEqual(value, bo.value);
    var expectedValue: []const u8 = "false";
    if (value) {
        expectedValue = "true";
    }
    try std.testing.expectEqualStrings(expectedValue, try bo.token_literal());
}

test "If Expression" {
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    const input = "if (x < y) { x }";

    const l = lexer.Lexer.init(input, galloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    try std.testing.expectEqual(1, program.statements.items.len);

    const stmt = program.statements.items[0].Expression;

    const exp = stmt.expression.?.If;

    try testInfixExpression(
        exp.condition.?.*,
        .{ .ident = "x" },
        "<",
        .{ .ident = "y" },
        galloc,
    );

    try std.testing.expectEqual(1, exp.consequest.statements.items.len);

    const consequence = exp.consequest.statements.items[0].Expression;

    try testIdentifier(consequence.expression.?, "x");

    try std.testing.expectEqual(null, exp.alternative);
}

test "If Expression Else" {
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    const input = "if (x < y) { x } else { y }";

    const l = lexer.Lexer.init(input, galloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    try std.testing.expectEqual(1, program.statements.items.len);

    const stmt = program.statements.items[0].Expression;

    const exp = stmt.expression.?.If;

    try testInfixExpression(
        exp.condition.?.*,
        .{ .ident = "x" },
        "<",
        .{ .ident = "y" },
        galloc,
    );

    try std.testing.expectEqual(1, exp.consequest.statements.items.len);

    const consequence = exp.consequest.statements.items[0].Expression;

    try testIdentifier(consequence.expression.?, "x");

    try std.testing.expectEqual(1, exp.alternative.?.statements.items.len);

    const alternative = exp.alternative.?.statements.items[0].Expression;

    try testIdentifier(alternative.expression.?, "y");
}

test "Function Literal Parsing" {
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    const input = "fn(x, y) { x + y; }";

    const l = lexer.Lexer.init(input, galloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    try std.testing.expectEqual(1, program.statements.items.len);

    const stmt = program.statements.items[0].Expression;
    const function = stmt.expression.?.FunctionLiteral;
    try std.testing.expectEqual(2, function.parameters.items.len);

    try testLiteralExpression(
        .{ .Ident = function.parameters.items[0] },
        .{ .ident = "x" },
        galloc,
    );
    try testLiteralExpression(
        .{ .Ident = function.parameters.items[1] },
        .{ .ident = "y" },
        galloc,
    );

    try std.testing.expectEqual(1, function.body.statements.items.len);
    const bodyStmt = function.body.statements.items[0].Expression;
    try testInfixExpression(
        bodyStmt.expression.?,
        .{ .ident = "x" },
        "+",
        .{ .ident = "y" },
        galloc,
    );
}

test "Function Parameter Parsing" {
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedParams: []const []const u8,
    }{
        .{
            .input = "fn() {};",
            .expectedParams = &[0][]const u8{},
        },
        .{
            .input = "fn(x) {};",
            .expectedParams = &[_][]const u8{"x"},
        },
        .{
            .input = "fn(x, y, z) {};",
            .expectedParams = &[_][]const u8{ "x", "y", "z" },
        },
    };

    for (tests) |tt| {
        var arenaAlloc = std.heap.ArenaAllocator.init(galloc);
        const alloc = arenaAlloc.allocator();
        const l = lexer.Lexer.init(tt.input, alloc);
        var p = try Parser.init(l);
        const program = try p.parse_program();
        try checkParserErrors(&p);

        const stmt = program.statements.items[0].Expression;
        const function = stmt.expression.?.FunctionLiteral;

        try std.testing.expectEqual(
            tt.expectedParams.len,
            function.parameters.items.len,
        );

        for (tt.expectedParams, 0..) |ident, i| {
            try testLiteralExpression(
                .{ .Ident = function.parameters.items[i] },
                .{ .ident = ident },
                alloc,
            );
        }

        arenaAlloc.deinit();
    }
}

test "Call Expression Parsing" {
    var gArenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer gArenaAlloc.deinit();
    const galloc = gArenaAlloc.allocator();

    const input = "add(1, 2 * 3, 4 + 5);";
    const l = lexer.Lexer.init(input, galloc);
    var p = try Parser.init(l);
    const program = try p.parse_program();
    try checkParserErrors(&p);

    try std.testing.expectEqual(1, program.statements.items.len);
    const stmt = program.statements.items[0].Expression;
    const exp = stmt.expression.?.Call;

    try testIdentifier(exp.function.?.*, "add");
    try std.testing.expectEqual(3, exp.arguments.items.len);

    try testLiteralExpression(
        exp.arguments.items[0],
        .{ .int = 1 },
        galloc,
    );
    try testInfixExpression(
        exp.arguments.items[1],
        .{ .int = 2 },
        "*",
        .{ .int = 3 },
        galloc,
    );
    try testInfixExpression(
        exp.arguments.items[2],
        .{ .int = 4 },
        "+",
        .{ .int = 5 },
        galloc,
    );
}
