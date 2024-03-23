const objects = @import("objects.zig");
const Environment = @import("env.zig").Environment;
const ast = @import("ast.zig");
const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const strEql = @import("utils.zig").strEql;
const print = std.debug.print;

pub const Evaluator = struct {
    pub fn eval(allocator: std.mem.Allocator, program: ast.Program, env: Environment) ?objects.Object {
        var result: ?objects.Object = null;
        for (program.statements) |statement| {
            result = evalStatement(allocator, statement, env);
            if (result) |r| {
                switch (r) {
                    .returnValue => |returnValue| return returnValue.value.*,
                    .err => |err| return err.asObject(),
                    else => continue,
                }
            } else {
                return null;
            }
        }
        return result;
    }

    fn evalStatement(allocator: std.mem.Allocator, statement: ast.Statement, env: Environment) ?objects.Object {
        // print("statement: {}\n", .{statement});
        switch (statement) {
            .expressionStatement => |exp_statement| return evalExpression(allocator, exp_statement.expression, env),
            // .letStatement => |let_statement| return evalLetStatement(let_statement, env),
            // .blockStatement => |block_statement| return evalBlockStatement(block_statement, env),
            // .returnStatement => |return_statement| return evalReturnStatement(return_statement, env),
            else => return null,
        }
    }

    fn evalExpression(allocator: std.mem.Allocator, expression: ?ast.Expression, env: Environment) ?objects.Object {
        if (expression) |exp| {

            // print("exp type: {}\n", .{@typeInfo(@TypeOf(exp)).Union});
            switch (exp) {
                .integerLiteral => |literal| return objects.Integer.init(allocator, literal.value).asObject(),
                .prefixExpression => |prefix| return evalPrefixExpression(allocator, prefix, env),
                .infixExpression => |infix| return evalInfixExpression(allocator, infix, env),
                .booleanLiteral => |literal| return objects.booleanAsObject(allocator, literal.value),
                else => return blk: {
                    print("unmanaged expression:{}, type={}\n", .{ exp, std.meta.activeTag(exp) });
                    break :blk null;
                },
            }
        } else {
            return null;
        }
    }

    fn evalMinusPrefixOperatorExpression(allocator: std.mem.Allocator, object: objects.Object) objects.Object {
        switch (object) {
            .integer => |integer| return objects.Integer.init(allocator, -integer.value).asObject(),
            else => return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "Unknown Operator -{}", .{std.meta.activeTag(object)}) catch unreachable).asObject(),
        }
    }

    fn evalBangOperatorExpression(object: objects.Object) objects.Object {
        return object;
    }

    fn evalPrefixExpression(allocator: std.mem.Allocator, prefix: ast.PrefixExpression, env: Environment) ?objects.Object {
        const right = evalExpression(allocator, prefix.right.?.*, env);
        const body = struct {
            operator: []const u8,
            fn invoke(self: @This(), alloc: std.mem.Allocator, r: objects.Object) ?objects.Object {
                switch (self.operator[0]) {
                    '!' => return evalBangOperatorExpression(r),
                    '-' => return evalMinusPrefixOperatorExpression(alloc, r),
                    else => return objects.Error.init(alloc, std.fmt.allocPrint(alloc, "Unknown operator {s}{}", .{ self.operator, std.meta.activeTag(r) }) catch unreachable).asObject(),
                }
            }
        };
        return ifError(allocator, right, body{ .operator = prefix.operator });
    }

    fn evalInfix(allocator: std.mem.Allocator, operator: []const u8, left: objects.Object, right: objects.Object) objects.Object {
        if (@as(objects.Object, left) == objects.Object.integer and @as(objects.Object, right) == objects.Object.integer) {
            switch (operator[0]) {
                '+' => return objects.Integer.init(allocator, left.integer.value + right.integer.value).asObject(),
                '-' => return objects.Integer.init(allocator, left.integer.value - right.integer.value).asObject(),
                '*' => return objects.Integer.init(allocator, left.integer.value * right.integer.value).asObject(),
                '/' => return objects.Integer.init(allocator, @divExact(left.integer.value, right.integer.value)).asObject(),
                '<' => return objects.booleanAsObject(allocator, left.integer.value < right.integer.value),
                '>' => return objects.booleanAsObject(allocator, left.integer.value > right.integer.value),
                // first character of "=="
                '=' => return objects.booleanAsObject(allocator, left.integer.value == right.integer.value),
                // first character of "!="
                '!' => return objects.booleanAsObject(allocator, left.integer.value != right.integer.value),
                else => return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "unknown operator: {} {s} {}", .{ std.meta.activeTag(left), operator, std.meta.activeTag(right) }) catch unreachable).asObject(),
            }
        }
        if (strEql(operator, "==")) {
            return objects.booleanAsObject(allocator, std.meta.eql(left, right));
        }
        if (strEql(operator, "!=")) {
            return objects.booleanAsObject(allocator, !std.meta.eql(left, right));
        }
        return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "unknown operator: {} {s} {}", .{ std.meta.activeTag(left), operator, std.meta.activeTag(right) }) catch unreachable).asObject();
    }

    fn evalInfixExpression(allocator: std.mem.Allocator, infix: ast.InfixExpression, env: Environment) ?objects.Object {
        const left = evalExpression(allocator, infix.left.?.*, env);
        const body = struct {
            _infix: ast.InfixExpression,
            _env: Environment,
            fn invoke(self: @This(), alloc: std.mem.Allocator, l: objects.Object) ?objects.Object {
                const right = evalExpression(alloc, self._infix.right.?.*, self._env);
                const inner = struct {
                    _l: objects.Object,
                    __infix: ast.InfixExpression,
                    fn invoke(_self: @This(), _alloc: std.mem.Allocator, r: objects.Object) ?objects.Object {
                        return evalInfix(_alloc, _self.__infix.operator, _self._l, r);
                    }
                };
                return ifError(alloc, right, inner{ ._l = l, .__infix = self._infix });
            }
        };
        return ifError(allocator, left, body{ ._infix = infix, ._env = env });
    }

    fn ifError(allocator: std.mem.Allocator, object: ?objects.Object, body: anytype) ?objects.Object {
        if (object) |obj| {
            switch (obj) {
                .err => |e| return e.asObject(),
                else => |o| return body.invoke(allocator, o),
            }
        } else {
            return null;
        }
    }
};

// Tests
fn TestData(comptime T: type) type {
    return struct { input: []const u8, expected: T };
}

const TestDataInt = TestData(i64);
const TestDataBool = TestData(bool);

const expect = std.testing.expect;

test "eval integer expressions" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const tests = [_]TestDataInt{
        TestDataInt{ .input = "5", .expected = 5 },
        TestDataInt{ .input = "10", .expected = 10 },
        TestDataInt{ .input = "-5", .expected = -5 },
        TestDataInt{ .input = "-10", .expected = -10 },
        TestDataInt{ .input = "5 + 5 + 5 + 5 -10", .expected = 10 },
        TestDataInt{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        TestDataInt{ .input = "-50 + 100 + -50", .expected = 0 },
        TestDataInt{ .input = "5 * 2 + 10", .expected = 20 },
        TestDataInt{ .input = "5 + 2 * 10", .expected = 25 },
        TestDataInt{ .input = "20 + 2 * -10", .expected = 0 },
        TestDataInt{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        TestDataInt{ .input = "2 * (5 + 10)", .expected = 30 },
        TestDataInt{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        TestDataInt{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        TestDataInt{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };
    try testInt(&tests, allocator);
}

test "eval boolean expression" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const tests = [_]TestDataBool{
        TestDataBool{ .input = "true", .expected = true },
        TestDataBool{ .input = "false", .expected = false },
        TestDataBool{ .input = "1 < 2", .expected = true },
        TestDataBool{ .input = "1 < 1", .expected = false },
        TestDataBool{ .input = "1 > 1", .expected = false },
        TestDataBool{ .input = "1 == 1", .expected = true },
        TestDataBool{ .input = "1 != 1", .expected = false },
        TestDataBool{ .input = "1 == 2", .expected = false },
        TestDataBool{ .input = "1 != 2", .expected = true },
        TestDataBool{ .input = "true == true", .expected = true },
        TestDataBool{ .input = "false == false", .expected = true },
        TestDataBool{ .input = "true == false", .expected = false },
        TestDataBool{ .input = "true != false", .expected = true },
        TestDataBool{ .input = "false != true", .expected = true },
        TestDataBool{ .input = "(1 < 2) == true", .expected = true },
        TestDataBool{ .input = "(1 < 2) == false", .expected = false },
        TestDataBool{ .input = "(1 > 2) == true", .expected = false },
        TestDataBool{ .input = "(1 > 2) == false", .expected = true },
    };
    try testBool(&tests, allocator);
}

fn testBool(tests: []const TestDataBool, allocator: std.mem.Allocator) !void {
    for (tests) |t| {
        const opt_object = testEval(allocator, t.input);
        if (opt_object) |object| {
            // std.debug.print("object:{}\n", .{object});
            try expect(object.boolean.value == t.expected);
        } else {
            try expect(false);
        }
    }
}

fn testInt(tests: []const TestDataInt, allocator: std.mem.Allocator) !void {
    for (tests) |t| {
        const opt_object = testEval(allocator, t.input);
        if (opt_object) |object| {
            // std.debug.print("object:{}\n", .{object});
            try expect(object.integer.value == t.expected);
        } else {
            try expect(false);
        }
    }
}

fn testEval(allocator: std.mem.Allocator, input: []const u8) ?objects.Object {
    var lexer = Lexer.init(allocator, input);
    var parser = Parser.init(allocator, lexer);
    var program = parser.parseProgram();
    checkParserErrors(parser) catch unreachable;
    return Evaluator.eval(allocator, program, Environment.init(allocator));
}

fn checkParserErrors(parser: Parser) !void {
    var errors = parser.errors.items;
    if (errors.len != 0) {
        print("parser has {} errors\n", .{errors.len});
        for (errors) |e| {
            print("{s}\n", .{e});
        }
        try expect(false);
    }
}
