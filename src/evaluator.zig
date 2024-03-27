const objects = @import("objects.zig");
const Environment = @import("env.zig").Environment;
const ast = @import("ast.zig");
const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
const strEql = @import("utils.zig").strEql;
const print = std.debug.print;

pub const Evaluator = struct {
    pub fn eval(allocator: std.mem.Allocator, program: ast.Program, env: *Environment) ?objects.Object {
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

    fn evalStatement(allocator: std.mem.Allocator, statement: ast.Statement, env: *Environment) ?objects.Object {
        // print("statement: {}\n", .{statement});
        switch (statement) {
            .expressionStatement => |exp_statement| return evalExpression(allocator, exp_statement.expression, env),
            .letStatement => |let_statement| return evalLetStatement(allocator, let_statement, env),
            .blockStatement => |block_statement| return evalBlockStatement(allocator, block_statement, env),
            .returnStatement => |return_statement| return evalReturnStatement(allocator, return_statement, env),
        }
    }

    fn evalExpression(allocator: std.mem.Allocator, expression: ?ast.Expression, env: *Environment) ?objects.Object {
        if (expression) |exp| {

            // print("exp type: {}\n", .{@typeInfo(@TypeOf(exp)).Union});
            switch (exp) {
                .integerLiteral => |literal| return objects.Integer.init(allocator, literal.value).asObject(),
                .prefixExpression => |prefix| return evalPrefixExpression(allocator, prefix, env),
                .infixExpression => |infix| return evalInfixExpression(allocator, infix, env),
                .booleanLiteral => |literal| return objects.booleanAsObject(allocator, literal.value),
                .ifExpression => |if_expression| return evalIfExpression(allocator, if_expression, env),
                .functionLiteral => |function| return objects.Function.init(allocator, function.parameters, function.body, env).asObject(),
                .callExpression => |call| return evalCallExpression(allocator, call, env),
                .identifier => |id| return evalIdentifier(allocator, id, env.*),
                else => return blk: {
                    std.debug.panic("unmanaged expression:{}, type={}\n", .{ exp, std.meta.activeTag(exp) });
                    break :blk null;
                },
            }
        } else {
            return null;
        }
    }

    fn evalIdentifier(allocator: std.mem.Allocator, identifier: ast.Identifier, env: Environment) objects.Object {
        const opt_value = env.get(identifier.value);
        if (opt_value) |value| {
            return value;
        } else {
            // TODO builtins
            return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "identifier not found: {s}", .{identifier.value}) catch unreachable).asObject();
        }
    }

    fn evalLetStatement(allocator: std.mem.Allocator, statement: ast.LetStatement, env: *Environment) ?objects.Object {
        const value = evalExpression(allocator, statement.value, env);
        const closure = struct {
            let_statement: ast.LetStatement,
            _env: *Environment,
            fn invoke(self: @This(), alloc: std.mem.Allocator, v: objects.Object) ?objects.Object {
                _ = alloc;
                return self._env.put(self.let_statement.name.value, v);
            }
        };
        const body = closure{ .let_statement = statement, ._env = env };
        return ifError(allocator, value, body);
    }

    fn evalCallExpression(allocator: std.mem.Allocator, expression: ast.CallExpression, env: *Environment) ?objects.Object {
        const function = evalExpression(allocator, expression.function.?.*, env);
        const closure = struct {
            call_expression: ast.CallExpression,
            _env: *Environment,
            fn invoke(self: @This(), alloc: std.mem.Allocator, f: objects.Object) ?objects.Object {
                const args = evalExpressions(alloc, self.call_expression.arguments, self._env);
                if (args.len == 1 and args[0].?.isError()) {
                    return args[0];
                }
                return applyFunction(alloc, f, args);
            }
        };
        return ifError(allocator, function, closure{ .call_expression = expression, ._env = env });
    }

    fn applyFunction(allocator: std.mem.Allocator, function: objects.Object, args: []const ?objects.Object) ?objects.Object {
        switch (function) {
            .function => |fun| return blk: {
                var extend_env = extendFunctionEnv(allocator, fun, args);
                if (fun.body) |body| {
                    const opt_evaluated = evalBlockStatement(allocator, body, &extend_env);
                    if (opt_evaluated) |evaluated| {
                        switch (evaluated) {
                            .returnValue => |return_value| break :blk return_value.value.*,
                            else => break :blk evaluated,
                        }
                    } else {
                        break :blk null;
                    }
                } else {
                    break :blk null;
                }
            },
            else => return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "not a function: {}", .{std.meta.activeTag(function)}) catch unreachable).asObject(),
        }
    }

    fn extendFunctionEnv(allocator: std.mem.Allocator, fun: objects.Function, args: []const ?objects.Object) Environment {
        var env = Environment.initWithOuter(allocator, fun.env);
        if (fun.parameters) |parameters| {
            for (parameters, 0..) |parameter, i| {
                env.set(parameter.value, args[i].?);
            }
        }

        return env;
    }

    fn evalExpressions(allocator: std.mem.Allocator, arguments: ?[]?*const ast.Expression, env: *Environment) []?objects.Object {
        var args = std.ArrayList(?objects.Object).init(allocator);
        for (arguments.?) |argument| {
            const evaluated = evalExpression(allocator, argument.?.*, env);

            if (evaluated.?.isError()) {
                var error_wrapper = [_]?objects.Object{evaluated};
                // return error_wrapper[0..error_wrapper.len];
                return &error_wrapper;
            }
            args.append(evaluated) catch unreachable;
        }
        return args.items;
    }

    fn isTruthy(allocator: std.mem.Allocator, object: objects.Object) bool {
        if (std.meta.eql(object, objects.Nil(allocator))) {
            return false;
        }
        if (std.meta.eql(object, objects.True())) {
            return true;
        }
        if (std.meta.eql(object, objects.False())) {
            return false;
        }
        return true;
    }

    fn evalIfExpression(allocator: std.mem.Allocator, expression: ast.IfExpression, env: *Environment) ?objects.Object {
        const condition = evalExpression(allocator, expression.condition.?.*, env);
        const closure = struct {
            if_expression: ast.IfExpression,
            _env: *Environment,
            fn invoke(self: @This(), alloc: std.mem.Allocator, c: objects.Object) ?objects.Object {
                if (isTruthy(alloc, c)) {
                    return evalBlockStatement(alloc, self.if_expression.consequence.?, self._env);
                } else {
                    if (self.if_expression.alternative) |alternative| {
                        return evalBlockStatement(alloc, alternative, self._env);
                    } else {
                        return objects.Nil(alloc);
                    }
                }
            }
        };
        return ifError(allocator, condition, closure{ .if_expression = expression, ._env = env });
    }

    fn evalReturnStatement(allocator: std.mem.Allocator, return_statement: ast.ReturnStatement, env: *Environment) ?objects.Object {
        const value = evalExpression(allocator, return_statement.returnValue, env);
        const closure = struct {
            fn invoke(self: @This(), alloc: std.mem.Allocator, r: objects.Object) ?objects.Object {
                _ = self;
                return objects.ReturnValue.init(alloc, &r).asObject();
            }
        };
        return ifError(allocator, value, closure{});
    }

    fn evalBlockStatement(allocator: std.mem.Allocator, block: ast.BlockStatement, env: *Environment) ?objects.Object {
        var result: ?objects.Object = null;
        if (block.statements) |statements| {
            for (statements) |statement| {
                result = evalStatement(allocator, statement.?.*, env);
                if (result) |r| {
                    switch (r) {
                        .returnValue => |return_value| return return_value.asObject(),
                        .err => |err| return err.asObject(),
                        else => {},
                    }
                }
            }
        }
        return result;
    }

    fn evalMinusPrefixOperatorExpression(allocator: std.mem.Allocator, object: objects.Object) objects.Object {
        switch (object) {
            .integer => |integer| return objects.Integer.init(allocator, -integer.value).asObject(),
            else => return objects.Error.init(allocator, std.fmt.allocPrint(allocator, "Unknown Operator -{}", .{std.meta.activeTag(object)}) catch unreachable).asObject(),
        }
    }

    fn evalBangOperatorExpression(allocator: std.mem.Allocator, object: objects.Object) objects.Object {
        if (std.meta.eql(object, objects.True())) {
            return objects.False();
        }
        if (std.meta.eql(object, objects.False())) {
            return objects.True();
        }
        if (std.meta.eql(object, objects.Nil(allocator))) {
            return objects.True();
        }
        return objects.False();
    }

    fn evalPrefixExpression(allocator: std.mem.Allocator, prefix: ast.PrefixExpression, env: *Environment) ?objects.Object {
        const right = evalExpression(allocator, prefix.right.?.*, env);
        const closure = struct {
            operator: []const u8,
            fn invoke(self: @This(), alloc: std.mem.Allocator, r: objects.Object) ?objects.Object {
                switch (self.operator[0]) {
                    '!' => return evalBangOperatorExpression(alloc, r),
                    '-' => return evalMinusPrefixOperatorExpression(alloc, r),
                    else => return objects.Error.init(alloc, std.fmt.allocPrint(alloc, "Unknown operator {s}{}", .{ self.operator, std.meta.activeTag(r) }) catch unreachable).asObject(),
                }
            }
        };
        return ifError(allocator, right, closure{ .operator = prefix.operator });
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

    fn evalInfixExpression(allocator: std.mem.Allocator, infix: ast.InfixExpression, env: *Environment) ?objects.Object {
        const left = evalExpression(allocator, infix.left.?.*, env);
        const closure = struct {
            _infix: ast.InfixExpression,
            _env: *Environment,
            fn invoke(self: @This(), alloc: std.mem.Allocator, l: objects.Object) ?objects.Object {
                const right = evalExpression(alloc, self._infix.right.?.*, self._env);
                const inner_closure = struct {
                    _l: objects.Object,
                    __infix: ast.InfixExpression,
                    fn invoke(_self: @This(), _alloc: std.mem.Allocator, r: objects.Object) ?objects.Object {
                        return evalInfix(_alloc, _self.__infix.operator, _self._l, r);
                    }
                };
                return ifError(alloc, right, inner_closure{ ._l = l, .__infix = self._infix });
            }
        };
        return ifError(allocator, left, closure{ ._infix = infix, ._env = env });
    }

    fn ifError(allocator: std.mem.Allocator, object: ?objects.Object, closure: anytype) ?objects.Object {
        if (object) |obj| {
            switch (obj) {
                .err => |e| return e.asObject(),
                else => |o| return closure.invoke(allocator, o),
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

test "bang operator" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const tests = [_]TestDataBool{
        TestDataBool{ .input = "!true", .expected = false },
        TestDataBool{ .input = "!false", .expected = true },
        TestDataBool{ .input = "!5", .expected = false },
        TestDataBool{ .input = "!!true", .expected = true },
        TestDataBool{ .input = "!!false", .expected = false },
        TestDataBool{ .input = "!!5", .expected = true },
    };
    try testBool(&tests, allocator);
}

test "if else expression" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const Test = TestData(?i64);
    const tests = [_]Test{
        Test{ .input = "if (true) { 10 }", .expected = 10 },
        Test{ .input = "if (false) { 10 }", .expected = null },
        Test{ .input = "if (1) { 10 }", .expected = 10 },
        Test{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        Test{ .input = "if (1 > 2) { 10 }", .expected = null },
        Test{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = 20 },
        Test{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = 10 },
    };
    for (tests) |t| {
        const opt_object = testEval(allocator, t.input);
        if (opt_object) |object| {
            if (t.expected) |expected| {
                try expect(object.integer.value == expected);
            } else {
                try expect(std.meta.eql(object, objects.Nil(allocator)));
            }
        } else {
            try expect(false);
        }
    }
}

test "return statements" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const tests = [_]TestDataInt{
        TestDataInt{ .input = "return 10;", .expected = 10 },
        TestDataInt{ .input = "return 10; 9;", .expected = 10 },
        TestDataInt{ .input = "return 2 * 5; 9;", .expected = 10 },
        TestDataInt{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        TestDataInt{ .input = 
        \\ if(10 > 1){
        \\  if(10 > 1){
        \\      return 10;
        \\  }
        \\  return 1;
        \\ }
        , .expected = 10 },
        TestDataInt{ .input = 
        \\  let f = fn(x){
        \\    return x;
        \\    x + 10;
        \\  };
        \\  f(10);
        , .expected = 10 },
        TestDataInt{ .input = 
        \\  let f = fn(x) {
        \\      let result = x + 10;
        \\      return result;
        \\      return 10;
        \\  };
        \\  f(10);
        , .expected = 20 },
    };
    try testInt(&tests, allocator);
}

fn testBool(tests: []const TestDataBool, allocator: std.mem.Allocator) !void {
    for (tests) |t| {
        const opt_object = testEval(allocator, t.input);
        if (opt_object) |object| {
            // print("object:{}\n", .{object});
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
            // print("object:{}\n", .{object});
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
    var env = Environment.init(allocator);
    return Evaluator.eval(allocator, program, &env);
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
