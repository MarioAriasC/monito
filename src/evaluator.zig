const objects = @import("objects.zig");
const Environment = @import("env.zig").Environment;
const ast = @import("ast.zig");
const std = @import("std");
const Parser = @import("parser.zig").Parser;
const Lexer = @import("lexer.zig").Lexer;
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
                    else => return r,
                }
            } else {
                return null;
            }
        }
        return result;
    }

    fn evalStatement(allocator: std.mem.Allocator, statement: ast.Statement, env: Environment) ?objects.Object {
        switch (statement) {
            .expressionStatement => |exp_statement| return evalExpression(allocator, exp_statement.expression, env),
            // .letStatement => |let_statement| return evalLetStatement(let_statement, env),
            // .blockStatement => |block_statement| return evalBlockStatement(block_statement, env),
            // .returnStatement => |return_statement| return evalReturnStatement(return_statement, env),
            else => return null,
        }
    }

    fn evalExpression(allocator: std.mem.Allocator, expression: ?ast.Expression, env: Environment) ?objects.Object {
        _ = env;
        if (expression) |exp| {
            switch (exp) {
                .integerLiteral => |literal| return objects.Integer.init(allocator, literal.value).asObject(),
                else => return null,
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

const expect = std.testing.expect;

test "eval integer expressions" {
    const test_allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(test_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const tests = [_]TestDataInt{
        TestDataInt{ .input = "5", .expected = 5 },
    };
    try testInt(&tests, allocator);
}

fn testInt(tests: []const TestDataInt, allocator: std.mem.Allocator) !void {
    for (tests) |t| {
        const opt_object = testEval(allocator, t.input);
        if (opt_object) |object| {
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
