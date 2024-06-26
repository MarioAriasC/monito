const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Environment = @import("env.zig").Environment;
const Evaluator = @import("evaluator.zig").Evaluator;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer std.debug.print("detected leaks:{}", .{gpa.detectLeaks()});
    const gpa_allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const fib =
        \\  let fibonacci = fn(x) {
        \\      if(x < 2) {
        \\          return x;
        \\      }
        \\      else {
        \\          fibonacci(x - 1) + fibonacci(x - 2);
        \\      }
        \\  };
        \\  fibonacci(35);
    ;

    const lexer = Lexer.init(allocator, fib);
    var parser = Parser.init(allocator, lexer);
    const program = parser.parseProgram();
    var env = Environment.init(allocator);
    const result = Evaluator.eval(allocator, program, &env);
    std.debug.print("{d}\n", .{result.?.integer.value});
}
