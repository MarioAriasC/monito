const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Environment = @import("env.zig").Environment;
const Evaluator = @import("evaluator.zig").Evaluator;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
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

    var lexer = Lexer.init(allocator, fib);
    var parser = Parser.init(allocator, lexer);
    const program = parser.parseProgram();
    var env = Environment.init(allocator);
    const result = Evaluator.eval(allocator, program, &env);
    std.debug.print("{d}", .{result.?.integer.value});
}
