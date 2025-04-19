const std = @import("std");
const process = std.process;
const io = std.io;

const Tokens = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
};

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    const stdout = io.getStdOut();
    // Uncomment this block to pass the first stage
    if (file_contents.len > 0) {
        const alloc = std.heap.page_allocator;
        var tokens = std.ArrayList(Tokens).init(alloc);
        defer tokens.deinit();

        for (file_contents) |ch| {
            // std.debug.print("eat `{c}`\n", .{ch});
            if (ch == '\n') {
                continue;
            }
            const token = switch (ch) {
                '(' => Tokens.LEFT_PAREN,
                ')' => Tokens.RIGHT_PAREN,
                '{' => Tokens.LEFT_BRACE,
                '}' => Tokens.RIGHT_BRACE,
                ',' => Tokens.COMMA,
                '.' => Tokens.DOT,
                '-' => Tokens.MINUS,
                '+' => Tokens.PLUS,
                ';' => Tokens.SEMICOLON,
                '/' => Tokens.SLASH,
                '*' => Tokens.STAR,
                else => @panic("unimplemented"),
            };
            try stdout.writer().print("{s} {c} null\n", .{ @tagName(token), ch });
            try tokens.append(token);
        }
        try stdout.writer().print("EOF  null\n", .{});
    } else {
        try io.getStdOut().writer().print("EOF  null\n", .{}); // Placeholder, remove this line when implementing the scanner
    }
}
