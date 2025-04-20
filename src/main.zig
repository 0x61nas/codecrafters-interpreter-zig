const std = @import("std");
const process = std.process;
const io = std.io;

const Token = struct {
    typ: TokenTyp,
    lexme: []const u8,

    const TokenTyp = enum {
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
        EQUAL,
        EQUAL_EQUAL,
        BANG,
        BANG_EQUAL,
        LESS,
        LESS_EQUAL,
        GREATER,
        GREATER_EQUAL,
    };
};

const Lexer = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),
    prev_token: ?Token,
    has_err: bool,
    line: u64,
    cursor: u64,

    pub fn new(source: []const u8, alloc: std.mem.Allocator) Lexer {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(alloc),
            .prev_token = null,
            .has_err = false,
            .line = 1,
            .cursor = 0,
        };
    }

    pub fn lex(self: *Lexer) !void {
        while (self.eat()) |ch| {
            // std.debug.print("eat `{c}`\n", .{ch});
            if (ch == '\n') {
                self.line += 1;
                continue;
            }
            const token: Token = switch (ch) {
                '(' => .{ .typ = .LEFT_PAREN, .lexme = "(" },
                ')' => .{ .typ = .RIGHT_PAREN, .lexme = ")" },
                '{' => .{ .typ = .LEFT_BRACE, .lexme = "{" },
                '}' => .{ .typ = .RIGHT_BRACE, .lexme = "}" },
                ',' => .{ .typ = .COMMA, .lexme = "," },
                '.' => .{ .typ = .DOT, .lexme = "." },
                '-' => .{ .typ = .MINUS, .lexme = "-" },
                '+' => .{ .typ = .PLUS, .lexme = "+" },
                ';' => .{ .typ = .SEMICOLON, .lexme = ";" },
                '/' => blk: {
                    if (self.prev_token) |ptok| {
                        if (ptok.typ == .SLASH) {
                            self.eat_comment();
                            _ = self.tokens.pop();
                            self.prev_token = null;
                            continue;
                        }
                    }
                    break :blk .{ .typ = .SLASH, .lexme = "/" };
                },
                '=' => blk: {
                    var tok: Token = .{ .typ = .EQUAL, .lexme = "=" };
                    if (self.prev_token) |last_tok| {
                        if (last_tok.typ == .EQUAL) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .EQUAL_EQUAL, .lexme = "==" };
                        } else if (last_tok.typ == .BANG) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .BANG_EQUAL, .lexme = "!=" };
                        } else if (last_tok.typ == .LESS) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .LESS_EQUAL, .lexme = "<=" };
                        } else if (last_tok.typ == .GREATER) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .GREATER_EQUAL, .lexme = ">=" };
                        }
                    }
                    break :blk tok;
                },
                '*' => .{ .typ = .STAR, .lexme = "*" },
                '!' => .{ .typ = .BANG, .lexme = "!" },
                '<' => .{ .typ = .LESS, .lexme = "<" },
                '>' => .{ .typ = .GREATER, .lexme = ">" },
                ' ', '\t' => continue,
                else => {
                    self.has_err = true;
                    self.prev_token = null;
                    try io.getStdErr().writer().print("[line {d}] Error: Unexpected character: {c}\n", .{ self.line, ch });
                    continue;
                },
            };
            self.prev_token = token;
            try self.tokens.append(token);
        }
        return;
    }

    fn eat(self: *Lexer) ?u8 {
        if (self.cursor < self.source.len) {
            const eaten = self.source[self.cursor];
            self.cursor += 1;
            return eaten;
        }
        return null;
    }

    pub fn eat_comment(self: *Lexer) void {
        while (self.eat()) |eaten| {
            if (eaten == '\n') {
                break;
            }
        }
    }
};

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    var exit_code: u8 = 0;
    const stdout = io.getStdOut();

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

    // Uncomment this block to pass the first stage
    if (file_contents.len > 0) {
        const alloc = std.heap.page_allocator;
        var lexer = Lexer.new(file_contents, alloc);

        try lexer.lex();

        for (lexer.tokens.items) |tok| {
            try stdout.writer().print("{s} {s} null\n", .{ @tagName(tok.typ), tok.lexme });
        }
        try stdout.writer().print("EOF  null\n", .{});

        if (lexer.has_err) {
            exit_code = 65;
        }
    } else {
        try io.getStdOut().writer().print("EOF  null\n", .{}); // Placeholder, remove this line when implementing the scanner
    }
    process.exit(exit_code);
}
