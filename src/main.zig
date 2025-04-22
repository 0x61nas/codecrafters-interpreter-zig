const std = @import("std");
const process = std.process;
const io = std.io;

const Token = struct {
    typ: TokenTyp,
    lexme: []const u8,
    lit: ?[]const u8,

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
        STRING,
        NUMBER,
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

    pub fn lex(self: *Lexer, comptime err_handler: type) !void {
        while (self.eat()) |ch| {
            // std.debug.print("eat `{c}`\n", .{ch});
            if (ch == '\n') {
                self.line += 1;
                continue;
            }
            const token: Token = switch (ch) {
                '(' => .{ .typ = .LEFT_PAREN, .lexme = "(", .lit = null },
                ')' => .{ .typ = .RIGHT_PAREN, .lexme = ")", .lit = null },
                '{' => .{ .typ = .LEFT_BRACE, .lexme = "{", .lit = null },
                '}' => .{ .typ = .RIGHT_BRACE, .lexme = "}", .lit = null },
                ',' => .{ .typ = .COMMA, .lexme = ",", .lit = null },
                '.' => .{ .typ = .DOT, .lexme = ".", .lit = null },
                '-' => .{ .typ = .MINUS, .lexme = "-", .lit = null },
                '+' => .{ .typ = .PLUS, .lexme = "+", .lit = null },
                ';' => .{ .typ = .SEMICOLON, .lexme = ";", .lit = null },
                '/' => blk: {
                    if (self.prev_token) |ptok| {
                        if (ptok.typ == .SLASH) {
                            self.eat_comment();
                            _ = self.tokens.pop();
                            self.prev_token = null;
                            continue;
                        }
                    }
                    break :blk .{ .typ = .SLASH, .lexme = "/", .lit = null };
                },
                '=' => blk: {
                    var tok: Token = .{ .typ = .EQUAL, .lexme = "=", .lit = null };
                    if (self.prev_token) |last_tok| {
                        if (last_tok.typ == .EQUAL) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .EQUAL_EQUAL, .lexme = "==", .lit = null };
                        } else if (last_tok.typ == .BANG) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .BANG_EQUAL, .lexme = "!=", .lit = null };
                        } else if (last_tok.typ == .LESS) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .LESS_EQUAL, .lexme = "<=", .lit = null };
                        } else if (last_tok.typ == .GREATER) {
                            _ = self.tokens.pop();
                            tok = .{ .typ = .GREATER_EQUAL, .lexme = ">=", .lit = null };
                        }
                    }
                    break :blk tok;
                },
                '*' => .{ .typ = .STAR, .lexme = "*", .lit = null },
                '!' => .{ .typ = .BANG, .lexme = "!", .lit = null },
                '<' => .{ .typ = .LESS, .lexme = "<", .lit = null },
                '>' => .{ .typ = .GREATER, .lexme = ">", .lit = null },
                ' ', '\t' => continue,
                '\"' => {
                    try self.eat_string(err_handler);
                    continue;
                },
                '0'...'9' => {
                    try self.eat_number(err_handler);
                    continue;
                },
                else => {
                    self.has_err = true;
                    self.prev_token = null;
                    try err_handler.send(try std.fmt.allocPrint(std.heap.page_allocator, "[line {d}] Error: Unexpected character: {c}\n", .{ self.line, ch }));
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
                self.line += 1;
                break;
            }
        }
    }

    fn eat_string(self: *Lexer, comptime err_handler: type) !void {
        const lit_start = self.cursor;
        var closed = false;
        // const arena = std.heap.ArenaAllocator.init(alloc);
        while (self.eat()) |eaten| {
            if (eaten == '\"') {
                closed = true;
                break;
            }
        }
        if (!closed) {
            // self.errors.append()
            self.has_err = true;
            try err_handler.send(try std.fmt.allocPrint(std.heap.page_allocator, "[line {d}] Error: Unterminated string.\n", .{self.line}));
            return;
        }
        try self.tokens.append(.{ .typ = .STRING, .lexme = self.source[lit_start - 1 .. self.cursor], .lit = self.source[lit_start .. self.cursor - 1] });
    }

    fn eat_number(self: *Lexer, comptime err_handler: type) !void {
        const lit_start = self.cursor - 1;
        var is_disimal = false;
        var lit_end = self.cursor;
        while (self.eat()) |eaten| {
            if (eaten == '.') {
                if (!is_disimal) {
                    lit_end = self.cursor + 1;
                    is_disimal = true;
                    continue;
                } else {
                    self.has_err = true;
                    try err_handler.send(try std.fmt.allocPrint(std.heap.page_allocator, "[line {d}] Error: Unexpected number.\n", .{self.line}));
                    return;
                }
            }
            if (!(eaten <= '9' and eaten >= '0')) {
                self.cursor -= 1;
                break;
            } else if (!is_disimal or eaten != '0') {
                lit_end = self.cursor;
            }
        }
        const lexme = self.source[lit_start..self.cursor];
        const lit = self.source[lit_start..lit_end];
        try self.tokens.append(.{ .typ = .NUMBER, .lexme = lexme, .lit = blk: {
            if (is_disimal) {
                break :blk lit;
            } else {
                break :blk try std.mem.concat(std.heap.page_allocator, u8, &[_][]const u8{ lit, ".0" });
            }
        } });
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

        const ErrorsHandeler = struct {
            fn send(msg: []const u8) !void {
                //
                try io.getStdErr().writer().print("{s}", .{msg});
            }
        };
        const errors_h = ErrorsHandeler;
        var lexer = Lexer.new(file_contents, alloc);

        try lexer.lex(errors_h);

        for (lexer.tokens.items) |tok| {
            try stdout.writer().print("{s} {s} {s}\n", .{ @tagName(tok.typ), tok.lexme, blk: {
                if (tok.lit) |lit| {
                    break :blk lit;
                } else {
                    break :blk "null";
                }
            } });
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
