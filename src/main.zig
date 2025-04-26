const std = @import("std");
const process = std.process;
const io = std.io;

const Token = struct {
    typ: TokenTyp,
    lexme: []const u8,
    lit: ?[]const u8,
    line: u64,

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
        IDENTIFIER,
        AND,
        OR,
        CLASS,
        IF,
        ELSE,
        WHILE,
        FOR,
        FALSE,
        TRUE,
        FUN,
        VAR,
        NIL,
        SUPER,
        THIS,
        RETURN,
        PRINT,
    };
};

const Lexer = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),
    prev_token: ?Token,
    keywords_map: std.StringHashMap(Token.TokenTyp),
    has_err: bool,
    line: u64,
    cursor: u64,

    pub fn new(source: []const u8, alloc: std.mem.Allocator) !Lexer {
        var map = std.StringHashMap(Token.TokenTyp).init(alloc);
        try map.ensureTotalCapacity(16);
        map.putAssumeCapacity("and", .AND);
        map.putAssumeCapacity("or", .OR);
        map.putAssumeCapacity("class", .CLASS);
        map.putAssumeCapacity("if", .IF);
        map.putAssumeCapacity("else", .ELSE);
        map.putAssumeCapacity("while", .WHILE);
        map.putAssumeCapacity("for", .FOR);
        map.putAssumeCapacity("false", .FALSE);
        map.putAssumeCapacity("true", .TRUE);
        map.putAssumeCapacity("fun", .FUN);
        map.putAssumeCapacity("var", .VAR);
        map.putAssumeCapacity("nil", .NIL);
        map.putAssumeCapacity("super", .SUPER);
        map.putAssumeCapacity("this", .THIS);
        map.putAssumeCapacity("return", .RETURN);
        map.putAssumeCapacity("print", .PRINT);

        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(alloc),
            .prev_token = null,
            .keywords_map = map,
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
            try switch (ch) {
                '(' => self.append_tok(.LEFT_PAREN, "(", null),
                ')' => self.append_tok(.RIGHT_PAREN, ")", null),
                '{' => self.append_tok(.LEFT_BRACE, "{", null),
                '}' => self.append_tok(.RIGHT_BRACE, "}", null),
                ',' => self.append_tok(.COMMA, ",", null),
                '.' => self.append_tok(.DOT, ".", null),
                '-' => self.append_tok(.MINUS, "-", null),
                '+' => self.append_tok(.PLUS, "+", null),
                ';' => self.append_tok(.SEMICOLON, ";", null),
                '/' => blk: {
                    if (self.match('/')) {
                        self.eat_comment();
                        continue;
                    }
                    break :blk self.append_tok(.SLASH, "/", null);
                },
                '=' => blk: {
                    var typ: Token.TokenTyp = .EQUAL;
                    var lexme: []const u8 = "=";
                    if (self.prev_token) |last_tok| {
                        if (last_tok.typ == .EQUAL) {
                            _ = self.tokens.pop();
                            typ = .EQUAL_EQUAL;
                            lexme = "==";
                        } else if (last_tok.typ == .BANG) {
                            _ = self.tokens.pop();
                            typ = .BANG_EQUAL;
                            lexme = "!=";
                        } else if (last_tok.typ == .LESS) {
                            _ = self.tokens.pop();
                            typ = .LESS_EQUAL;
                            lexme = "<=";
                        } else if (last_tok.typ == .GREATER) {
                            _ = self.tokens.pop();
                            typ = .GREATER_EQUAL;
                            lexme = ">=";
                        }
                    }
                    break :blk self.append_tok(typ, lexme, null);
                },
                '*' => self.append_tok(.STAR, "*", null),
                '!' => self.append_tok(.BANG, "!", null),
                '<' => self.append_tok(.LESS, "<", null),
                '>' => self.append_tok(.GREATER, ">", null),
                ' ', '\t' => continue,
                '\"' => self.eat_string(err_handler),
                '0'...'9' => self.eat_number(err_handler),
                'A'...'Z', 'a'...'z', '_' => self.eat_ident(),
                else => {
                    self.has_err = true;
                    self.prev_token = null;
                    try err_handler.send(try std.fmt.allocPrint(std.heap.page_allocator, "[line {d}] Error: Unexpected character: {c}\n", .{ self.line, ch }));
                    continue;
                },
            };
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

    fn peek(self: *Lexer) ?u8 {
        if (self.cursor < self.source.len) {
            return self.source[self.cursor];
        }
        return null;
    }

    fn match(self: *Lexer, expected: u8) bool {
        const eaten = self.eat();
        if (eaten) |ch| {
            if (ch == expected) {
                return true;
            }
        }
        self.cursor -= 1;
        return false;
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
            self.prev_token = null;
            try err_handler.send(try std.fmt.allocPrint(std.heap.page_allocator, "[line {d}] Error: Unterminated string.\n", .{self.line}));
            return;
        }
        try self.append_tok(.STRING, self.source[lit_start - 1 .. self.cursor], self.source[lit_start .. self.cursor - 1]);
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
        try self.append_tok(.NUMBER, lexme, blk: {
            if (is_disimal) {
                break :blk lit;
            } else {
                break :blk try std.mem.concat(std.heap.page_allocator, u8, &[_][]const u8{ lit, ".0" });
            }
        });
    }

    fn eat_ident(self: *Lexer) !void {
        const lit_start = self.cursor - 1;
        while (self.eat()) |eaten| {
            if (!((eaten >= 'a' and eaten <= 'z') or (eaten >= 'A' and eaten <= 'Z') or (eaten >= '0' and eaten <= '9') or eaten == '_')) {
                self.cursor -= 1;
                break;
            }
        }
        const ident = self.source[lit_start..self.cursor];
        try self.append_tok(blk: {
            var typ: Token.TokenTyp = .IDENTIFIER;
            if (self.keywords_map.get(ident)) |tt| {
                typ = tt;
            }
            break :blk typ;
        }, ident, null);
    }

    fn append_tok(self: *Lexer, typ: Token.TokenTyp, lexme: []const u8, lit: ?[]const u8) !void {
        const tok: Token = .{ .typ = typ, .lexme = lexme, .lit = lit, .line = self.line };
        self.prev_token = tok;
        try self.tokens.append(tok);
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
        var lexer = try Lexer.new(file_contents, alloc);

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
