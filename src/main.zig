const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Builtin = enum {
    exit,
    echo,
    pwd,
    cd,
    type,
    export_,
};

const Command = struct {
    typ: Typ,
    args: ?[][]const u8,

    const Tag = enum {
        builtin,
        exec,
    };

    const Typ = union(Tag) {
        builtin: Builtin,
        exec: External,
    };

    const External = struct {
        cmd: []const u8,
    };
};

const Token = struct {
    tag: Tag,
    word_idx: ?usize = null,

    const Tag = enum {
        // Word-like tokens (can be command, arg, var name etc.)
        word, // General purpose for sequences of non-special characters

        // Literals (content already processed by lexer, e.g., quotes stripped)
        // OR structural tokens if parser handles quote content
        single_quoted_literal, // '...' -> lexme is content
        double_quote_start, // "
        double_quote_end, // "
        // Chunks of text *inside* double quotes can be 'word' tokens

        // Variable related
        dollar, // $
        assignment,
        dollar_brace_open, // ${
        brace_open,
        brace_close, // }

        // Command Substitution
        dollar_paren_open, // $(
        backtick, // `
        paren_open,
        paren_close, // ) (can be for $( ) or other grouping)

        // Operators & Control
        pipe, // |
        and_cmd, // &&
        or_cmd, // ||
        semicolon, // ;
        background, // &
        redirect_out, // >
        redirect_out_append, // >>
        redirect_in, // <
        // redirect_heredoc, // << (more complex)

        space, // Keep for separation, parser will mostly consume
        eof, // End Of File/Input
    };
};

fn CursorIterator(comptime T: type) type {
    return struct {
        const Me = @This();
        items: []const T,
        cursor: usize,

        pub fn init(items: []const T) Me {
            return .{
                .items = items,
                .cursor = 0,
            };
        }

        pub fn next(me: *Me) ?T {
            if (me.cursor >= me.items.len) {
                return null;
            }
            const eaten = me.items[me.cursor];
            me.cursor += 1;
            return eaten;
        }

        pub fn peek(me: *const Me) ?T {
            if (me.cursor >= me.items.len) {
                return null;
            }
            return me.items[me.cursor];
        }
    };
}

const Lexer = struct {
    input: []const u8,
    cursor: usize,
    tokens: Tokens,
    words: Words, // stores the actual string content for the tokens
    allocator: Allocator,

    const Me = @This();
    const Tokens = std.ArrayList(Token);
    const Words = std.ArrayList([]const u8);

    pub fn init(input: []const u8, allocator: Allocator) !Me {
        return .{
            .input = input,
            .cursor = 0,
            .tokens = try Tokens.initCapacity(allocator, 5),
            .words = try Words.initCapacity(allocator, 5),
            .allocator = allocator,
        };
    }

    pub fn deinit(me: *Me) void {
        me.words.deinit();
        me.tokens.deinit();
    }

    fn eat(me: *Me) ?u8 {
        if (me.cursor >= me.input.len) {
            return null;
        }
        const eaten = me.input[me.cursor];
        me.cursor += 1;
        return eaten;
    }

    pub fn finshed(me: *const Me) bool {
        return me.cursor > me.input.len;
    }

    fn ve_cursor(me: *const Me) usize {
        if (me.finshed()) {
            return me.cursor - 1;
        }
        return me.cursor;
    }

    fn match(me: *Me, ch: u8) bool {
        if (me.cursor >= me.input.len) {
            return false;
        }
        if (me.input[me.cursor] == ch) {
            me.cursor += 1;
            return true;
        }
        return false;
    }

    fn peek(me: *const Me) ?u8 {
        if (me.finshed()) return null;
        return me.input[me.cursor];
    }

    fn add_token(me: *Me, tag: Token.Tag, word_slice: ?[]const u8) !void {
        var word_idx: ?usize = null;
        if (word_slice) |slice| {
            try me.words.append(slice);
            word_idx = me.words.items.len - 1;
        }
        try me.tokens.append(.{ .tag = tag, .word_idx = word_idx });
    }

    pub fn lex(me: *Me) !void {
        // const token_start_cursor = me.cursor;
        while (me.eat()) |ch| {
            switch (ch) {
                ' ', '\t', '\n' => {
                    if (me.tokens.items.len == 0) continue;
                    const prv = me.tokens.getLast().tag;
                    if (prv != .space and (prv == .single_quoted_literal or prv == .double_quote_end)) {
                        try me.tokens.append(.{ .tag = .space });
                    }
                },
                '\'' => try me.eat_single_quoted_lit(),
                '"' => try me.add_token(.double_quote_start, null),
                '$' => {
                    if (me.match('{')) {
                        try me.add_token(.dollar_brace_open, null);
                    } else if (me.match('(')) {
                        try me.add_token(.dollar_paren_open, null);
                    } else {
                        try me.add_token(.dollar, null);
                    }
                },
                '{' => try me.add_token(.brace_open, null),
                '}' => try me.add_token(.brace_close, null),
                '(' => try me.add_token(.paren_open, null),
                ')' => try me.add_token(.paren_close, null),
                '`' => try me.add_token(.backtick, null),
                '|' => {
                    if (me.match('|')) {
                        try me.add_token(.or_cmd, null);
                    } else {
                        try me.add_token(.pipe, null);
                    }
                },
                '&' => {
                    if (me.match('&')) {
                        try me.add_token(.and_cmd, null);
                    } else {
                        try me.add_token(.background, null);
                    }
                },
                ';' => try me.add_token(.semicolon, null),
                '>' => {
                    if (me.match('>')) {
                        try me.add_token(.redirect_out_append, null);
                    } else {
                        try me.add_token(.redirect_out, null);
                    }
                },
                '<' => {
                    // TODO: Handle Here Document '<<' logic if needed
                    try me.add_token(.redirect_in, null);
                },
                '=' => try me.add_token(.assignment, null),
                else => try me.eat_word(),
            }
        }
    }

    fn eat_single_quoted_lit(me: *Me) !void {
        const qoute_start = me.cursor;
        var escaped = false;
        while (me.eat()) |eaten| {
            if (eaten == '\\') {
                escaped = !escaped;
            } else if (eaten == '\'' and !escaped) {
                // TODO(anas): handle the unclosed quote
                break;
            } else {
                escaped = false;
            }
        }
        try me.add_token(.single_quoted_literal, me.input[qoute_start .. me.cursor - 1]);
    }

    fn eat_word(me: *Me) !void {
        var escaped = false;
        const cmd_start = me.cursor - 1;
        while (me.eat()) |eaten| {
            if (eaten == '\\') {
                escaped = true;
            } else if (!escaped and is_shell_metachar(eaten)) {
                me.cursor -= 1;
                break;
            } else {
                escaped = false;
            }
        }
        const cmd = me.input[cmd_start..me.ve_cursor()];
        try me.add_token(.word, cmd);
    }

    fn is_shell_metachar(char: u8) bool {
        return switch (char) {
            '|', '&', ';', '<', '>', '(', ')', '$', '`', '"', '=', '\'', ' ', '\t', '\n' => true,
            else => false,
        };
    }
};

pub fn FixedCircularStack(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Me = @This();
        data: [capacity]T = undefined,
        start: usize = 0,
        len: usize = 0,

        pub fn push(self: *Me, value: T) void {
            if (self.len < capacity) {
                const idx = (self.start + self.len) % capacity;
                self.data[idx] = value;
                self.len += 1;
            } else {
                // Overwrite oldest and move start forward
                self.data[self.start] = value;
                self.start = (self.start + 1) % capacity;
            }
        }

        pub fn pop(self: *Me) ?T {
            if (self.len == 0) return null;

            const idx = (self.start + self.len - 1) % capacity;
            const value = self.data[idx];
            self.len -= 1;
            return value;
        }

        pub fn peek(self: *Me) ?T {
            if (self.len == 0) return null;
            const idx = (self.start + self.len - 1) % capacity;
            return self.data[idx];
        }

        pub fn reset(self: *Me) void {
            self.start = 0;
            self.len = 0;
        }

        pub fn count(self: *Me) usize {
            return self.len;
        }
    };
}

const PATH_VAR = "PATH";
const HOME_VAR = "HOME";

const EnvVarsMap = std.process.EnvMap;
const AliasesMap = std.StringArrayHashMap([]const u8);
const PathBinsMap = std.StringArrayHashMap([]const u8);
const BuiltinsMap = std.StaticStringMap(Builtin);

const ShellCtx = struct {
    const Me = @This();
    const CdCircularStack = FixedCircularStack([]const u8, 5);
    env: EnvVarsMap,
    aliases: AliasesMap,
    path_bins: PathBinsMap,
    builtins: BuiltinsMap,
    pwd: []const u8,
    cd_stack: CdCircularStack,
    allocator: Allocator,

    pub fn init(allocator: Allocator) !Me {
        const builtins = BuiltinsMap.initComptime(comptime blk: {
            const builtin_valuse = std.enums.values(Builtin);
            var arr: [builtin_valuse.len]struct { []const u8, Builtin } = undefined;
            for (0..builtin_valuse.len) |idx| {
                const tag = builtin_valuse[idx];
                arr[idx] = .{ @tagName(tag), tag };
            }
            break :blk arr;
        });

        const env = try std.process.getEnvMap(allocator);
        const aliases = AliasesMap.init(allocator);
        const path_bins = PathBinsMap.init(allocator);
        const cwd = try std.process.getCwdAlloc(allocator);

        // Initialize cd_stack properly, ensure 'pwd' is also on stack if it's the first "previous" dir
        const cd_stack = CdCircularStack{};

        return .{
            .env = env,
            .aliases = aliases,
            .path_bins = path_bins,
            .builtins = builtins,
            .pwd = cwd,
            .cd_stack = cd_stack,
            .allocator = allocator,
        };
    }

    pub fn deinit(me: *Me) void {
        me.allocator.free(me.pwd);
        // Free cd_stack entries if they are owned
        for (0..me.cd_stack.len) |i| {
            const idx = (me.cd_stack.start + i) % me.cd_stack.data.len;
            me.allocator.free(me.cd_stack.data[idx]);
        }
        me.env.deinit();
        // For AliasesMap and PathBinsMap, if keys/values are duplicated, they need explicit freeing.
        // Assuming keys/values are either slices of static data or managed by getEnvMap/getCwdAlloc for now.
        // If `path_bins.put` duplicated strings, they need to be freed in a loop.
        var path_it = me.path_bins.iterator();
        while (path_it.next()) |entry| {
            // Assuming keys and values were duplicated with allocator
            me.allocator.free(entry.key_ptr.*);
            me.allocator.free(entry.value_ptr.*);
        }
        me.path_bins.deinit();

        var alias_it = me.aliases.iterator();
        while (alias_it.next()) |entry| {
            me.allocator.free(entry.key_ptr.*);
            me.allocator.free(entry.value_ptr.*);
        }
        me.aliases.deinit();
    }
};

const Shell = struct {
    const Me = @This();
    should_exit: bool,
    ctx: ShellCtx,
    allocator: Allocator,

    pub fn init(ctx: ShellCtx, allocator: Allocator) Me {
        return .{ .should_exit = false, .ctx = ctx, .allocator = allocator };
    }

    pub fn setup(me: *Me) !void {
        if (me.ctx.env.get(PATH_VAR)) |paths_raw| {
            const paths = try parse_path(paths_raw);
            defer paths.deinit();
            for (paths.items) |path| {
                var dir = std.fs.openDirAbsolute(path, .{ .access_sub_paths = true, .iterate = true }) catch continue;
                defer dir.close();
                var itr = dir.iterateAssumeFirstIteration();
                while (itr.next() catch continue) |entry| {
                    if (entry.kind == .file or entry.kind == .sym_link) {
                        const bin = dir.openFile(entry.name, .{}) catch continue;
                        defer bin.close();
                        const stat = bin.stat() catch continue;
                        if ((stat.mode & 0o111) != 0) {
                            try me.ctx.path_bins.put(try me.allocator.dupe(u8, entry.name), try me.allocator.dupe(u8, path));
                        }
                    }
                }
            }
        }
    }

    const ExecError = error{
        command_not_fond,
    };

    pub fn run(me: *Me, user_input: []const u8) !void {
        var lexer = try Lexer.init(user_input, me.allocator);
        defer lexer.deinit();
        try lexer.lex();
        const tokens = lexer.tokens;
        var tokens_itr = CursorIterator(Token).init(tokens.items);

        while (tokens_itr.next()) |tok| {
            const tag = tok.tag;
            if (tag == .word) {
                const program = lexer.words.items[tok.word_idx.?];

                // Collect arguments for the command
                var args_lexemes = try std.ArrayList([]const u8).initCapacity(me.allocator, lexer.tokens.items.len);
                defer args_lexemes.deinit();

                args_lexemes.appendAssumeCapacity(program);

                while (tokens_itr.peek()) |next_token_struct| {
                    const next_tag = next_token_struct.tag;
                    if (next_tag == .space) {
                        _ = tokens_itr.next(); // Consume space
                        args_lexemes.appendAssumeCapacity(" ");
                        continue;
                    }
                    if (next_tag == .dollar) {
                        if (tokens_itr.peek()) |nn_tok| {
                            if (nn_tok.tag == .space) {
                                args_lexemes.appendAssumeCapacity("$");
                            } else if (nn_tok.tag == .word) {
                                if (nn_tok.word_idx) |idx| {
                                    if (me.ctx.env.get(lexer.words.items[idx])) |val| {
                                        args_lexemes.appendAssumeCapacity(val);
                                    }
                                }
                            }
                        }
                    }
                    // These tokens are considered part of arguments
                    if (next_tag == .word or
                        next_tag == .single_quoted_literal or
                        next_tag == .assignment or // VAR=val can be an argument
                        // For full shell, also: .double_quote_start (then parse until end), variable tokens after expansion.
                        // For now, keeping it simple for what lexer produces as whole lexemes.
                        next_tag == .backtick // `foo` (after expansion)
                    // TODO: add more arg types like contents of double quotes, var expansions
                    ) {
                        _ = tokens_itr.next(); // Consume argument token
                        if (next_token_struct.word_idx) |arg_idx| {
                            args_lexemes.appendAssumeCapacity(lexer.words.items[arg_idx]);
                        }
                        // Other tokens without word_idx (like .dollar_brace_open) would need expansion first.
                    } else {
                        break; // Not an argument token for a simple command (e.g., pipe, semicolon, eof)
                    }
                }

                // NOTE: codecrafters wants to prefer the builtin on the path, so if the command is available in both of the path and our
                // shell builtins, we would prefer the builtin; for me i would prefer the opposite
                if (me.ctx.builtins.get(program)) |builtin| {
                    try me.exec_builtin(builtin, args_lexemes.items[1..]);
                    return;
                }
                if (me.ctx.path_bins.get(program)) |ex_bin_path| {
                    // cmd.typ = .{ .exec = .{ .cmd = try mem.concat(me.allocator, u8, &[_][]const u8{ ex_bin_path, "/", cmd }) } });
                    // NOTE: maybe its better approach to use the full bin path
                    // args.insertAssumeCapacity(0, try mem.concat(me.allocator, u8, &[_][]const u8{ ex_bin_path, "/", program }));
                    _ = ex_bin_path;

                    var cp = std.process.Child.init(args_lexemes.items, me.allocator);
                    cp.cwd = me.ctx.pwd;
                    // TODO: insert local vars
                    cp.env_map = &me.ctx.env;
                    if (cp.spawnAndWait()) |term| {
                        _ = term;
                    } else |err| {
                        switch (err) {
                            error.FileNotFound => try std.io.getStdOut().writer().print("{s}: command not found\n", .{program}),

                            else => std.debug.print("{any}\n", .{err}),
                        }
                    }
                    return;
                }
                return error.command_not_fond;
            } else if (tag == .dollar) {} else if (tag == .dollar_brace_open) {}
        }
    }

    fn exec_builtin(me: *Me, command: Builtin, args: [][]const u8) !void {
        switch (command) {
            .exit => {
                // TODO: handle the optional exit code arg
                me.should_exit = true;
                return;
            },
            .echo => {
                var space = false;
                for (args) |arg| {
                    if (space) {
                        _ = try std.io.getStdOut().write(" ");
                    }
                    _ = try std.io.getStdOut().write(arg);
                    space = true;
                }
                _ = try std.io.getStdOut().write("\n");
            },
            .type => {
                if (args.len == 0) {
                    _ = try std.io.getStdErr().write("need one or more argument\n");
                    return;
                }
                for (args) |arg| {
                    const cmd = arg;
                    // NOTE: codecrafters wants to prefer the builtin on the path, so if the command is available in both of the path and our
                    // shell builtins, we would prefer the builtin; for me i would prefer the opposite
                    if (me.ctx.builtins.getIndex(cmd)) |_| {
                        try std.io.getStdOut().writer().print("{s} is a shell builtin\n", .{cmd});
                    } else if (me.ctx.path_bins.get(cmd)) |path| {
                        try std.io.getStdOut().writer().print("{s} is {s}/{s}\n", .{ cmd, path, cmd });
                    } else {
                        try std.io.getStdOut().writer().print("{s}: not found\n", .{cmd});
                    }
                }
                // const arg = lexmes.items[idx];
                // if (me.ctx.path_bins.get())
            },
            .pwd => _ = try std.io.getStdOut().writer().print("{s}\n", .{me.ctx.pwd}),
            .cd => {
                //
                var abslute_path: []const u8 = undefined;
                if (args.len == 0) {
                    // cd home
                    if (me.ctx.env.get(HOME_VAR)) |home| {
                        abslute_path = home;
                    }
                } else if (mem.eql(u8, args[0], "-")) {
                    abslute_path = me.ctx.cd_stack.data[0];
                } else {
                    const target = args[0];
                    var buffer = try std.ArrayList(u8).initCapacity(std.heap.page_allocator, target.len);
                    defer buffer.deinit();
                    try me.expand_relative_path(target, &buffer);
                    abslute_path = try std.heap.page_allocator.dupe(u8, buffer.items);
                }
                var dir = std.fs.openDirAbsolute(abslute_path, .{ .iterate = false, .access_sub_paths = false }) catch |err| {
                    switch (err) {
                        error.FileNotFound => try std.io.getStdErr().writer().print("cd: {s}: No such file or directory\n", .{abslute_path}),
                        else => std.debug.print("Failed to open directory '{s}': {any}\n", .{ abslute_path, err }),
                    }
                    return;
                };
                dir.close();
                me.ctx.cd_stack.push(me.ctx.pwd);
                me.ctx.pwd = abslute_path;
            },
            .export_ => @panic("unimplemented"),
        }
    }

    fn expand_relative_path(me: *const Me, path: []const u8, out: *std.ArrayList(u8)) !void {
        var escaped = false;
        var in_single_quote = false;
        var itr = CursorIterator(u8).init(path);
        if (itr.peek()) |first| {
            if (first == '~') {
                if (me.ctx.env.get(HOME_VAR)) |home| {
                    try out.appendSlice(home);
                    _ = itr.next();
                }
            } else if (first != '/') {
                try out.appendSlice(me.ctx.pwd);
                try out.append('/');
            }
        }
        while (itr.next()) |np| {
            if (escaped or in_single_quote) {
                escaped = false;
                try out.append(np);
                continue;
            }
            switch (np) {
                '\\' => {
                    escaped = true;
                    continue;
                },
                '\'' => {
                    try out.append(np);
                    in_single_quote = !in_single_quote;
                    continue;
                },
                '.' => {
                    if (itr.peek()) |next| {
                        if (next == '.') {
                            _ = itr.next();
                            if (itr.peek() == '.') {
                                try std.io.getStdErr().writeAll("No\n");
                                return;
                            }
                            // shop a one level from the current path
                            if (out.items.len == 1) {
                                // there's no place we can go to
                                continue;
                            }
                            var first = true;
                            while (true) {
                                if ((out.pop() == '/' and !first) or out.items.len == 1) {
                                    break;
                                }
                                first = false;
                            }
                        }
                    }
                },
                '/' => {
                    if (out.getLastOrNull()) |last| {
                        if (last == '/') {
                            continue;
                        }
                    }
                    if (itr.peek()) |_| {
                        try out.append('/');
                    }
                },
                else => try out.append(np),
            }
        }
    }
};

pub fn main() !void {
    // Uncomment this block to pass the first stage
    const stdout = std.io.getStdOut().writer();

    const stdin = std.io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;
    const allocator = std.heap.page_allocator;
    const shell_ctx = try ShellCtx.init(allocator);
    var shell = Shell.init(shell_ctx, allocator);
    try shell.setup();
    while (true) {
        try stdout.print("$ ", .{});
        const user_input = try stdin.readUntilDelimiter(&buffer, '\n');

        // short-circit
        if (user_input.len == 0) {
            continue;
        }

        shell.run(user_input) catch |err| {
            switch (err) {
                error.command_not_fond => try stdout.print("{s}: command not found\n", .{user_input}),
                else => return err,
            }
        };
        if (shell.should_exit) {
            std.process.exit(0);
        }
    }
}

fn parse_path(paths_raw: []const u8) !std.ArrayList([]const u8) {
    var paths = std.ArrayList([]const u8).init(std.heap.page_allocator);
    var spliter = mem.tokenizeAny(u8, paths_raw, ":");
    while (spliter.next()) |p| {
        //TODO: expand the vars first
        try paths.append(p);
    }
    return paths;
}
