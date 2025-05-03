const std = @import("std");
const mem = std.mem;

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
    typ: Typ,
    lexme: ?[]const u8,

    const Typ = enum {
        command,
        background,
        and_cmd,
        or_cmd,
        pipe,
        redirect,
        redirect_append,
        variable,
        string,
        export_local,
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
    };
}

const Lexer = struct {
    const Me = @This();
    const Tokens = std.ArrayList(Token);
    input: []const u8,
    cursor: usize,
    tokens: Tokens,
    ctx: *const ShellCtx,

    pub fn init(input: []const u8, ctx: *const ShellCtx, allocator: mem.Allocator) !Me {
        return .{
            .input = input,
            .cursor = 0,
            .tokens = try Tokens.initCapacity(allocator, 5),
            .ctx = ctx,
        };
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

    pub fn lex(me: *Me) !void {
        while (me.eat()) |ch| {
            switch (ch) {
                '&' => @panic("unimplemented"),
                '\"', '\'' => @panic("unimplemented"),
                '|' => @panic("unimplemented"),
                '$' => @panic("unimplemented"),
                '`' => @panic("unimplemented"),
                ' ' => continue,
                else => try me.eat_cmd_or_var(),
            }
        }
    }

    fn eat_cmd_or_var(me: *Me) !void {
        var escape = false;
        const cmd_start = me.cursor - 1;
        while (me.eat()) |eaten| {
            if (eaten == '\\') {
                escape = true;
                continue;
            }
            if (eaten == ' ') {
                me.cursor -= 1;
                break;
            }
            if (eaten == '=') {
                if (escape) {
                    escape = false;
                    continue;
                }
                // oh, no, this mf wanna local export
                try me.eat_local_export(cmd_start);
                break;
            }
            if (eaten == '&' or eaten == '$' or eaten == '|' or eaten == '>') {
                if (escape) {
                    escape = false;
                    continue;
                }
                me.cursor -= 1;
                break;
            }
        }
        // args?
        // if (has_args) {}
        const cmd = me.input[cmd_start..me.ve_cursor()];
        try me.tokens.append(.{ .typ = .command, .lexme = cmd });
    }

    fn eat_local_export(me: *Me, var_start: usize) !void {
        // we have eaten the `=` already so no need to check that
        var escaped = false;
        var in_single_quote = false;
        var in_d_quote = false;
        while (me.eat()) |eaten| {
            if (eaten == '\'' and !escaped) {
                in_single_quote = !in_single_quote;
                continue;
            } else if (eaten == '\"' and !escaped) {
                in_d_quote = true;
                continue;
            } else if (eaten == '\\') {
                escaped = !escaped;
                continue;
            }

            if (eaten == ' ' and !in_single_quote and !in_d_quote) {
                break;
            }
            // if ((in_single_quote and eaten == '\'') or eaten == '\"' and !escaped) {
            //     break;
            // }
        }
        try me.tokens.append(.{ .typ = .export_local, .lexme = me.input[var_start..me.ve_cursor()] });
    }
};

const PATH_VAR = "PATH";

const EnvVarsMap = std.process.EnvMap;
const AliasesMap = std.StringArrayHashMap([]const u8);
const PathBinsMap = std.StringArrayHashMap([]const u8);
const BuiltinsMap = std.StringArrayHashMap(Builtin);

const ShellCtx = struct {
    const Me = @This();
    env: EnvVarsMap,
    aliases: AliasesMap,
    path_bins: PathBinsMap,
    builtins: BuiltinsMap,
    pwd: []const u8,

    pub fn init() !Me {
        var builtins = BuiltinsMap.init(std.heap.page_allocator);
        const builtin_valuse = std.enums.values(Builtin);
        try builtins.ensureTotalCapacity(builtin_valuse.len);
        for (builtin_valuse) |b| {
            builtins.putAssumeCapacity(@tagName(b), b);
        }
        const env = try std.process.getEnvMap(std.heap.page_allocator);
        const aliases = AliasesMap.init(std.heap.page_allocator);
        const path_bins = PathBinsMap.init(std.heap.page_allocator);
        const cwd = try std.process.getCwdAlloc(std.heap.page_allocator);

        return .{
            .env = env,
            .aliases = aliases,
            .path_bins = path_bins,
            .builtins = builtins,
            .pwd = cwd,
        };
    }
};

const Shell = struct {
    const Me = @This();
    should_exit: bool,
    ctx: ShellCtx,

    pub fn init(ctx: ShellCtx) Me {
        return .{ .should_exit = false, .ctx = ctx };
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
                            try me.ctx.path_bins.put(try std.heap.page_allocator.dupe(u8, entry.name), try std.heap.page_allocator.dupe(u8, path));
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
        var lexer = try Lexer.init(user_input, &me.ctx, std.heap.page_allocator);
        try lexer.lex();
        const tokens = lexer.tokens;
        defer tokens.deinit();
        var tokens_itr = CursorIterator(Token).init(tokens.items);

        while (tokens_itr.next()) |tok| {
            switch (tok.typ) {
                .command => {
                    var args = try std.ArrayList([]const u8).initCapacity(std.heap.page_allocator, 5);
                    while (tokens_itr.next()) |ntok| {
                        if (!(ntok.typ == .command or ntok.typ == .variable or ntok.typ == .string)) {
                            tokens_itr.cursor -= 1;
                            break;
                        }
                        try args.append(ntok.lexme.?);
                    }

                    const cmd = tok.lexme.?;
                    if (me.ctx.path_bins.get(cmd)) |ex_bin_path| {
                        // cmd.typ = .{ .exec = .{ .cmd = try mem.concat(std.heap.page_allocator, u8, &[_][]const u8{ ex_bin_path, "/", cmd }) } });
                        _ = ex_bin_path;
                        return;
                    }
                    if (me.ctx.builtins.get(cmd)) |builtin| {
                        try me.exec_builtin(builtin, args.items);
                        return;
                    }
                    return error.command_not_fond;
                },
                .background => @panic("unimplemented"),
                .and_cmd => @panic("unimplemented"),
                .or_cmd => @panic("unimplemented"),
                .pipe => @panic("unimplemented"),
                .redirect => @panic("unimplemented"),
                .redirect_append => @panic("unimplemented"),
                .variable => @panic("unimplemented"),
                .string => @panic("unimplemented"),
                .export_local => {},
            }
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
                    var exsists = false;
                    if (me.ctx.path_bins.get(arg)) |path| {
                        try std.io.getStdOut().writer().print("{s}: is {s}/{s}\n", .{ arg, path, arg });
                        exsists = true;
                    }
                    if (me.ctx.builtins.getIndex(arg)) |_| {
                        try std.io.getStdOut().writer().print("{s} is a shell builtin\n", .{arg});
                        exsists = true;
                    }
                    if (!exsists) {
                        try std.io.getStdOut().writer().print("{s}: not found\n", .{arg});
                    }
                }
                // const arg = lexmes.items[idx];
                // if (me.ctx.path_bins.get())
            },
            .pwd => _ = try std.io.getStdOut().write(me.ctx.pwd),
            .cd => std.log.debug("cd builtin", .{}),
            .export_ => @panic("unimplemented"),
        }
    }
};

pub fn main() !void {
    // Uncomment this block to pass the first stage
    const stdout = std.io.getStdOut().writer();

    const stdin = std.io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;
    const shell_ctx = try ShellCtx.init();
    var shell = Shell.init(shell_ctx);
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
