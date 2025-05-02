const std = @import("std");
const mem = std.mem;

const Builtin = enum {
    exit,
    echo,
    pwd,
    cd,
    type,
};

const CommandTag = enum {
    builtin,
    exec,
};

const Command = union(CommandTag) {
    builtin: Builtin,
    exec: ExecutableCommand,
};

const ExecutableCommand = struct {
    cmd: []const u8,
};

const CommandsList = std.ArrayList(Command);

const Lexer = struct {
    const Me = @This();
    input: []const u8,
    cursor: usize,
    lexmes: CommandsList,
    ctx: *const ShellCtx,

    pub fn init(input: []const u8, ctx: *const ShellCtx, allocator: mem.Allocator) !Me {
        return .{
            .input = input,
            .cursor = 0,
            .lexmes = try CommandsList.initCapacity(allocator, 5),
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
                ' ' => continue,
                else => try me.eat_cmd(),
            }
        }
    }

    fn eat_cmd(me: *Me) !void {
        const cmd_start = me.cursor - 1;
        while (me.eat()) |eaten| {
            if (eaten == ' ' or eaten == '&') {
                me.cursor -= 1;
                break;
            }
        }
        const cmd = me.input[cmd_start..me.ve_cursor()];

        if (me.ctx.path_bins.get(cmd)) |ex_bin_path| {
            try me.lexmes.append(.{ .exec = .{ .cmd = try mem.concat(std.heap.page_allocator, u8, &[_][]const u8{ ex_bin_path, "/", cmd }) } });
            return;
        }
        if (me.ctx.builtins.get(cmd)) |builtin| {
            try me.lexmes.append(.{ .builtin = builtin });
            return;
        }

        try me.lexmes.append(.{ .exec = .{ .cmd = cmd } });
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
            std.debug.print("PATH: {s}\n", .{paths_raw});
            const paths = try parse_path(paths_raw);
            defer paths.deinit();
            for (paths.items) |path| {
                std.debug.print("\n\n PATH: {s}\n", .{path});
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
        const lexmes = lexer.lexmes;
        defer lexmes.deinit();

        var idx: usize = 0;
        while (idx < lexmes.items.len) {
            const lex = lexmes.items[idx];
            idx += 1;
            switch (lex) {
                .exec => |exe| {
                    _ = exe;
                    return ExecError.command_not_fond;
                },
                .builtin => |b| {
                    switch (b) {
                        .exit => {
                            // TODO: handle the optional exit code arg
                            me.should_exit = true;
                            break;
                        },
                        .echo => {
                            var space = false;
                            while (idx < lexmes.items.len) {
                                const ilex = lexmes.items[idx];
                                idx += 1;
                                if (space) {
                                    _ = try std.io.getStdOut().write(" ");
                                }
                                _ = try std.io.getStdOut().write(switch (ilex) {
                                    .exec => |e| e.cmd,
                                    .builtin => |ib| @tagName(ib),
                                });
                                space = true;
                            }
                            _ = try std.io.getStdOut().write("\n");
                        },
                        .type => std.log.debug("type builtin", .{}),
                        .pwd => _ = try std.io.getStdOut().write(me.ctx.pwd),
                        .cd => std.log.debug("cd builtin", .{}),
                    }
                },
            }
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
    // try shell.setup();
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
