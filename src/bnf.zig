const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

pub const Args = struct {
    filepath: []const u8 = "",
    start_sym: []const u8 = "",
};

pub fn firstN(s: []const u8, n: usize) []const u8 {
    return s[0..@min(s.len, n)];
}

pub const Grammar = struct {
    rules: std.StringArrayHashMapUnmanaged(Node) = .{},

    pub fn deinit(g: *Grammar, alloc: Allocator) void {
        for (g.rules.values()) |*val| {
            val.deinit(alloc);
        }
        g.rules.deinit(alloc);
    }

    pub fn format(g: Grammar, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const vals = g.rules.values();
        for (g.rules.keys()) |key, i| {
            _ = try writer.write("<");
            _ = try writer.write(key);
            _ = try writer.write("> ::= ");
            try writer.print("{}\n", .{vals[i]});
        }
    }

    pub fn gen(g: Grammar, start_sym: []const u8, writer: anytype, rand: std.rand.Random) !void {
        var rule = g.rules.get(start_sym) orelse return error.InvalidStartSymbol;
        try g.genImpl(rule, writer, rand);
    }

    fn genImpl(g: Grammar, rule: Node, writer: anytype, rand: std.rand.Random) !void {
        switch (rule) {
            .sym => |s| {
                const newrule = g.rules.get(s) orelse {
                    std.debug.print("missing rule '{s}'\n", .{s});
                    return error.MissingRule;
                };
                try @call(.always_tail, genImpl, .{ g, newrule, writer, rand });
            },
            .dstr, .sstr => |s| _ = try writer.write(s),
            .seq => |s| for (s.items) |n|
                try g.genImpl(n, writer, rand),
            .alt, .group => |s| {
                const i = rand.uintLessThan(u32, @intCast(u32, s.items.len));
                try @call(.always_tail, genImpl, .{ g, s.items[i], writer, rand });
            },
        }
    }
};

const Node = union(enum) {
    sym: []const u8,
    dstr: []const u8,
    sstr: []const u8,
    seq: List,
    alt: List,
    group: List,

    pub const List = std.ArrayListUnmanaged(Node);

    pub fn deinit(n: *Node, alloc: Allocator) void {
        switch (n.*) {
            .sym, .dstr, .sstr => {},
            .seq, .alt, .group => |*s| {
                for (s.items) |*nn|
                    nn.deinit(alloc);
                s.deinit(alloc);
            },
        }
    }

    pub fn format(n: Node, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (n) {
            .sym => |s| try writer.print("<{s}>", .{s}),
            .dstr => |s| try writer.print("\"{}\"", .{std.zig.fmtEscapes(s)}),
            .sstr => |s| try writer.print("'{'}'", .{std.zig.fmtEscapes(s)}),
            .seq => |s| for (s.items) |nn, i| {
                if (i != 0) _ = try writer.write(" ");
                try writer.print("{}", .{nn});
            },
            .alt => |s| for (s.items) |nn, i| {
                if (i != 0) _ = try writer.write(" | ");
                try writer.print("{}", .{nn});
            },
            .group => |s| {
                _ = try writer.write("(");
                for (s.items) |nn, i| {
                    if (i != 0) _ = try writer.write(" | ");
                    try writer.print("{}", .{nn});
                }
                _ = try writer.write(")");
            },
        }
    }
};

pub const Parser = struct {
    alloc: Allocator,
    content: []u8,
    content_start: []const u8,
    args: Args,
    grammar: Grammar = .{},

    pub fn init(alloc: Allocator, content: []u8, args: Args) Parser {
        return .{
            .alloc = alloc,
            .content = content,
            .content_start = content,
            .args = args,
        };
    }

    pub fn deinit(p: *Parser) void {
        p.grammar.deinit(p.alloc);
    }

    pub fn trimWs(p: *Parser) void {
        var content = mem.trimLeft(u8, p.content, &std.ascii.whitespace);
        p.content.ptr = @intToPtr([*]u8, @ptrToInt(content.ptr));
        p.content.len = content.len;
    }
    fn errFmt(p: Parser, comptime fmt: []const u8, args: anytype) void {
        var line: u32 = 1;
        var col: u32 = 1;
        const len = @ptrToInt(p.content.ptr) - @ptrToInt(p.content_start.ptr);
        for (p.content_start[0..len]) |c| {
            if (c == '\n') {
                col = 1;
                line += 1;
            } else col += 1;
        }
        std.debug.print("{s}:{}:{}: " ++ fmt, .{ p.args.filepath, line, col } ++ args);
    }
    pub fn readUntilChar(p: *Parser, char: u8, skiplen: u32) ![]u8 {
        const end = mem.indexOfScalar(u8, p.content[1..], char) orelse {
            p.errFmt("didn't find expected char '{c}'\n", .{char});
            return error.CharNotFound;
        } + 1;
        const result = p.content[skiplen .. skiplen + end];
        p.content = p.content[end + skiplen + 1 ..];
        return result;
    }

    pub fn expectStr(p: *Parser, str: []const u8) !void {
        if (!mem.startsWith(u8, p.content, str)) {
            p.errFmt("expecting '{s}' but found '{s}'..\n", .{ str, firstN(p.content, 10) });
            return error.StringNotFound;
        }
        p.content = p.content[str.len..];
    }
    fn escapeStr(str: []u8) ![]u8 {
        var readidx: u32 = 0;
        var writeidx: u32 = 0;
        while (readidx < str.len) {
            const c = str[readidx];
            const isesc = c == '\\';
            str[writeidx] = if (isesc)
                switch (str[readidx + 1]) {
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    else => |c2| std.debug.panic("TODO escape '{c}'", .{c2}),
                }
            else
                c;
            readidx += @as(u2, @boolToInt(isesc)) + 1;
            writeidx += 1;
        }
        return str[0..writeidx];
    }

    fn parseSeq(p: *Parser, seq: *Node.List, start: *[]const u8, lastsym: *[]const u8) !void {
        while (true) {
            p.trimWs();
            if (p.content.len == 0) break;
            if (lastsym.len > 0 and p.content[0] != ':') {
                try seq.append(p.alloc, .{ .sym = lastsym.* });
                lastsym.* = "";
            }
            switch (p.content[0]) {
                '<' => {
                    start.* = p.content;
                    lastsym.* = try p.readUntilChar('>', 1);
                },
                '"' => {
                    var str = try p.readUntilChar('"', 1);
                    try seq.append(p.alloc, .{ .dstr = try escapeStr(str) });
                },
                '\'' => {
                    var str = try p.readUntilChar('\'', 1);
                    try seq.append(p.alloc, .{ .sstr = try escapeStr(str) });
                },
                '|', ')' => {
                    break;
                },
                '(' => {
                    p.content = p.content[1..];
                    const subseq = try seq.addOne(p.alloc);
                    subseq.* = .{ .group = .{} };
                    try p.parseGroup(&subseq.group);
                },
                ':' => {
                    break;
                },
                else => {
                    p.errFmt("unexpected token '{s}'..\n", .{firstN(p.content, 10)});
                    return error.UnexpectedContent;
                },
            }
        }
    }
    fn parseChoices(p: *Parser, alt: *Node.List) !void {
        var lastsym: []const u8 = "";
        var start = p.content;
        var choice = try alt.addOne(p.alloc);
        choice.* = .{ .seq = .{} };
        while (true) {
            try p.parseSeq(&choice.seq, &start, &lastsym);
            p.trimWs();
            if (p.content.len == 0) break;

            switch (p.content[0]) {
                '|' => {
                    p.content = p.content[1..];
                    choice = try alt.addOne(p.alloc);
                    choice.* = .{ .seq = .{} };
                },
                ':' => {
                    p.content = start;
                    break;
                },
                ')' => {
                    p.content = p.content[1..];
                    break;
                },
                else => {
                    p.errFmt("unexpected token '{s}'..\n", .{firstN(p.content, 10)});
                    return error.UnexpectedContent;
                },
            }
        }
    }
    const Error = error{ UnexpectedContent, CharNotFound } || mem.Allocator.Error;
    fn parseGroup(p: *Parser, seq: *Node.List) Error!void {
        p.trimWs();
        if (p.content.len == 0) return;
        switch (p.content[0]) {
            ')' => {
                p.content = p.content[1..];
            },
            else => {
                try p.parseChoices(seq);
            },
        }
    }

    pub fn parse(p: *Parser) !Grammar {
        // var p: Parser = .{ .alloc = alloc, .content = content, .content_start = content, .args = args, .grammar = .{} };
        while (true) {
            p.trimWs();
            if (p.content.len == 0) break;
            switch (p.content[0]) {
                '<' => {
                    const rule_name = try p.readUntilChar('>', 1);
                    p.trimWs();
                    try p.expectStr("::=");
                    const gop = try p.grammar.rules.getOrPut(p.alloc, rule_name);
                    if (gop.found_existing) {
                        p.errFmt("duplicate rule: '{s}'..\n", .{rule_name});
                        return error.DuplicateRule;
                    }
                    gop.value_ptr.* = .{ .alt = .{} };
                    try p.parseChoices(&gop.value_ptr.alt);
                },
                else => {
                    p.errFmt("unexpected content: '{s}'..\n", .{firstN(p.content, 10)});
                    return error.InvalidToken;
                },
            }
        }
        return p.grammar;
    }
};
