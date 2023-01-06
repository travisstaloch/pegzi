const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const Allocator = mem.Allocator;

pub const Args = struct {
    filepath: []const u8 = "",
    start_sym: []const u8 = "",
};

pub fn firstN(s: []const u8, n: usize) []const u8 {
    return s[0..@min(s.len, n)];
}

pub fn panicf(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.panic(fmt, args);
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    std.log.scoped(.peg).debug(fmt, args);
}
pub fn println(comptime fmt: []const u8, args: anytype) void {
    // TODO remove this temporary hack to avoid printing extra newlines
    // https://github.com/ziglang/zig/issues/12201
    if (@import("builtin").is_test)
        print(fmt, args)
    else
        print(fmt ++ "\n", args);
}

pub const Grammar = struct {
    rules: std.StringArrayHashMapUnmanaged(Node) = .{},
    eof_identifier: []const u8 = "eof",

    pub fn format(g: Grammar, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const vals = g.rules.values();
        for (g.rules.keys()) |key, i| {
            try writer.print("{s} <- {}\n", .{ key, vals[i] });
        }
    }

    pub fn deinit(g: *Grammar, alloc: Allocator) void {
        for (g.rules.values()) |*val| {
            val.deinit(alloc);
        }
        g.rules.deinit(alloc);
    }
};

// ---
// --- these are ordered groups. do not change the order
// ---
pub const Node = union(enum) {
    sym: StrWithFlags,
    dstr: StrWithFlags,
    sstr: StrWithFlags,
    char_set: StrWithFlags,
    dot: VoidWithFlags,
    group: NodeWithFlags,
    action: StrWithFlags,
    // end atoms
    seq: ListWithFlags,
    alt: ListWithFlags,

    pub const List = std.ArrayListUnmanaged(Node);
    pub const Flag = enum { many, some, opt, not, amp };
    pub const Flags = std.enums.EnumSet(Flag);
    pub const Tag = std.meta.Tag(Node);
    pub fn WithFlags(comptime T: type) type {
        return struct {
            payload: T,
            flags: Flags = Flags.initEmpty(),
            pub fn init(payload: T) @This() {
                return .{ .payload = payload };
            }
        };
    }

    pub const StrWithFlags = WithFlags([]const u8);
    pub const ListWithFlags = WithFlags(List);
    pub const VoidWithFlags = WithFlags(void);
    pub const NodeWithFlags = WithFlags(*Node);
    pub fn init(comptime tag: Tag, payload: anytype) Node {
        return @unionInit(Node, @tagName(tag), .{ .payload = payload });
    }

    pub fn deinit(n: *Node, alloc: Allocator) void {
        switch (n.*) {
            .sym, .dstr, .sstr, .char_set, .dot, .action => {},
            .seq, .alt => |*s| {
                for (s.payload.items) |*nn|
                    nn.deinit(alloc);
                s.payload.deinit(alloc);
            },
            .group => |g| {
                g.payload.deinit(alloc);
                alloc.destroy(g.payload);
            },
        }
    }

    pub fn format(n: Node, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const nflags = n.flagsConst();
        const many = nflags.contains(.many);
        const some = nflags.contains(.some);
        const opt = nflags.contains(.opt);
        if (nflags.contains(.not)) try writer.writeByte('!');
        // _ = try writer.write(@tagName(n));
        // _ = try writer.write(": ");

        switch (n) {
            .sym => |s| try writer.print("{s}", .{s.payload}),
            .dstr => |s| try writer.print("\"{}\"", .{std.zig.fmtEscapes(s.payload)}),
            .sstr => |s| try writer.print("'{'}'", .{std.zig.fmtEscapes(s.payload)}),
            .char_set => |s| try writer.print("[{}]", .{std.zig.fmtEscapes(s.payload)}),
            .seq => |s| for (s.payload.items) |*nn, i| {
                if (i != 0) try writer.writeByte(' ');
                try writer.print("{}", .{nn.*});
            },
            .alt => |s| for (s.payload.items) |nn, i| {
                if (i != 0) _ = try writer.write(" / ");
                try writer.print("{}", .{nn});
            },
            .group => |s| {
                try writer.writeByte('(');
                try writer.print("{}", .{s.payload.*});
                try writer.writeByte(')');
            },
            .dot => try writer.writeByte('.'),
            .action => |s| {
                try writer.writeByte('{');
                _ = try writer.write(s.payload);
                try writer.writeByte('}');
            },
        }
        if (many) try writer.writeByte('*');
        if (some) try writer.writeByte('+');
        if (opt) try writer.writeByte('?');
    }

    pub inline fn flags(n: *Node) *Flags {
        return switch (n.*) {
            inline else => |*p| &p.flags,
        };
    }

    pub inline fn setFlags(n: *Node, f: Flags) void {
        return switch (n.*) {
            inline else => |*p| p.flags = f,
        };
    }

    pub fn flagsConst(n: Node) Flags {
        return switch (n) {
            inline else => |p| p.flags,
        };
    }

    pub fn isAtom(n: Node) bool {
        return @enumToInt(@as(Tag, n)) <= @enumToInt(@as(Tag, .group));
    }
};

pub const CharSet = struct {
    sets: Sets = Sets.init(0) catch unreachable,
    negated: bool = false,

    pub const Set = union(enum) {
        chars: []const u8,
        range: [2]u8,
    };
    pub const Sets = std.BoundedArray(Set, 64);
};

pub const Parser = struct {
    alloc: Allocator,
    content: []u8,
    content_start: []const u8,
    args: Args,
    grammar: Grammar = .{},
    peeks: Peeks = Peeks.init(0) catch unreachable,

    const Peeks = std.BoundedArray(Token, 4); // TODO use ringbuffer
    pub const Error = error{
        UnexpectedContent,
        CharNotFound,
        InvalidEscape,
        InvalidStringLiteral,
        BadPeek,
        Group,
        UnexpectedToken,
    } ||
        mem.Allocator.Error ||
        std.fmt.ParseIntError ||
        std.io.FixedBufferStream([]u8).WriteError;

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

    fn skipWsImpl(p: *Parser) void {
        // this hack necessary because content is []u8 while trimLeft returns []const u8
        var content = mem.trimLeft(u8, p.content, &std.ascii.whitespace);
        p.content.ptr = @intToPtr([*]u8, @ptrToInt(content.ptr));
        p.content.len = content.len;
    }

    fn skipWs(p: *Parser) void {
        p.skipWsImpl();
        while (p.content.len > 0 and p.content[0] == '#') {
            const nlidx = mem.indexOfScalar(u8, p.content, '\n') orelse p.content.len;
            p.content = p.content[nlidx..];
            p.skipWsImpl();
        }
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
        std.log.err("{s}:{}:{}: " ++ fmt, .{ p.args.filepath, line, col } ++ args);
    }

    fn ItemParser(comptime T: type) type {
        return fn (T) bool;
    }

    fn not(comptime T: type, comptime p: ItemParser(T)) ItemParser(T) {
        return struct {
            fn func(c: T) bool {
                return !p(c);
            }
        }.func;
    }

    fn isItem(comptime T: type, comptime c: T) ItemParser(T) {
        return struct {
            fn func(d: T) bool {
                return c == d;
            }
        }.func;
    }

    fn oneOf(comptime T: type, comptime p: ItemParser(T), comptime q: ItemParser(T)) ItemParser(T) {
        return struct {
            fn func(c: T) bool {
                return p(c) or q(c);
            }
        }.func;
    }

    // escapes \n, \r, \\, \t, \', \", \NNN (octal)
    pub fn parseEscapeSequence(p: *Parser, slice: []const u8, offsetp: *usize) !u8 {
        const offset = offsetp.*;
        assert(slice.len > offset);
        assert(slice[offset] == '\\');

        if (slice.len == offset + 1) return error.InvalidEscape;

        var skiplen: u8 = 2;
        defer offsetp.* += skiplen;
        switch (slice[offset + 1]) {
            'n' => return '\n',
            'r' => return '\r',
            '\\' => return '\\',
            't' => return '\t',
            '\'' => return '\'',
            '"' => return '"',
            '0'...'7' => {
                const octstr = slice[offset + 1 .. offset + 4];
                assert(octstr.len == 3);
                const oct = try std.fmt.parseUnsigned(u8, octstr, 8);
                skiplen += 2;
                return oct;
            },
            else => {
                p.errFmt("invalid escape '{c}'", .{slice[offset + 1]});
                return error.InvalidEscape;
            },
        }
    }

    /// Consumes `p.content` until `end`. asserts that `p.content` starts
    /// with 'start'.  If behavior == .escape, escape sequences are parsed
    /// and converted in-place in the returned slice.
    pub fn parseBetween(p: *Parser, start: u8, end: u8, comptime behavior: enum { escape, dont_escape }) ![]u8 {
        assert(p.content.len > 0 and p.content[0] == start);
        var fbs = std.io.fixedBufferStream(p.content);
        const writer = fbs.writer();

        var index: usize = 1;
        while (true) {
            const b = p.content[index];
            if (b == end) {
                defer p.content = p.content[index + 1 ..];
                return p.content[0..fbs.pos];
            }
            if (behavior == .escape)
                switch (b) {
                    '\\' => try writer.writeByte(try p.parseEscapeSequence(p.content, &index)),
                    '\n' => return error.InvalidStringLiteral,
                    else => {
                        try writer.writeByte(b);
                        index += 1;
                    },
                }
            else {
                try writer.writeByte(b);
                index += 1;
            }
        }
    }

    /// like `parseBetween` but skips over nested `start` ... `end` content.
    /// this allows actions delimited by '{' ... '}' to contain  '{' ... '}'
    fn parseBetweenWithNesting(p: *Parser, start: u8, end: u8) ![]u8 {
        assert(p.content.len > 0 and p.content[0] == start);
        var fbs = std.io.fixedBufferStream(p.content);
        const writer = fbs.writer();

        var index: usize = 1;
        var depth: usize = 1;
        while (true) {
            const b = p.content[index];
            const endmatch = b == end;
            depth -= @boolToInt(endmatch);
            if (endmatch and depth == 0) {
                defer p.content = p.content[index + 1 ..];
                return p.content[0..fbs.pos];
            }
            try writer.writeByte(b);
            depth += @boolToInt(b == start);
            index += 1;
        }
    }

    /// Linear search for the index of a scalar value inside a slice.
    pub fn indexOfScalarFn(comptime T: type, slice: []const T, func: ItemParser(T)) ?usize {
        return indexOfScalarPosFn(T, slice, 0, func);
    }
    pub fn indexOfScalarPosFn(comptime T: type, slice: []const T, start_index: usize, func: ItemParser(T)) ?usize {
        var i: usize = start_index;
        while (i < slice.len) : (i += 1) {
            if (func(slice[i])) return i;
        }
        return null;
    }

    pub fn readUntilCharFn(p: *Parser, comptime charp: ItemParser(u8), skiplen: u32) ![]u8 {
        const end = indexOfScalarFn(u8, p.content[skiplen..], charp) orelse p.content.len;
        defer p.content = p.content[end + skiplen ..];
        return p.content[skiplen .. skiplen + end];
    }

    /// ---
    /// These are ordered into groups
    /// ---
    pub const Token = union(enum) {
        end,
        bar,
        rparen,
        // end seq terminators
        not,
        amp,
        // end prefix mods
        many,
        some,
        opt,
        // end suffix mods
        leftarrow,
        // start atoms
        dot,
        lparen,
        action: []const u8,
        dstr: []const u8,
        sstr: []const u8,
        char_set: []const u8,
        sym: []const u8,

        pub const Tag = std.meta.Tag(Token);

        const first_atom = @enumToInt(Token.dot);
        pub inline fn isAtom(t: Token) bool {
            return @enumToInt(t) >= first_atom;
        }

        const last_seq_terminator = @enumToInt(Token.rparen);
        pub inline fn isEndSeq(t: Token) bool {
            return @enumToInt(t) <= last_seq_terminator;
        }

        pub fn format(t: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            // _ = try writer.write(@tagName(t));
            switch (t) {
                .sym => |s| _ = try writer.write(s),
                .sstr => |s| try writer.print("'{s}'", .{s}),
                .dstr => |s| try writer.print("\"{s}\"", .{s}),
                .char_set => |s| try writer.print("[{s}]", .{s}),
                .action => |s| try writer.print("{{ {s} }}", .{s}),
                .many => try writer.writeByte('*'),
                .some => try writer.writeByte('+'),
                .opt => try writer.writeByte('?'),
                .not => try writer.writeByte('!'),
                .dot => try writer.writeByte('.'),
                .bar => try writer.writeByte('/'),
                .end => _ = try writer.write("EOF"),
                .leftarrow => _ = try writer.write("<-"),
                .lparen => try writer.writeByte('('),
                .rparen => try writer.writeByte(')'),
                .amp => try writer.writeByte('&'),
            }
        }
    };

    inline fn skipYield(p: *Parser, n: u32, t: Token) Token {
        p.content = p.content[n..];
        return t;
    }

    fn matchSkipContent(p: *Parser, s: []const u8) bool {
        const match = mem.startsWith(u8, p.content, s);
        p.content = p.content[s.len * @boolToInt(match) ..];
        return match;
    }

    fn nextToken(p: *Parser) !Token {
        p.skipWs();
        if (p.content.len == 0) return .end;
        const c = p.content[0];
        return switch (c) {
            '"' => .{ .dstr = try p.parseBetween('"', '"', .escape) },
            '\'' => .{ .sstr = try p.parseBetween('\'', '\'', .escape) },
            '[' => .{ .char_set = try p.parseBetween('[', ']', .escape) },
            '/' => p.skipYield(1, .bar),
            '!' => p.skipYield(1, .not),
            '&' => p.skipYield(1, .amp),
            '?' => p.skipYield(1, .opt),
            '*' => p.skipYield(1, .many),
            '+' => p.skipYield(1, .some),
            '(' => p.skipYield(1, .lparen),
            ')' => p.skipYield(1, .rparen),
            '{' => .{ .action = try p.parseBetweenWithNesting('{', '}') },
            '}' => unreachable,
            '.' => p.skipYield(1, .dot),
            else => if (p.matchSkipContent("<-"))
                .leftarrow
            else if (std.ascii.isAlphabetic(c)) .{ .sym = try p.readUntilCharFn(
                not(u8, oneOf(u8, std.ascii.isAlphanumeric, isItem(u8, '_'))),
                0,
            ) } else {
                p.errFmt("internal error: nextToken() unhandled '{c}'", .{c});
                panicf("internal error: nextToken() unhandled '{c}'", .{c});
            },
        };
    }

    pub fn next(p: *Parser) Token {
        if (p.peeks.len > 0) {
            return p.peeks.orderedRemove(0);
        } else return p.nextToken() catch |e|
            panicf("internal error in next(): {s}", .{@errorName(e)});
    }

    pub fn peek(p: *Parser, idx: u2) Token {
        if (idx >= p.peeks.buffer.len)
            panicf("internal error in peek() index '{}' out of range", .{idx});
        return if (idx < p.peeks.len) p.peeks.get(idx) else blk: {
            var i = @intCast(u2, p.peeks.len);
            while (i <= idx) : (i += 1) {
                const t = p.nextToken() catch |e|
                    panicf("internal error in peek(): {s}", .{@errorName(e)});
                p.peeks.append(t) catch unreachable;
                if (i == idx) break :blk p.peeks.buffer[i];
            }
        };
    }

    fn expectToken(p: *Parser, tag: Token.Tag) !Token {
        const t = p.next();
        if (t != tag) {
            p.errFmt("unexpected token {s}: '{}'", .{ @tagName(t), t });
            return error.UnexpectedToken;
        }
        return t;
    }

    fn consume(p: *Parser, tag: Token.Tag) bool {
        const istag = p.peek(0) == tag;
        if (istag) _ = p.next();
        return istag;
    }

    /// parse a raw charset literal into a `CharSet`
    pub fn parseCharSet(bytes_: []const u8) !CharSet {
        var bytes = bytes_;
        var set: CharSet = .{};
        set.negated = bytes[0] == '^';
        bytes = bytes[@boolToInt(set.negated)..];
        var chars_start: usize = 0;
        for (bytes) |c, i| {
            if (i + 2 < bytes.len and bytes[i + 1] == '-') {
                // found range
                if (i > chars_start)
                    try set.sets.append(.{ .chars = bytes[chars_start..i] });
                chars_start = i + 3;
                if (c >= bytes[i + 2]) return error.InvalidCharSetRange;
                const range: CharSet.Set = .{ .range = .{ c, bytes[i + 2] } };
                try set.sets.append(range);
            }
        }
        if (chars_start < bytes.len)
            try set.sets.append(.{ .chars = bytes[chars_start..] });
        // TODO maybe check for duplicate chars?
        return set;
    }

    // Primary <- Identifier !LEFTARROW
    //            / OPEN Expression CLOSE
    //            / Literal
    //            / Class
    //            / DOT
    //            / Action
    //            / BEGIN
    //            / END
    fn parsePrimary(p: *Parser) Error!Node {
        println("parsePrimary() peeks={any}", .{p.peeks.slice()});
        return switch (p.next()) {
            .sym => |t| blk: {
                assert(p.peek(0) != .leftarrow);
                break :blk Node.init(.sym, t);
            },
            .lparen => blk: {
                var node = try p.alloc.create(Node);
                node.* = try p.parseExpr();
                _ = try p.expectToken(.rparen);
                break :blk Node.init(.group, node);
            },
            .dstr => |t| Node.init(.dstr, t),
            .sstr => |t| Node.init(.sstr, t),
            .char_set => |t| Node.init(.char_set, t),
            .dot => Node.init(.dot, {}),
            .action => |t| Node.init(.action, t),
            else => |t| {
                p.errFmt("unexpected token: '{}'", .{t});
                return error.UnexpectedToken;
            },
        };
    }

    // Suffix <- Primary ( QUERY / STAR / PLUS )?
    fn parseSuffix(p: *Parser) Error!Node {
        var node = try p.parsePrimary();
        if (p.consume(.opt))
            node.flags().insert(.opt)
        else if (p.consume(.many))
            node.flags().insert(.many)
        else if (p.consume(.some))
            node.flags().insert(.some);
        return node;
    }

    // Action <- '{' < [^}]* > '}' Spacing
    // Prefix <- AND Action
    //           / ( AND | NOT )? Suffix
    fn parsePrefix(p: *Parser) Error!Node {
        println("parsePrefix() peeks {any}", .{p.peeks.slice()});
        if (p.peek(0) == .amp and p.peek(1) == .action) {
            _ = p.next();
            var node = Node.init(.action, p.next().action);
            node.flags().insert(.amp);
            return node;
        }
        const isand = p.consume(.amp);
        const isnot = p.consume(.not);
        var node = try p.parseSuffix();
        if (isnot) node.flags().insert(.not);
        if (isand) node.flags().insert(.amp);
        return node;
    }

    inline fn isSeqEnd(p: *Parser) bool {
        return p.peek(0).isEndSeq() or p.peek(1) == .leftarrow;
    }

    // Sequence <- Prefix*
    fn parseSeq(p: *Parser) Error!Node {
        var node = try p.parsePrefix();
        println("parseSeq() 1 peeks {any} node={}", .{ p.peeks.slice(), node });

        if (p.isSeqEnd()) return node;
        { // turn node into a seq
            const copy = node;
            node = Node.init(.seq, .{});
            try node.seq.payload.append(p.alloc, copy);
        }

        while (true) {
            const node2 = try p.parsePrefix();
            println("parseSeq() 2 node2={}", .{node2});
            try node.seq.payload.append(p.alloc, node2);
            if (p.isSeqEnd()) break;
        }
        return node;
    }

    inline fn isExprEnd(p: *Parser) bool {
        return !p.consume(.bar) or p.peek(1) == .leftarrow or p.peek(0) == .end;
    }

    // Expression <- Sequence ( SLASH Sequence )*
    fn parseExpr(p: *Parser) Error!Node {
        var node = try p.parseSeq();
        println("parseExpr() 1 node={} peeks={any}", .{ node, p.peeks.slice() });

        if (p.isExprEnd()) return node;
        { // turn node into an alt
            const copy = node;
            node = Node.init(.alt, .{});
            try node.alt.payload.append(p.alloc, copy);
        }

        while (true) {
            var node2 = try p.parseSeq();
            try node.alt.payload.append(p.alloc, node2);
            println("parseExpr() 2 node2={}", .{node2});
            if (p.isExprEnd()) break;
        }
        return node;
    }

    fn debugContent(p: *Parser) void {
        print("content:", .{});
        for (p.peeks.slice()) |pk, i| {
            if (i != 0) print(" ", .{});
            print("{}", .{pk});
        }
        print("{s}...", .{firstN(p.content, 40)});
    }

    // Grammar <- Spacing Definition+ EndOfFile
    // Definition <- Identifier LEFTARROW Expression
    pub fn parse(p: *Parser) !Grammar {
        while (p.peek(0) != .end) {
            p.debugContent();
            const identifier = (try p.expectToken(.sym)).sym;
            _ = try p.expectToken(.leftarrow);
            const expr = try p.parseExpr();

            println("parse() def={s} <- {}\n", .{ identifier, expr });
            if (p.grammar.rules.contains(identifier)) {
                p.errFmt("duplicate rule: '{s}'", .{identifier});
                return error.DuplicateRule;
            }
            try p.grammar.rules.putNoClobber(p.alloc, identifier, expr);
        }

        return p.grammar;
    }
};
