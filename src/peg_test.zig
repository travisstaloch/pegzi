const std = @import("std");
const testing = std.testing;
const peg = @import("peg.zig");
const Parser = peg.Parser;
const Grammar = peg.Grammar;

const test_filepath = "<test>";
inline fn testParserInit(s: anytype) Parser {
    var arr = s.*;
    std.log.debug("\ntestParserInit:\n{s}", .{&arr});
    var p: Parser = .{
        .content = &arr,
        .content_start = &arr,
        .args = .{ .filepath = test_filepath },
        .alloc = talloc,
    };
    return p;
}

fn testEscape(input: anytype, expected: []const u8) !void {
    var p = testParserInit(input);
    defer p.deinit();
    const actual = try p.parseBetween('[', ']', .escape);
    // std.log.debug("expected 0x{} '{s}'", .{ std.fmt.fmtSliceHexLower(expected), expected });
    // std.log.debug("actual   0x{} '{s}'", .{ std.fmt.fmtSliceHexLower(actual), actual });
    try testing.expectEqualStrings(expected, actual);
}

const log_level = .err;

test "parse escapes 1" {
    testing.log_level = log_level;
    try testEscape(
        \\[\\]
    , "\\");
    try testEscape(
        \\[\200-\277]
    , "\x80-\xbf");
    try testEscape(
        \\[\364]
    , "\xf4");
    try testEscape(
        \\[\200-\217]
    , "\x80-\x8f");
    try testEscape(
        \\[\000-\011\013-\046-\050-\133\135-\177]
    , "\x00-\x09\x0b-\x26-\x28-\x5b\x5d-\x7f");
}

const talloc = testing.allocator;

inline fn getGrammar(in: anytype) !Grammar {
    var arr = in.*;
    std.log.debug("\n-- rules:\n{s}\n---", .{&arr});
    var p = Parser.init(talloc, &arr, .{ .filepath = test_filepath });
    return try p.parse();
}

test "parse flags" {
    testing.log_level = log_level;

    var g = try getGrammar(
        \\a <- !"a"?
    );
    defer g.deinit(talloc);
    const dstr = g.rules.values()[0];
    try testing.expect(dstr == .dstr);
    try testing.expect(dstr.flagsConst().contains(.not));
    try testing.expect(dstr.flagsConst().contains(.opt));
}

test "parse flags extra suffix" {
    var arr = "a <- !\"a\"?*".*;
    var p = Parser.init(talloc, &arr, .{ .filepath = test_filepath });
    defer p.deinit();
    try testing.expectError(error.UnexpectedToken, p.parse());
}

test "parse flags extra prefix" {
    var arr = "a <- !&\"a\"?".*;
    var p = Parser.init(talloc, &arr, .{ .filepath = test_filepath });
    defer p.deinit();
    try testing.expectError(error.UnexpectedToken, p.parse());
}

test "parse basic 1" {
    testing.log_level = log_level;
    {
        var g = try getGrammar(
            \\a <- b c d
        );
        defer g.deinit(talloc);
        const seq = g.rules.values()[0];
        try testing.expect(seq == .seq);
        try testing.expectEqual(@as(usize, 3), seq.seq.payload.items.len);
        for (seq.seq.payload.items) |it|
            try testing.expect(it == .sym);
    }
    {
        var g = try getGrammar(
            \\a <- b c / d
        );
        defer g.deinit(talloc);
        const ch = g.rules.values()[0];
        try testing.expect(ch == .alt);
        try testing.expectEqual(@as(usize, 2), ch.alt.payload.items.len);
        const alts = ch.alt.payload.items;
        try testing.expect(alts[0] == .seq);
        try testing.expectEqual(@as(usize, 2), alts[0].seq.payload.items.len);
        try testing.expect(alts[1] == .sym);
    }
}
test "parse basic 2" {
    testing.log_level = log_level;
    {
        var g = try getGrammar(
            \\a <- (b c)
        );
        defer g.deinit(talloc);
        const root = g.rules.values()[0];
        try testing.expect(root == .group);
        const gp = root.group.payload.*;
        try testing.expect(gp == .seq);
        const items = root.group.payload.seq.payload.items;
        try testing.expectEqual(@as(usize, 2), items.len);
        try testing.expect(items[0] == .sym);
        try testing.expect(items[1] == .sym);
    }
    {
        var g = try getGrammar(
            \\a <- (b / c)
        );
        defer g.deinit(talloc);
        const root = g.rules.values()[0];
        try testing.expect(root == .group);
        try testing.expect(root.group.payload.* == .alt);
        const items = root.group.payload.alt.payload.items;
        try testing.expectEqual(@as(usize, 2), items.len);
        try testing.expect(items[0] == .sym);
        try testing.expect(items[1] == .sym);
    }
    {
        var g = try getGrammar(
            \\a <- b (c d)* (e / f)
        );
        defer g.deinit(talloc);
        const root = g.rules.values()[0];
        try testing.expect(root == .seq);
        try testing.expectEqual(@as(usize, 3), root.seq.payload.items.len);
        try testing.expect(root.seq.payload.items[0] == .sym);
        try testing.expect(root.seq.payload.items[1] == .group);
        try testing.expect(root.seq.payload.items[1].group.flags.contains(.many));
        try testing.expect(root.seq.payload.items[1].group.payload.* == .seq);
        const g1seq = root.seq.payload.items[1].group.payload.seq;
        try testing.expectEqual(@as(usize, 2), g1seq.payload.items.len);
        try testing.expectEqual(@as(usize, 3), root.seq.payload.items.len);
        try testing.expect(root.seq.payload.items[2] == .group);
        const g2 = root.seq.payload.items[2].group;
        try testing.expect(g2.payload.* == .alt);
        try testing.expectEqual(@as(usize, 2), g2.payload.alt.payload.items.len);
    }
}

test "peek 1" {
    testing.log_level = log_level;
    var p = testParserInit(
        \\a <- b c
        \\d <- e / f
    );
    defer p.deinit();
    try testing.expectEqualStrings("a", (p.peek(0)).sym);
    try testing.expectEqual(Parser.Token.leftarrow, p.peek(1));
    try testing.expectEqualStrings("a", (p.peek(0)).sym);
    try testing.expectEqualStrings("a", (p.next()).sym);
    try testing.expectEqual(Parser.Token.leftarrow, p.peek(0));
    try testing.expectEqualStrings("b", (p.peek(1)).sym);
    try testing.expect(p.next() == .leftarrow);
    try testing.expect(p.peek(3) == .leftarrow);
    try testing.expectEqualStrings("b", p.next().sym);
    try testing.expectEqualStrings("c", p.next().sym);
    try testing.expect(p.peek(1) == .leftarrow);
    try testing.expectEqualStrings("d", p.next().sym);
    try testing.expect(p.next() == .leftarrow);
    try testing.expectEqualStrings("e", p.next().sym);
    try testing.expect(p.next() == .bar);
    try testing.expectEqualStrings("f", p.next().sym);
    try testing.expect(p.next() == .eof);
}

test "comments" {
    testing.log_level = log_level;
    const in =
        \\# comment
        \\a <- b c? d e
        \\# comment
        \\# comment
        \\
        \\# comment
        \\
        \\f <- g (h i)* 
        \\# comment
        \\(j / k)
        \\
        \\# comment
    ;
    var p = testParserInit(in);
    defer p.deinit();
    // testing.log_level = .debug;
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 2), g.rules.count());
    try testing.expectEqual(@as(usize, 3), g.rules.values()[1].seq.payload.items.len);
}

test "trailing bar 1" {
    testing.log_level = log_level;
    const in =
        \\a  <- b c
        \\     /
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 1), g.rules.count());
    const first = g.rules.values()[0];
    try testing.expect(first == .seq);
    try testing.expectEqual(@as(usize, 2), first.seq.payload.items.len);
}

test "trailing bar 2" {
    testing.log_level = log_level;
    const in =
        \\a  <- b c
        \\     /
        \\d <- e
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 2), g.rules.count());
    const first = g.rules.values()[0];
    try testing.expect(first == .seq);
    try testing.expectEqual(@as(usize, 2), first.seq.payload.items.len);
}

test "trailing bar 3" {
    testing.log_level = log_level;
    const in =
        \\a  <- b c
        \\     / d e
        \\     /
        \\
        \\f <- g
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 2), g.rules.count());
    const first = g.rules.values()[0];
    try testing.expect(first == .alt);
    try testing.expectEqual(@as(usize, 2), first.alt.payload.items.len);
}

test "trailing bar 4" {
    testing.log_level = log_level;
    const in =
        \\a  <- b c
        \\     / d e
        \\     /
        \\
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 1), g.rules.count());
    const first = g.rules.values()[0];
    try testing.expect(first == .alt);
    try testing.expectEqual(@as(usize, 2), first.alt.payload.items.len);
}

test "two groups" {
    testing.log_level = log_level;
    const in =
        \\n   <- (a / b)? c (d / e)
        \\
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    const first = g.rules.values()[0];
    try testing.expect(first == .seq);
    try testing.expectEqual(@as(usize, 3), first.seq.payload.items.len);
    try testing.expect(first.seq.payload.items[0] == .group);
    try testing.expect(first.seq.payload.items[0].group.flags.contains(.opt));
    try testing.expect(first.seq.payload.items[2] == .group);
    try testing.expect(first.seq.payload.items[2].group.flags.count() == 0);
}

test "two defs" {
    testing.log_level = log_level;
    const in =
        \\a <- b / c
        \\g <- h
        \\
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    try testing.expectEqual(@as(usize, 2), g.rules.count());
}

test "bug repro: printing seq last expr prints same flags as first" {
    // this was caused by usage of now deleted Node.writePayload() method
    // which was copying flags from the first child to the containing seq/alt.
    const in =
        \\a<-b+ c
        \\d<-e+ / f
        \\
    ;
    var p = testParserInit(in);
    defer p.deinit();
    const g = try p.parse();
    {
        const root = g.rules.values()[0];
        try testing.expect(root == .seq);
        try testing.expect(root.seq.flags.count() == 0);
        const items = root.seq.payload.items;
        try testing.expectEqual(@as(usize, 2), items.len);
        try testing.expect(items[0].sym.flags.count() == 1);
        try testing.expect(items[0].sym.flags.contains(.some));
        try testing.expect(items[1].sym.flags.count() == 0);
        const printed = try std.fmt.allocPrint(talloc, "{}", .{root});
        defer talloc.free(printed);
        try testing.expectEqualStrings("b+ c", printed);
    }
    {
        const root = g.rules.values()[1];
        try testing.expect(root == .alt);
        try testing.expect(root.alt.flags.count() == 0);
        const items = root.alt.payload.items;
        try testing.expectEqual(@as(usize, 2), items.len);
        try testing.expect(items[0].sym.flags.count() == 1);
        try testing.expect(items[0].sym.flags.contains(.some));
        try testing.expect(items[1].sym.flags.count() == 0);
        const printed = try std.fmt.allocPrint(talloc, "{}", .{root});
        defer talloc.free(printed);
        try testing.expectEqualStrings("e+ / f", printed);
    }
}

const parseCharSet = peg.Parser.parseCharSet;
test "parse char set" {
    {
        const set = try parseCharSet("^\n");
        try testing.expect(set.negated);
        try testing.expect(set.sets.len == 1);
        try testing.expect(set.sets.buffer[0] == .chars);
        try testing.expect(set.sets.buffer[0].chars.len == 1);
        try testing.expect(set.sets.buffer[0].chars[0] == '\n');
    }
    {
        const set = try parseCharSet("^0-9");
        try testing.expect(set.negated);
        try testing.expect(set.sets.len == 1);
        try testing.expect(set.sets.buffer[0] == .range);
        try testing.expect(set.sets.buffer[0].range[0] == '0');
        try testing.expect(set.sets.buffer[0].range[1] == '9');
    }
    {
        const set = try parseCharSet("^0-9a-z");
        try testing.expect(set.negated);
        try testing.expect(set.sets.len == 2);
        try testing.expect(set.sets.buffer[0] == .range);
        try testing.expect(set.sets.buffer[0].range[0] == '0');
        try testing.expect(set.sets.buffer[0].range[1] == '9');
        try testing.expect(set.sets.buffer[1] == .range);
        try testing.expect(set.sets.buffer[1].range[0] == 'a');
        try testing.expect(set.sets.buffer[1].range[1] == 'z');
    }
    {
        const set = try parseCharSet("ABC0-9DEFa-zGHI");
        try testing.expect(set.sets.len == 5);
        try testing.expect(set.sets.buffer[0] == .chars);
        try testing.expect(set.sets.buffer[0].chars.len == 3);
        try testing.expectEqualStrings("ABC", set.sets.buffer[0].chars);
        try testing.expect(set.sets.buffer[1] == .range);
        try testing.expect(set.sets.buffer[1].range[0] == '0');
        try testing.expect(set.sets.buffer[1].range[1] == '9');
        try testing.expect(set.sets.buffer[2] == .chars);
        try testing.expect(set.sets.buffer[2].chars.len == 3);
        try testing.expectEqualStrings("DEF", set.sets.buffer[2].chars);
        try testing.expect(set.sets.buffer[3] == .range);
        try testing.expect(set.sets.buffer[3].range[0] == 'a');
        try testing.expect(set.sets.buffer[3].range[1] == 'z');
        try testing.expect(set.sets.buffer[4] == .chars);
        try testing.expect(set.sets.buffer[4].chars.len == 3);
        try testing.expectEqualStrings("GHI", set.sets.buffer[4].chars);
    }
}

test "negations" {
    {
        const in =
            \\a<- ![a-z] b
            \\
        ;
        var p = testParserInit(in);
        defer p.deinit();
        const g = try p.parse();
        const root = g.rules.values()[0];
        try testing.expect(root == .seq);
        try testing.expect(root.seq.flags.count() == 0);
        const items = root.seq.payload.items;
        try testing.expectEqual(@as(usize, 2), items.len);
        try testing.expect(items[0] == .char_set);
        try testing.expect(items[0].char_set.flags.contains(.not));
    }
}

test "actions" {
    {
        const in =
            \\a<- &{ code here with nested curlies { { {} } } }
            \\
        ;
        var p = testParserInit(in);
        defer p.deinit();
        const g = try p.parse();
        const root = g.rules.values()[0];
        try testing.expect(root == .action);
        try testing.expect(root.action.flags.contains(.amp));
    }
}

test "begin / end" {
    {
        const in =
            \\a <- < b c* > d
            \\
        ;
        var p = testParserInit(in);
        defer p.deinit();
        const g = try p.parse();
        const root = g.rules.values()[0];
        try testing.expect(root == .seq);
        try testing.expect(root.seq.payload.items.len == 5);
        try testing.expect(root.seq.payload.items[0] == .begin);
        try testing.expect(root.seq.payload.items[3] == .end);
    }
}
