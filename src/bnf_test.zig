const std = @import("std");
const testing = std.testing;
const bnf = @import("bnf.zig");
const Parser = bnf.Parser;
const Grammar = bnf.Grammar;

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

const talloc = testing.allocator;
const log_level = .debug;

inline fn getGrammar(in: anytype) !Grammar {
    var arr = in.*;
    std.log.debug("\n-- rules:\n{s}\n---", .{&arr});
    var p = Parser.init(talloc, &arr, .{ .filepath = test_filepath });
    return try p.parse();
}

test "simple def" {
    testing.log_level = log_level;
    {
        var g = try getGrammar(
            \\<a> ::= "a"
        );
        defer g.deinit(talloc);
        const root = g.rules.values()[0];
        std.debug.print("{} {s}\n", .{ root, @tagName(root) });
        try testing.expect(root == .alt);
        try testing.expect(root.alt.items.len == 1);
        try testing.expect(root.alt.items[0] == .seq);
        try testing.expect(root.alt.items[0].seq.items.len == 1);
        try testing.expect(root.alt.items[0].seq.items[0] == .dstr);
    }
}
// test "parse basic 1" {
//     testing.log_level = log_level;
//     {
//         var g = try getGrammar(
//             \\a ::= b c d
//         );
//         defer g.deinit(talloc);
//         const seq = g.rules.values()[0];
//         try testing.expect(seq == .seq);
//         try testing.expectEqual(@as(usize, 3), seq.seq.payload.items.len);
//         for (seq.seq.payload.items) |it|
//             try testing.expect(it == .sym);
//     }
//     {
//         var g = try getGrammar(
//             \\a ::= b c / d
//         );
//         defer g.deinit(talloc);
//         const ch = g.rules.values()[0];
//         try testing.expect(ch == .alt);
//         try testing.expectEqual(@as(usize, 2), ch.alt.payload.items.len);
//         const alts = ch.alt.payload.items;
//         try testing.expect(alts[0] == .seq);
//         try testing.expectEqual(@as(usize, 2), alts[0].seq.payload.items.len);
//         try testing.expect(alts[1] == .sym);
//     }
// }
// test "parse basic 2" {
//     testing.log_level = log_level;
//     {
//         var g = try getGrammar(
//             \\a ::= (b c)
//         );
//         defer g.deinit(talloc);
//         const root = g.rules.values()[0];
//         try testing.expect(root == .group);
//         const gp = root.group.payload.*;
//         try testing.expect(gp == .seq);
//         const items = root.group.payload.seq.payload.items;
//         try testing.expectEqual(@as(usize, 2), items.len);
//         try testing.expect(items[0] == .sym);
//         try testing.expect(items[1] == .sym);
//     }
//     {
//         var g = try getGrammar(
//             \\a ::= (b / c)
//         );
//         defer g.deinit(talloc);
//         const root = g.rules.values()[0];
//         try testing.expect(root == .group);
//         try testing.expect(root.group.payload.* == .alt);
//         const items = root.group.payload.alt.payload.items;
//         try testing.expectEqual(@as(usize, 2), items.len);
//         try testing.expect(items[0] == .sym);
//         try testing.expect(items[1] == .sym);
//     }
//     {
//         var g = try getGrammar(
//             \\a ::= b (c d)* (e / f)
//         );
//         defer g.deinit(talloc);
//         const root = g.rules.values()[0];
//         try testing.expect(root == .seq);
//         try testing.expectEqual(@as(usize, 3), root.seq.payload.items.len);
//         try testing.expect(root.seq.payload.items[0] == .sym);
//         try testing.expect(root.seq.payload.items[1] == .group);
//         try testing.expect(root.seq.payload.items[1].group.flags.contains(.many));
//         try testing.expect(root.seq.payload.items[1].group.payload.* == .seq);
//         const g1seq = root.seq.payload.items[1].group.payload.seq;
//         try testing.expectEqual(@as(usize, 2), g1seq.payload.items.len);
//         try testing.expectEqual(@as(usize, 3), root.seq.payload.items.len);
//         try testing.expect(root.seq.payload.items[2] == .group);
//         const g2 = root.seq.payload.items[2].group;
//         try testing.expect(g2.payload.* == .alt);
//         try testing.expectEqual(@as(usize, 2), g2.payload.alt.payload.items.len);
//     }
// }

// test "peek 1" {
//     testing.log_level = log_level;
//     var p = testParserInit(
//         \\a ::= b c
//         \\d ::= e / f
//     );
//     defer p.deinit();
//     try testing.expectEqualStrings("a", (p.peek(0)).sym);
//     try testing.expectEqual(Parser.Token.leftarrow, p.peek(1));
//     try testing.expectEqualStrings("a", (p.peek(0)).sym);
//     try testing.expectEqualStrings("a", (p.next()).sym);
//     try testing.expectEqual(Parser.Token.leftarrow, p.peek(0));
//     try testing.expectEqualStrings("b", (p.peek(1)).sym);
//     try testing.expect(p.next() == .leftarrow);
//     try testing.expect(p.peek(3) == .leftarrow);
//     try testing.expectEqualStrings("b", p.next().sym);
//     try testing.expectEqualStrings("c", p.next().sym);
//     try testing.expect(p.peek(1) == .leftarrow);
//     try testing.expectEqualStrings("d", p.next().sym);
//     try testing.expect(p.next() == .leftarrow);
//     try testing.expectEqualStrings("e", p.next().sym);
//     try testing.expect(p.next() == .bar);
//     try testing.expectEqualStrings("f", p.next().sym);
//     try testing.expect(p.next() == .end);
// }

// test "comments" {
//     testing.log_level = log_level;
//     const in =
//         \\# comment
//         \\a ::= b c? d e
//         \\# comment
//         \\# comment
//         \\
//         \\# comment
//         \\
//         \\f ::= g (h i)*
//         \\# comment
//         \\(j / k)
//         \\
//         \\# comment
//     ;
//     var p = testParserInit(in);
//     defer p.deinit();
//     // testing.log_level = .debug;
//     const g = try p.parse();
//     try testing.expectEqual(@as(usize, 2), g.rules.count());
//     try testing.expectEqual(@as(usize, 3), g.rules.values()[1].seq.payload.items.len);
// }

// test "trailing bar" {
//     testing.log_level = log_level;
//     const in =
//         \\a  ::= b c
//         \\     / d e
//         \\     /
//         \\
//         \\f ::= g (h / i)? j
//     ;
//     var p = testParserInit(in);
//     defer p.deinit();
//     const g = try p.parse();
//     try testing.expectEqual(@as(usize, 2), g.rules.count());
//     const first = g.rules.values()[0];
//     try testing.expect(first == .alt);
//     try testing.expectEqual(@as(usize, 2), first.alt.payload.items.len);
// }

// test "two groups" {
//     testing.log_level = log_level;
//     const in =
//         \\n   ::= (a / b)? c (d / e)
//         \\
//     ;
//     var p = testParserInit(in);
//     defer p.deinit();
//     const g = try p.parse();
//     const first = g.rules.values()[0];
//     try testing.expect(first == .seq);
//     try testing.expectEqual(@as(usize, 3), first.seq.payload.items.len);
//     try testing.expect(first.seq.payload.items[0] == .group);
//     try testing.expect(first.seq.payload.items[0].group.flags.contains(.opt));
//     try testing.expect(first.seq.payload.items[2] == .group);
//     try testing.expect(first.seq.payload.items[2].group.flags.count() == 0);
// }

// test "two defs" {
//     testing.log_level = log_level;
//     const in =
//         \\a ::= b / c
//         \\g ::= h
//         \\
//     ;
//     var p = testParserInit(in);
//     defer p.deinit();
//     const g = try p.parse();
//     try testing.expectEqual(@as(usize, 2), g.rules.count());
// }

// // TODO
// test "bug: printing seq last expr prints same flags as first" {
//     const in =
//         \\a<-b?+ c
//         \\d<-e?+ / f
//         \\
//     ;
//     var p = testParserInit(in);
//     defer p.deinit();
//     const g = try p.parse();
//     {
//         const root = g.rules.values()[0];
//         try testing.expect(root == .seq);
//         const items = root.seq.payload.items;
//         try testing.expectEqual(@as(usize, 2), items.len);
//         try testing.expect(items[0].sym.flags.count() == 2);
//         try testing.expect(items[0].sym.flags.contains(.opt));
//         try testing.expect(items[0].sym.flags.contains(.some));
//         try testing.expect(items[1].sym.flags.count() == 0);
//         const printed = try std.fmt.allocPrint(talloc, "{}", .{root});
//         defer talloc.free(printed);
//         if (true) return error.SkipZigTest; // TODO
//         try testing.expectEqualStrings("b?+ c", printed);
//     }
//     {
//         const root = g.rules.values()[1];
//         try testing.expect(root == .alt);
//         const items = root.alt.payload.items;
//         try testing.expectEqual(@as(usize, 2), items.len);
//         try testing.expect(items[0].sym.flags.count() == 2);
//         try testing.expect(items[0].sym.flags.contains(.opt));
//         try testing.expect(items[0].sym.flags.contains(.some));
//         try testing.expect(items[1].sym.flags.count() == 0);
//         const printed = try std.fmt.allocPrint(talloc, "{}", .{root});
//         defer talloc.free(printed);
//         if (true) return error.SkipZigTest; // TODO
//         try testing.expectEqualStrings("e?+ / f", printed);
//     }
// }
