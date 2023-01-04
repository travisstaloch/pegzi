const std = @import("std");

pub fn Gen(comptime Grammar: type, comptime Node: type) type {
    return struct {
        pub fn gen(g: Grammar, start_sym: []const u8, writer: anytype, errwriter: anytype, rand: std.rand.Random) !void {
            var rule = g.rules.get(start_sym) orelse return error.InvalidStartSymbol;
            try genImpl(g, rule, writer, errwriter, rand);
        }

        fn genImpl(g: Grammar, rule: Node, writer: anytype, errwriter: anytype, rand: std.rand.Random) !void {
            switch (rule) {
                .sym => |s| {
                    const newrule = g.rules.get(s.payload) orelse {
                        try errwriter.print("missing rule '{s}'\n", .{s.payload});
                        return error.MissingRule;
                    };
                    try @call(.always_tail, genImpl, .{ g, newrule, writer, errwriter, rand });
                },
                .dstr, .sstr => |s| _ = try writer.write(s.payload),
                .seq => |s| for (s.payload.items) |n|
                    try genImpl(g, n, writer, errwriter, rand),
                .alt => |s| {
                    const i = rand.uintLessThan(u32, @intCast(u32, s.payload.items.len));
                    try @call(.always_tail, genImpl, .{ g, s.payload.items[i], writer, errwriter, rand });
                },
                .group => |group| try @call(.always_tail, genImpl, .{ g, group.payload.*, writer, errwriter, rand }),
                .dot => @panic("TODO dot"),
                .char_set => @panic("TODO char_set"),
            }
        }
    };
}
