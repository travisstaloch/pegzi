const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const peg = @import("peg.zig");
const gen = @import("gen.zig");
const Grammar = peg.Grammar;
const Parser = peg.Parser;
const Node = peg.Node;
const Args = peg.Args;

pub const log_level: std.log.Level = .debug;

fn usage(exepath: []const u8) void {
    std.debug.print("usage: {s} <peg_filepath> <start_symbol>\n", .{exepath});
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = arena.allocator();

    var procargs = try std.process.argsAlloc(alloc);
    if (procargs.len < 3) return usage(procargs[0]);
    var args: Args = .{
        .filepath = procargs[1],
        .start_sym = procargs[2],
    };

    const f = try std.fs.cwd().openFile(args.filepath, .{});
    defer f.close();

    const content = try f.readToEndAlloc(alloc, std.math.maxInt(u32));
    var p = Parser.init(alloc, content, args);
    const grammar = try p.parse();
    std.debug.print("{}\n", .{grammar});
    const writer = std.io.getStdOut().writer();
    const errwriter = std.io.getStdErr().writer();
    // var prng = std.rand.DefaultPrng.init(@intCast(u64, std.time.microTimestamp()));
    var prng = std.rand.DefaultPrng.init(0);
    var i: usize = 0;
    const Gen = gen.Gen(Grammar, Node);
    while (i < 20) : (i += 1) {
        try Gen.gen(grammar, args.start_sym, writer, errwriter, prng.random());
        _ = try writer.write("\n");
    }
}

test {
    _ = @import("peg_test.zig");
}
