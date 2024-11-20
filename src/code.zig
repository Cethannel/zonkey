const std = @import("std");

pub const Instructions = std.ArrayList(u8);

pub const Opcode = enum(u8) {
    Constant = 0,
};

pub const Operation = union(Opcode) {
    const t = @This();

    Constant: u16,

    pub fn to_bytes(self: @This(), alloc: std.mem.Allocator) ![]const u8 {
        const info = @typeInfo(t);
        inline for (info.Union.fields) |fld| {
            if (std.mem.eql(u8, @tagName(self), fld.name)) {
                const field = @field(self, fld.name);
                const size: usize = @sizeOf(fld.type);
                const tmp: [size]u8 = @bitCast(@byteSwap(field));
                const out = try alloc.alloc(u8, size + 1);
                out[0] = @intFromEnum(self);
                @memcpy(out[1..], &tmp);
                return out;
            }
        }

        return error.BadComptime;
    }

    pub fn extend_instructions(self: @This(), instructions: *Instructions) !void {
        const info = @typeInfo(t);
        inline for (info.Union.fields) |fld| {
            if (std.mem.eql(u8, @tagName(self), fld.name)) {
                const field = @field(self, fld.name);
                const size: usize = @sizeOf(fld.type);
                const tmp: [size]u8 = @bitCast(@byteSwap(field));
                try instructions.append(@intFromEnum(self));
                try instructions.appendSlice(&tmp);
            }
        }
    }
};

test "Make" {
    const tests = [_]struct { op: Operation, expected: []const u8 }{
        .{
            .op = Operation{ .Constant = 65534 },
            .expected = &.{ @intFromEnum(Opcode.Constant), 255, 254 },
        },
    };

    for (tests) |tt| {
        var arenaAlloc = std.heap.ArenaAllocator.init(std.testing.allocator);
        const alloc = arenaAlloc.allocator();

        var intrs = Instructions.init(alloc);

        try tt.op.extend_instructions(&intrs);
        const made = try tt.op.to_bytes(alloc);

        try std.testing.expectEqualSlices(u8, tt.expected, intrs.items);
        try std.testing.expectEqualSlices(u8, tt.expected, made);

        arenaAlloc.deinit();
    }
}
