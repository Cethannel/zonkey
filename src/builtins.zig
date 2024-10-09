const std = @import("std");
const object = @import("object.zig");
const chm = @import("comptime_hash_map");

fn len(args: []object.Object) object.Object {
    if (args.len != 1) {}
}

pub const BuiltIns = chm.ComptimeStringHashMap(object.Object, .{
    .{
        "len", object.Object{
            .Builtin = .{ .fun = len },
        },
    },
});
