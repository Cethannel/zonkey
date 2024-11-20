const std = @import("std");

fn clone_new_inner(value: anytype, alloc: std.mem.Allocator, tla: std.mem.Allocator) anyerror!@TypeOf(value) {
    const t: type = @TypeOf(value);

    const info = @typeInfo(t);

    switch (info) {
        .Int, .Float, .Bool, .Void => {
            return value;
        },
        .Optional => {
            if (value) |val| {
                const out: t = try clone_new_inner(val, alloc, tla);
                return out;
            } else {
                return value;
            }
        },
        .Array => |arr| {
            var out: t = undefined;
            if (arr.sentinel) |sentinal| {
                out = try alloc.allocSentinel(arr.child, arr.len, sentinal);
            } else {
                out = try alloc.alloc(arr.child, arr.len);
            }
            errdefer alloc.free(out);
            for (value, 0..) |val, i| {
                out[i] = try clone_new_inner(val, alloc, tla);
            }
            return out;
        },
        .Struct => |stct| {
            if (@hasDecl(t, "clone_new_alloc")) {
                return value.clone_new_alloc(tla);
            }
            var out: t = undefined;
            inline for (stct.fields) |fld| {
                if (comptime strEql(fld.name, "allocator") or strEql(fld.name, "alloc")) {
                    @field(out, fld.name) = tla;
                    continue;
                }

                @field(out, fld.name) = try clone_new_inner(@field(value, fld.name), alloc, tla);
            }
            return out;
        },
        .Pointer => |ptr| {
            switch (ptr.size) {
                .One => {
                    if (ptr.is_const) {
                        return value;
                    }
                    const out = try alloc.create(ptr.child);
                    out.* = try clone_new_inner(value.*, alloc, tla);
                    return out;
                },
                .Slice => {
                    var out: t = undefined;
                    if (ptr.sentinel) |sentinal| {
                        out = try alloc.allocSentinel(ptr.child, value.len, @as(*const ptr.child, @ptrCast(sentinal)).*);
                    } else {
                        out = try alloc.alloc(ptr.child, value.len);
                    }

                    const vT =
                        if (ptr.sentinel) |sent|
                        [:@as(*const ptr.child, @ptrCast(sent)).*]ptr.child
                    else
                        []ptr.child;

                    var vOut: vT = @constCast(out);

                    for (value, 0..) |val, i| {
                        vOut[i] = try clone_new_inner(val, alloc, tla);
                    }

                    return out;
                },
                else => {
                    @compileError(std.fmt.comptimePrint("Type: {s} is not supporterd", .{@typeName(t)}));
                },
            }
        },
        .Union => |un| {
            if (@hasDecl(t, "clone_new_alloc")) {
                return value.clone_new_alloc(tla);
            }
            inline for (un.fields) |fld| {
                if (strEql(@tagName(value), fld.name)) {
                    const inner = @field(value, fld.name);
                    const outInner = try clone_new_inner(inner, alloc, tla);
                    return @unionInit(t, fld.name, outInner);
                }
            }
            @panic("Failed to find tag of value");
        },
        else => {
            @compileError(std.fmt.comptimePrint("Type: {s} is not supporterd", .{@tagName(info)}));
        },
    }
}

pub fn clone_new_alloc(value: anytype, new_alloc: std.mem.Allocator) !@TypeOf(value) {
    var arenaAlloc = std.heap.ArenaAllocator.init(new_alloc);
    errdefer arenaAlloc.deinit();
    const alloc = arenaAlloc.allocator();

    const out = try clone_new_inner(value, alloc, new_alloc);

    return out;
}

fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
