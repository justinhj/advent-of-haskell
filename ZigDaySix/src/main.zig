const std = @import("std");

// Equivalent to Haskell's `Loc` type
const Loc = enum {
    EMPTY,
    BLOCKED,
    GUARD,
    VISITED,
};

// Equivalent to Haskell's `Dir` type
const Dir = enum {
    N,
    E,
    S,
    W,
};

const Position = struct {
    row: i32,
    col: i32,
    pub fn equals(self: Position, other: Position) bool {
        return self.row == other.row and self.col == other.col;
    }
};

const PositionAndDir = struct {
    pos: Position,
    dir: Dir,
    pub fn equals(self: PositionAndDir, other: PositionAndDir) bool {
        return self.dir == other.dir and self.pos.equals(other.pos);
    }
};

/// Parses a string into a 2D array of `Loc`.
/// The caller is responsible for freeing the allocated memory.
fn parseGrid(allocator: std.mem.Allocator, input: []const u8) ![][]Loc {
    // Split the input into lines
    var lines = std.mem.split(u8, input, "\n");

    // Count the number of lines and the maximum line length
    var row_count: usize = 0;
    var col_count: usize = 0;
    while (lines.next()) |line| {
        if (line.len > 0) {
            row_count += 1;
            if (line.len > col_count) {
                col_count = line.len;
            }
        }
    }

    // Allocate the 2D array
    var grid = try allocator.alloc([]Loc, row_count);
    for (grid) |*row| {
        row.* = try allocator.alloc(Loc, col_count);
    }

    // Reset the iterator and parse the grid
    lines = std.mem.split(u8, input, "\n");
    var row: usize = 0;
    while (lines.next()) |line| : (row += 1) {
        if (line.len > 0) {
            for (line, 0..) |char, col| {
                grid[row][col] = switch (char) {
                    '.' => Loc.EMPTY,
                    '#' => Loc.BLOCKED,
                    '^' => Loc.GUARD,
                    else => Loc.EMPTY, // Treat unknown characters as EMPTY
                };
            }
        }
    }

    return grid;
}

fn freeGrid(allocator: std.mem.Allocator, grid: [][]Loc) void {
    for (grid) |row| {
        allocator.free(row);
    }
    allocator.free(grid);
}

fn moveForward(pos: Position, dir: Dir) Position {
    return switch (dir) {
        .N => Position{ .row = pos.row -| 1, .col = pos.col },
        .E => Position{ .row = pos.row, .col = pos.col +| 1 },
        .S => Position{ .row = pos.row +| 1, .col = pos.col },
        .W => Position{ .row = pos.row, .col = pos.col -| 1 },
    };
}

fn turnRight(dir: Dir) Dir {
    return switch (dir) {
        .N => .E,
        .E => .S,
        .S => .W,
        .W => .N,
    };
}

fn isForwardBlocked(pos: Position, dir: Dir, grid: []const []const Loc) bool {
    const new_pos = moveForward(pos, dir);

    if (new_pos.row >= grid.len or new_pos.col >= grid[0].len or new_pos.row < 0 or new_pos.col < 0) {
        return false;
    }

    // Check if the location is blocked
    return grid[@intCast(new_pos.row)][@intCast(new_pos.col)] == Loc.BLOCKED;
}

fn guardStart(grid: [][]Loc) !Position {
    for (grid, 0..) |row, row_index| {
        for (row, 0..) |loc, col_index| {
            if (loc == Loc.GUARD) {
                return Position{ .row = @intCast(row_index), .col = @intCast(col_index) };
            }
        }
    }
    return error.GuardNotFound;
}

fn keyToPosition(key: u64) Position {
    return Position{
        .row = @intCast(key >> 32),
        .col = @intCast(key & 0xFFFFFFFF),
    };
}

fn positionToKey(pos: Position) u64 {
    // Reinterpret i32 as u32 using @bitCast, then cast to u64
    const row_u32 = @as(u32, @bitCast(pos.row));
    const col_u32 = @as(u32, @bitCast(pos.col));

    // Combine row and col into a u64
    return (@as(u64, row_u32) << 32) | @as(u64, col_u32);
}

fn visitedPositions(allocator: std.mem.Allocator, grid: [][]Loc) !std.AutoHashMap(u64, void) {
    var visited = std.AutoHashMap(u64, void).init(allocator);

    for (grid, 0..) |row, i| {
        for (row, 0..) |loc, j| {
            if (loc == Loc.VISITED) {
                const pos = Position{ .row = @intCast(i), .col = @intCast(j) };
                try visited.put(positionToKey(pos), {});
            }
        }
    }
    return visited;
}

fn score(allocator: std.mem.Allocator, grid: [][]Loc) !usize {
    var visited = try visitedPositions(allocator, grid);
    defer visited.deinit();
    return visited.count();
}

fn search(
    allocator: std.mem.Allocator,
    pos: Position,
    dir: Dir,
    grid: [][]Loc,
) !struct { score: usize, grid: [][]Loc } {
    // Ensure grid dimensions are valid
    if (grid.len == 0 or grid[0].len == 0) {
        return error.InvalidGrid;
    }

    // Check if position is out of bounds
    if (pos.row >= grid.len or pos.col >= grid[0].len or pos.row < 0 or pos.col < 0) {
        const s = try score(allocator, grid);
        return .{ .score = s, .grid = grid };
    }

    // Check if forward is blocked
    if (isForwardBlocked(pos, dir, grid)) {
        // Recursive call with a turned direction
        return try search(allocator, pos, turnRight(dir), grid);
    }

    // Not blocked, so mark current spot as visited
    grid[@intCast(pos.row)][@intCast(pos.col)] = Loc.VISITED;

    // Move forward
    const new_pos = moveForward(pos, dir);

    // Recursive call with updated position and grid
    return try search(allocator, new_pos, dir, grid);
}

fn searchStep(
    pos: Position,
    dir: Dir,
    grid: []const []const Loc,
) PositionAndDir {
    if (isForwardBlocked(pos, dir, grid)) {
        return searchStep(pos, turnRight(dir), grid);
    }

    const new_pos = moveForward(pos, dir);
    return PositionAndDir{ .pos = new_pos, .dir = dir };
}

fn advanceStep(
    m: []const []const Loc,
    pd: PositionAndDir,
) ?PositionAndDir {
    const spot = pd.pos;
    if (spot.row >= m.len or spot.col >= m[0].len or spot.row < 0 or spot.col < 0) {
        return null;
    }

    const dir = pd.dir;
    const result = searchStep(spot, dir, m);
    const newSpot = result.pos;
    const newDir = result.dir;

    return PositionAndDir{ .pos = newSpot, .dir = newDir };
}

// testBlockPosition function
fn testBlockPosition(
    allocator: std.mem.Allocator,
    m: []const []const Loc,
    gs: Position,
    bp: Position,
) bool {
    var blockedMap = allocator.alloc([]Loc, m.len) catch unreachable;
    for (blockedMap, m) |*row, originalRow| {
        row.* = allocator.dupe(Loc, originalRow) catch unreachable;
    }
    defer {
        for (blockedMap) |row| allocator.free(row);
        allocator.free(blockedMap);
    }
    blockedMap[@intCast(bp.row)][@intCast(bp.col)] = Loc.BLOCKED;

    // run two paths in a loop where one advances 2 steps and the other advances
    // one step. as soon as one escapes it is safe, otherwise, if there is a loop
    // then we will find it when the two steps are in the same place

    var p1: ?PositionAndDir = PositionAndDir{ .pos = gs, .dir = Dir.N };
    var p2: ?PositionAndDir = PositionAndDir{ .pos = gs, .dir = Dir.N };

    var loopFound = false;
    while (p1 != null and p2 != null) {
        p1 = advanceStep(blockedMap, p1.?);

        p2 = advanceStep(blockedMap, p2.?);
        if (p2) |p2Val| {
            p2 = advanceStep(blockedMap, p2Val);
        }

        if (p1 != null and p2 != null and p1.?.equals(p2.?)) {
            loopFound = true;
            break;
        }
    }

    return loopFound;
}

pub fn worker(allocator: std.mem.Allocator, m: []const []const Loc, gs: Position, bp: []Position, offset: usize, results: []bool) void {
    var i: usize = 0;
    for (bp) |thisBp| {
        results[offset + i] = testBlockPosition(allocator, m, gs, thisBp);
        i += 1;
    }
}

pub fn solve(allocator: std.mem.Allocator, grid: [][]Loc) !usize {
    const gs = try guardStart(grid);
    const searchResult = try search(allocator, gs, Dir.N, grid);
    var vps = try visitedPositions(allocator, searchResult.grid);
    defer vps.deinit();

    _ = vps.remove(positionToKey(gs));

    const cpuCount = try std.Thread.getCpuCount();

    const vpCount = vps.count();

    const results = try allocator.alloc(bool, vpCount);
    defer allocator.free(results);

    var threads = try allocator.alloc(?std.Thread, cpuCount);
    defer allocator.free(threads);

    const config = std.Thread.SpawnConfig{};

    std.debug.print("cpu count {}\n", .{cpuCount});

    // Create an array of candidate positions for each thread
    var candidates = try allocator.alloc([]Position, cpuCount);
    defer {
        for (candidates) |c| {
            allocator.free(c);
        }
        allocator.free(candidates);
    }

    const candidatesPerThread = vpCount / cpuCount;
    const remainders = vpCount % cpuCount;

    var it = vps.keyIterator();

    for (0..cpuCount) |i| {
        var thisCount: usize = 0;
        if (i < cpuCount - 1) {
            thisCount = candidatesPerThread;
        } else {
            thisCount = candidatesPerThread + remainders;
        }
        candidates[i] = try allocator.alloc(Position, thisCount);

        for (0..thisCount) |j| {
            const key = it.next();
            const blockPosition = keyToPosition(key.?.*);
            candidates[i][j] = blockPosition;
        }
    }

    var ti: usize = 0;
    var off: usize = 0;
    for (candidates) |c| {
        const thread = try std.Thread.spawn(config, worker, .{ allocator, grid, gs, c, off, results });
        // std.debug.print("{} {} {} {}\n", .{ nextIndex, first, cpuCount, vpCount });
        threads[ti] = thread;
        ti += 1;
        off += candidatesPerThread;
    }

    for (threads) |t| {
        if (t) |t1| {
            t1.join();
        }
    }

    var count: usize = 0;
    for (results) |r| {
        if (r) {
            count += 1;
        }
    }

    return count;
}

/// Loads the content of a file into a string using the provided allocator.
fn loadFileToString(allocator: std.mem.Allocator, file_path: []const u8) ![]u8 {
    // Open the file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    // Read the entire file into a string
    const file_size = try file.getEndPos();
    const file_content = try file.readToEndAlloc(allocator, file_size);
    return file_content;
}

pub fn main() !void {
    const startTime = std.time.microTimestamp();

    // Get the allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get the command-line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check if a file path was provided
    if (args.len < 2) {
        std.debug.print("Usage: {any} <file_path>\n", .{args[0]});
        return error.InvalidArguments;
    }

    const file_path = args[1];

    const file_content = try loadFileToString(allocator, file_path);
    defer allocator.free(file_content);

    const grid = try parseGrid(allocator, file_content);
    defer freeGrid(allocator, grid);

    const stdout = std.io.getStdOut().writer();

    const result = solve(allocator, grid);

    const endTime = std.time.microTimestamp();

    try stdout.print("Elapsed time: {} us\n", .{endTime - startTime});
    try stdout.print("Result: {!}\n", .{result});
}
