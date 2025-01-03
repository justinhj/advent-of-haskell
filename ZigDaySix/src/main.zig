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
    row: usize,
    col: usize,
};

const PositionAndDir = struct {
    pos: Position,
    dir: Dir,
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

fn isForwardBlocked(pos: Position, dir: Dir, grid: [][]Loc) bool {
    const new_pos = moveForward(pos, dir);

    // First check if the new position is in bounds
    if (new_pos.row >= grid.len or new_pos.col >= grid[0].len) {
        return false;
    }

    // Check if the location is blocked
    return grid[new_pos.row][new_pos.col] == Loc.BLOCKED;
}

fn guardStart(grid: [][]Loc) !Position {
    for (grid, 0..) |row, row_index| {
        for (row, 0..) |loc, col_index| {
            if (loc == Loc.GUARD) {
                return Position{ .row = row_index, .col = col_index };
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
    return (@as(u64, pos.row) << 32) | pos.col;
}

fn visitedPositions(allocator: std.mem.Allocator, grid: [][]Loc) !std.AutoHashMap(u64, void) {
    var visited = std.AutoHashMap(u64, void).init(allocator);

    for (grid, 0..) |row, i| {
        for (row, 0..) |loc, j| {
            if (loc == Loc.VISITED) {
                const pos = Position{ .row = i, .col = j };
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
    grid: [][]Loc, // Mutable reference to grid
) !struct { score: usize, grid: [][]Loc } {
    // Ensure grid dimensions are valid
    if (grid.len == 0 or grid[0].len == 0) {
        return error.InvalidGrid;
    }

    // Check if position is out of bounds
    if (pos.row >= grid.len or pos.col >= grid[0].len) {
        const s = try score(allocator, grid);
        return .{ .score = s, .grid = grid };
    }

    // Check if forward is blocked
    if (isForwardBlocked(pos, dir, grid)) {
        // Recursive call with a turned direction
        return try search(allocator, pos, turnRight(dir), grid);
    }

    // Not blocked, so mark current spot as visited
    grid[pos.row][pos.col] = Loc.VISITED;

    // Move forward
    const new_pos = moveForward(pos, dir);

    // Recursive call with updated position and grid
    return try search(allocator, new_pos, dir, grid);
}

fn searchStep(
    allocator: std.mem.Allocator,
    pos: Position,
    dir: Dir,
    grid: [][]Loc, // Mutable reference to grid
) !struct { score: usize, grid: [][]Loc } {
    // Ensure grid dimensions are valid
    if (grid.len == 0 or grid[0].len == 0) {
        return error.InvalidGrid;
    }

    // Check if position is out of bounds
    if (pos.row >= grid.len or pos.col >= grid[0].len) {
        const s = try score(allocator, grid);
        return .{ .score = s, .grid = grid };
    }

    // Check if forward is blocked
    if (isForwardBlocked(pos, dir, grid)) {
        // Recursive call with a turned direction
        return try search(allocator, pos, turnRight(dir), grid);
    }

    // Not blocked, so mark current spot as visited
    grid[pos.row][pos.col] = Loc.VISITED;

    // Move forward
    const new_pos = moveForward(pos, dir);

    // Recursive call with updated position and grid
    return try search(allocator, new_pos, dir, grid);
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

fn testBlockPositionHelper(
    allocator: std.mem.Allocator,
    m: [][]Loc,
    spot: Position,
    dir: Dir,
    visited: *std.AutoHashMap(PositionAndDir, void),
) bool {
    // Check if position is out of bounds
    if (spot.row >= m.len or spot.col >= m[0].len) {
        return false;
    }

    // Check if the (spot, dir) pair is in the visited set
    const key = .{ spot, dir };
    if (visited.contains(key)) return true;

    // Insert the current (spot, dir) pair into the visited set
    visited.put(key, {}) catch unreachable;

    // Get the new spot and direction using searchStep
    const result = searchStep(m, spot, dir);
    const newSpot = result[0];
    const newDir = result[1];

    return testBlockPositionHelper(allocator, m, newSpot, newDir, visited);
}

// testBlockPosition function
fn testBlockPosition(
    allocator: std.mem.Allocator,
    m: [][]Loc,
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
    blockedMap[bp.row][bp.col] = Loc.BLOCKED;

    var visited = std.AutoHashMap(PositionAndDir, void).init(allocator);
    defer visited.deinit();

    return testBlockPositionHelper(allocator, blockedMap, gs, Dir.N, &visited);
}

pub fn solve(allocator: std.mem.Allocator, grid: [][]Loc) !usize {
    const gs = try guardStart(grid);
    const searchResult = try search(allocator, gs, Dir.N, grid);
    var vps = try visitedPositions(allocator, searchResult.grid);
    defer vps.deinit();

    _ = vps.remove(positionToKey(gs));

    var it = vps.keyIterator();
    var blockedCount: usize = 0;
    while (it.next()) |key| {
        const blockPosition = keyToPosition(key.*);
        const t = testBlockPosition(allocator, grid, gs, blockPosition);
        if (t) {
            blockedCount += 1;
        }
    }
    return blockedCount;
}

pub fn main() !void {
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

    // Load the file content into a string
    const file_content = try loadFileToString(allocator, file_path);
    defer allocator.free(file_content);

    const grid = try parseGrid(allocator, file_content);
    defer freeGrid(allocator, grid);

    const stdout = std.io.getStdOut().writer();
    // // Print the grid
    // for (grid) |row| {
    //     for (row) |loc| {
    //         const char: u8 = switch (loc) {
    //             .EMPTY => '.',
    //             .BLOCKED => '#',
    //             .GUARD => '^',
    //             .VISITED => 'V',
    //         };
    //         try stdout.print("{c}", .{char});
    //     }
    //     try stdout.print("\n", .{});
    // }

    const result = solve(allocator, grid);
    try stdout.print("Result: {!}\n", .{result});
}
