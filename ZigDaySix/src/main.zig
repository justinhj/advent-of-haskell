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
    // Get the allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get the command-line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Check if a file path was provided
    if (args.len < 2) {
        std.debug.print("Usage: {s} <file_path>\n", .{args[0]});
        return error.InvalidArguments;
    }

    const file_path = args[1];

    // Load the file content into a string
    const file_content = try loadFileToString(allocator, file_path);
    defer allocator.free(file_content);

    const grid = try parseGrid(allocator, file_content);
    defer freeGrid(allocator, grid);

    // Print the grid
    const stdout = std.io.getStdOut().writer();
    for (grid) |row| {
        for (row) |loc| {
            const char: u8 = switch (loc) {
                .EMPTY => '.',
                .BLOCKED => '#',
                .GUARD => '^',
                .VISITED => 'V',
            };
            try stdout.print("{c}", .{char});
        }
        try stdout.print("\n", .{});
    }
}
