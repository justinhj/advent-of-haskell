const std = @import("std");

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

    // Print the file content
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{file_content});
}
