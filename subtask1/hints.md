## Hint 1 (Slight):

Consider using the `tui` crate to create a split layout in the terminal, with one pane showing the file list and another showing the content of the selected file.

## Hint 2 (Medium):

Implement event handling using the `crossterm` crate to respond to user input. Use key events to navigate the file list (e.g., Up/Down arrows), select files (Enter key), and scroll through file content (e.g., 'j' and 'k' keys for down and up).

## Hint 3 (Final):

Use the `syntect` crate to add syntax highlighting to the displayed file content. Load syntax definitions and themes, and apply them when rendering the content. Organize your code by separating the application state (`app.rs`), the UI rendering logic (`ui.rs`), and the main application loop (`main.rs`). Create a Syntaxes folder and dump the binary for zig syntax or the zig.sublime-text file works too.
