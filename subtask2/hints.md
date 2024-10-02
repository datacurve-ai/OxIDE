## Hint 1 (Slight):

Consider defining a `Mode` enum with variants for each editor mode, and a trait `ModeHandler` that each mode implements for handling key events.

## Hint 2 (Medium):

Use the `tui` crate's `Terminal` and `Backend` to manage the rendering of the UI components. Split the UI into chunks to display the file list, file content, and status bar.

## Hint 3 (Strong):

For syntax highlighting, use the `syntect` crate's `SyntaxSet` and `Theme` to parse and highlight the file content. You can embed the syntax definitions and themes using `include_bytes!` for portability.
