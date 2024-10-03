## Hint 1 (Slight):

Implement the mode system using an enum `Mode` with variants for Normal, Insert, and Command modes. Create a trait `ModeHandler` that defines common behavior for all modes, such as handling key events. Each mode should implement this trait.

## Hint 2 (Medium):

Extend the `App` struct to include cursor position (`cursor_x` and `cursor_y`) and an `Editor` instance. Implement methods for text insertion, deletion, and cursor movement. For example:

```rust
impl App {
    fn insert_char(&mut self, c: char) {
        // Insert character at cursor position
        // Update cursor position
    }

    fn backspace(&mut self) {
        // Remove character before cursor
        // Update cursor position
    }

    fn move_cursor_down(&mut self) {
        // Move cursor to next line
        // Handle edge cases (e.g., end of file)
    }
}
```

## Hint 3 (Strong):

For syntax highlighting, use the `syntect` crate's `SyntaxSet` and `Theme` to parse and highlight the file content. You can embed the syntax definitions and themes using `include_bytes!` for portability.
