## Hint 1 (Slight):

Use the `tui` crate to create a split layout in the terminal. The left pane should display the file list, and the right pane should show the content of the selected file. Implement basic navigation using the `crossterm` crate to handle key events (Up/Down arrows for file list, 'j' and 'k' for content scrolling).

## Hint 2 (Medium):

Implement the `App` struct in `app.rs` to manage the application state. Include fields for:
- `files`: A vector of file names
- `selected_file_index`: The currently selected file in the list
- `current_file`: The content of the selected file
- `content_scroll`: The current scroll position in the file content

Implement methods for file navigation, content scrolling, and file loading. Use the `crossterm` crate for handling key events in `main.rs`, updating the `App` state accordingly.

## Hint 3 (Final):

Integrate the `syntect` crate for syntax highlighting:
1. In `app.rs`, add `syntax_set` and `theme` fields to the `App` struct.
2. Load syntax definitions and themes in the `App::new()` method, ensuring Zig syntax is included.
3. Implement a method to apply syntax highlighting to file content.
4. In `ui.rs`, use the highlighting information when rendering file content.

Organize your code as follows:
- `main.rs`: Set up the terminal, run the main event loop, and handle input events.
- `app.rs`: Define the `App` struct and implement methods for state management and syntax highlighting.
- `ui.rs`: Implement functions to draw the UI components using `tui`.

Create a `Syntaxes` folder in your project and include the Zig syntax definition file (e.g., `zig.sublime-syntax`) to ensure Zig highlighting works correctly.

Remember to implement proper error handling and ensure the terminal is restored to its original state on exit or panic.
