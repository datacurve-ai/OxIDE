# overview_prompt.md

Your task is to build a Vim-like terminal text editor in Rust. The editor should:

- Display a list of directories and files, allowing users to navigate through them using keyboard inputs (e.g., 'j' for down, 'k' for up).
- Show the content of selected files with syntax highlighting, supporting at least the Zig programming language.
- Allow users to scroll through file content using keyboard inputs (e.g., arrow keys or 'j'/'k' for up/down).
- Provide a user interface using the `tui` and `crossterm` crates.
- Respond to the 'q' key to quit the application.

Use the following Rust crates:

- `tui` for building the terminal user interface.
- `crossterm` for handling terminal input and control.
- `syntect` for syntax highlighting of file content, ensuring Zig syntax is included and functional.

Structure your application into the following modules:

- `main.rs`: Initializes the terminal, sets up the application loop, handles input events such as navigation keys and 'q' to quit, and ensures proper cleanup on exit.
- `app.rs`: Contains the `App` struct representing the application state, and methods for file loading, navigation, content scrolling, and syntax highlighting.
- `ui.rs`: Handles the drawing of UI components such as the file list, content display with syntax highlighting, and status bar.

Ensure that your application gracefully handles errors and restores the terminal state upon exit or panic. Additionally, the application should pass the provided tests in `mod.rs` which check for file navigation, content scrolling, and syntax highlighting, as well as the `interaction.exp` script which tests the interactive behavior of the application.
