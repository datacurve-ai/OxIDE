# overview_prompt.md

Your task is to build a Vim-like terminal text editor in Rust. The editor should:

- Display a list of directories and files, allowing users to navigate through them.
- Show the content of selected files with syntax highlighting.
- Allow users to scroll through file content.
- Provide a user interface using the `tui` and `crossterm` crates.

Use the following Rust crates:

- `tui` for building the terminal user interface.
- `crossterm` for handling terminal input and control.
- `syntect` for syntax highlighting of file content.

Structure your application into the following modules:

- `main.rs`: Initializes the terminal, sets up the application loop, handles input events, and ensures proper cleanup on exit.
- `app.rs`: Contains the `App` struct representing the application state, and methods for file loading, navigation, and content handling.
- `ui.rs`: Handles the drawing of UI components such as the file list, content display with syntax highlighting, and status bar.

Ensure that your application gracefully handles errors and restores the terminal state upon exit or panic.
