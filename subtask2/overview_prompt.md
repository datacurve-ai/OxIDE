# Overview

You are tasked with building a terminal-based text editor in Rust that mimics some functionalities of Vim. The editor should support multiple modes (Normal, Insert, and Command), file navigation, text editing, and basic commands like saving and quitting. Additionally, it should feature syntax highlighting for code files.

## Requirements

- Implement an application structure that separates concerns into modules: app, ui, editor, modes, and action.
- Use the `tui` crate for terminal user interface rendering.
- Use the `crossterm` crate for handling keyboard events.
- Implement the following modes:
  - **Normal Mode**: Navigate files and the cursor, switch modes.
  - **Insert Mode**: Insert text into the current file.
  - **Command Mode**: Execute commands (e.g., save, quit).
- Implement syntax highlighting using the `syntect` crate.
- Load and display files from the current directory.
- Implement scrolling for both the file list and file content (j/k for up/down)
- `i` to go insert mode, `esc` to go back to normal mode, and `:` to go to command mode.
- `:w` saves a file and `:q` quits the program.
- `enter` to enter a file


1. **Setup the Project Structure**: Organize your code into modules for better maintainability.
2. **Implement the App Struct**: This will manage the state of the application, including file management, cursor position, and modes.
3. **Implement Modes**: Create separate structs for each mode, implementing a common trait for handling key events.
4. **Implement the Editor**: The editor should handle the current mode and delegate key events appropriately.
5. **Implement UI Rendering**: Use the `tui` crate to render the file list, file content with syntax highlighting, and a status bar.
6. **Handle Key Events**: Use the `crossterm` crate to capture and process user input, changing modes and updating the state as needed.
7. **Implement Syntax Highlighting**: Use the `syntect` crate to highlight the content based on file extension.
