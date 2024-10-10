# Implementation Rubrics

## Basic Editor Functionality
- [x] **Uses terminal for text editing**
    - Implemented using the `tui` and `crossterm` crates in `src/ui.rs` and `src/main.rs`.
  
- [x] **Has a cursor for navigation**
    - Managed via `cursor_x` and `cursor_y` in `src/app.rs`.
  
- [x] **Supports basic text insertion and deletion**
    - Handled by `AppAction::InsertChar`, `insert_char`, and `backspace` methods in `src/app.rs`.
  
- [x] **Allows file navigation and selection**
    - Implemented with `load_files` and `select_file` methods in `src/app.rs`.
  
- [x] **Supports scrolling through file content**
    - Managed with `content_scroll` in `src/app.rs` and rendered in `src/ui.rs`.

## User Interface
- [x] **Displays a file list**
    - Rendered as a `List` widget with a border and title in `src/ui.rs` using the `tui` crate.
  
- [x] **Shows file content with syntax highlighting**
    - Implemented in `src/ui.rs` using the `syntect` crate for syntax highlighting within the `highlight_content` method of `src/app.rs`.
  
- [x] **Includes a status bar with mode and cursor information**
    - Rendered as a `Paragraph` widget in `src/ui.rs`, displaying current mode and cursor position.
  
- [x] **Implements a command-line interface for executing commands**
    - Managed through `CommandMode` in `src/modes/command_mode.rs`, allowing command input and execution.

## File Operations
- [x] **Supports opening files**
    - Handled by the `select_file` and `load_current_file` methods in `src/app.rs`.
  
- [x] **Allows saving files**
    - Implemented via the `save_current_file` and `save_file_as` methods in `src/app.rs`, triggered by `AppAction::ExecuteCommand`.
  
- [x] **Implements basic directory navigation**
    - Managed through `select_file` and `go_back` methods in `src/app.rs`.

## Advanced Features
- [x] **Integrates Lua scripting capabilities**
    - Implemented in `src/lua_integration.rs` using the `mlua` crate to bridge Rust and Lua.
  
- [x] **Supports Markdown preview functionality**
    - Managed by `MarkdownPreview` in `src/markdown_preview.rs`, which converts Markdown to HTML.
  
- [x] **Implements a browser-based preview for Markdown files**
    - Served via an HTTP server in `src/markdown_preview.rs` using the `warp` crate and opened in the default browser by `AppAction::OpenBrowserPreview` in `src/app.rs`.

## Code Structure and Design
- [x] **Modular code organization (separate files for different components)**
    - Organized into modules such as `app`, `ui`, `modes`, `editor`, `action`, `lua_integration`, and `markdown_preview` within the `src/` directory.
  
- [x] **Uses Rust's ownership and borrowing concepts effectively**
    - Utilized throughout the codebase with `Rc`, `RefCell`, and proper borrowing in `src/app.rs` and `src/lua_integration.rs`.
  
- [x] **Implements error handling for file operations and user inputs**
    - Achieved using `Result` types and error messages via `eprintln!` in methods like `select_file`, `save_current_file`, and `load_files` in `src/app.rs`.

## Additional Features
- [x] **Supports custom color schemes**
    - Defined in `src/ui.rs` with customizable `background_color` and theme configurations loaded in `src/app.rs`.
  
- [x] **Implements a plugin system (via Lua integration)**
    - Facilitated through `LuaPluginManager` in `src/lua_integration.rs`, allowing Lua scripts to extend editor functionality.
  
- [x] **Allows execution of Lua commands and scripts**
    - Enabled via `execute_command` and `execute_script` methods in `src/lua_integration.rs`, callable through command mode in `src/modes/command_mode.rs`.

## Mode System
- [x] **Implements Normal mode**
    - Defined in `src/modes/normal_mode.rs` with key bindings for navigation and mode transitions.
  
- [x] **Implements Insert mode**
    - Defined in `src/modes/insert_mode.rs`, allowing text insertion and handling of `Esc` key to return to Normal mode.
  
- [x] **Implements Command mode**
    - Defined in `src/modes/command_mode.rs`, enabling command input and execution.
  
- [x] **Supports mode transitions**
    - Managed through `AppAction::ChangeMode` in `src/action.rs` and handled within `src/editor.rs`.

## Key Commands
- [x] **'q' to quit the editor**
    - Implemented in `NormalMode::handle_key_event` within `src/modes/normal_mode.rs`.
  
- [x] **'w' to save the current file**
    - Handled in `CommandMode::handle_key_event` within `src/modes/command_mode.rs` triggering `AppAction::ExecuteCommand`.
  
- [x] **'i' to enter Insert mode**
    - Implemented in `NormalMode::handle_key_event` within `src/modes/normal_mode.rs`, switching to `InsertMode`.
  
- [x] **'j' for cursor movement (up)**
    - Managed in `NormalMode::handle_key_event` within `src/modes/normal_mode.rs` triggering `AppAction::MoveCursorDown`.
  
- [x] **'k' for cursor movement (down)**
    - Managed in `NormalMode::handle_key_event` within `src/modes/normal_mode.rs` triggering `AppAction::MoveCursorUp`.
  
- [x] **':' to enter Command mode**
    - Implemented in `NormalMode::handle_key_event` within `src/modes/normal_mode.rs`, switching to `CommandMode`.
  
- [x] **'esc' to return to Normal mode**
    - Handled in both `InsertMode::handle_key_event` and `CommandMode::handle_key_event` within their respective modules in `src/modes/`.
  
- [x] **':browser' to activate the browser-based Markdown preview**
    - Executed in `CommandMode::handle_key_event` within `src/modes/command_mode.rs` triggering `AppAction::OpenBrowserPreview`.

## Markdown Preview
- [x] **Implements browser-based Markdown preview**
    - Served via HTTP server in `src/markdown_preview.rs` using the `warp` crate.
  
- [x] **Supports real-time updating of preview**
    - Updated periodically in `src/app.rs` within the main loop in `src/main.rs` when `markdown_preview` is active.

## Lua Scripting
- [x] **Implements Lua script execution**
    - Executed through `execute_script` in `src/lua_integration.rs`, allowing external Lua scripts to run.
  
- [x] **Allows custom commands via Lua scripts**
    - Facilitated by registering commands in Lua through `register_command` in `src/lua_integration.rs` and executing them via `AppAction::ExecuteLuaScript`.

## Performance and Efficiency
- [x] **Uses the binary package for syntax highlighting, particularly for Zig syntax highlighting**
    - Loaded in `App::new` within `src/app.rs` using `syntect`’s `SyntaxSet`.
  
- [x] **Efficient text rendering and updates**
    - Achieved using the `tui` crate for rendering in `src/ui.rs` and efficient state management in `src/app.rs`.
  
- [x] **Responsive user interface with minimal lag**
    - Maintained with a `tick_rate` of 250 milliseconds in `src/main.rs`, ensuring timely UI updates and responsiveness.



