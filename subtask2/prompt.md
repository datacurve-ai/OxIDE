# Overview

Building upon the terminal-based text editor from subtask1, enhance the editor with the following features:

## New Requirements

1. **Mode System**: Implement a robust mode system including Normal, Insert, and Command modes.
   - Normal mode: Default mode for navigation and entering other modes.
   - Insert mode: For text insertion and editing.
   - Command mode: For executing commands like save and quit.

2. **Editor Structure**: Create an `Editor` struct to manage the current mode and handle mode-specific actions.

3. **Cursor Movement**: Implement cursor movement within the file content, including moving down to the next line.

4. **Text Editing**: 
   - Allow inserting characters at the cursor position.
   - Implement backspace functionality to delete characters.

5. **File Operations**:
   - Implement file saving with custom filenames (e.g., `:w filename.txt`).
   - Implement quitting the editor (`:q`).

6. **Action System**: Create an `AppAction` enum to represent various actions the editor can perform.

7. **Key Event Handling**: Implement key event handling for mode transitions and text editing operations.

## Implementation Details

- Create separate modules for `app.rs`, `editor.rs`, `modes/mod.rs`, and `action.rs`.
- In `app.rs`, extend the `App` struct with fields for `cursor_x`, `cursor_y`, and an `Editor` instance.
- Implement methods in `App` for inserting characters, backspace, and cursor movement.
- In `editor.rs`, create the `Editor` struct to manage the current mode and handle key events.
- In `modes/mod.rs`, define the `Mode` enum and implement mode-specific behaviors.
- In `action.rs`, define the `AppAction` enum to represent various editor actions.

## Testing

Ensure your implementation does the following:
- Proper initialization of `App` and `Editor`.
- Text insertion and deletion.
- Cursor movement.
- Mode transitions.
- Content scrolling.
- File operations (loading and updating).

More complex interactions, including:
- Navigating through the file list.
- Entering Insert mode and typing text.
- Saving files with custom names.
- Quitting the application.

