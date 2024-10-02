// src/action.rs

use crate::modes::Mode;

/// Represents all possible actions that can be performed in the application.
pub enum AppAction {
    MoveCursorLeft,
    MoveCursorRight,
    MoveCursorUp,
    MoveCursorDown,
    ChangeMode(Mode),
    Quit,
    InsertChar(char),
    Backspace,
    InsertNewline,
    ExecuteCommand(String),
    SelectFile,
    GoBack,
    ScrollFilesUp,
    ScrollFilesDown,
    None,
}
