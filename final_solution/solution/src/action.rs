// src/action.rs

use crate::modes::Mode;

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
    OpenBrowserPreview,
    ScrollFilesUp,
    ScrollFilesDown,
    ExecuteLuaScript(String),
    None,
}
