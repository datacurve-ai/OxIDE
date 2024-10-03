use super::ModeHandler;
use crossterm::event::{KeyCode, KeyEvent};
use crate::action::AppAction;
use crate::modes::{Mode, normal_mode::NormalMode};

/// Represents the Insert mode of the editor.
pub struct InsertMode;

impl InsertMode {
    /// Creates a new instance of InsertMode.
    pub fn new() -> Self {
        InsertMode
    }
}

impl ModeHandler for InsertMode {
    fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction {
        match key_event.code {
            KeyCode::Esc => AppAction::ChangeMode(Mode::Normal(NormalMode::new())),
            KeyCode::Char(c) => AppAction::InsertChar(c),
            KeyCode::Backspace => AppAction::Backspace,
            KeyCode::Enter => AppAction::InsertNewline,
            _ => AppAction::None,
        }
    }
}
