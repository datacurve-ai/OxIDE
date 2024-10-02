use super::ModeHandler;
use crossterm::event::{KeyCode, KeyEvent};
use crate::action::AppAction;
use crate::modes::{Mode, normal_mode::NormalMode};

/// Represents the Command mode of the editor.
pub struct CommandMode {
    pub input: String,
}

impl CommandMode {
    /// Creates a new instance of CommandMode.
    pub fn new() -> Self {
        CommandMode {
            input: String::new(),
        }
    }
}

impl ModeHandler for CommandMode {
    fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction {
        match key_event.code {
            KeyCode::Char(c) => {
                self.input.push(c);
                AppAction::None // No immediate action
            }
            KeyCode::Backspace => {
                self.input.pop();
                AppAction::None
            }
            KeyCode::Enter => {
                let cmd = self.input.clone();
                AppAction::ExecuteCommand(cmd)
            }
            KeyCode::Esc => {
                // Cancel command input and return to Normal mode
                AppAction::ChangeMode(Mode::Normal(NormalMode::new()))
            }
            _ => AppAction::None,
        }
    }
}
