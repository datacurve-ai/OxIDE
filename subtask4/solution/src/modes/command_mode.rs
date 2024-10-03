// src/modes/command_mode.rs

use super::ModeHandler;
use crossterm::event::{KeyCode, KeyEvent};
use crate::action::AppAction;
use crate::modes::{Mode, normal_mode::NormalMode};

pub struct CommandMode {
    pub input: String,
}

impl CommandMode {
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
                AppAction::None
            }
            KeyCode::Backspace => {
                self.input.pop();
                AppAction::None
            }
            KeyCode::Enter => {
                let cmd = self.input.clone();
                self.input.clear();
                if cmd.starts_with("lua ") {
                    let script = cmd.trim_start_matches("lua ").to_string();
                    AppAction::ExecuteLuaScript(script)
                } else {
                    match cmd.as_str() {
                        "browser" => AppAction::OpenBrowserPreview,
                        _ => AppAction::ExecuteCommand(cmd),
                    }
                }
            }
            KeyCode::Esc => {
                AppAction::ChangeMode(Mode::Normal(NormalMode::new()))
            }
            _ => AppAction::None,
        }
    }
}
