// src/modes/mod.rs

pub mod normal_mode;
pub mod insert_mode;
pub mod command_mode;

use crossterm::event::KeyEvent;
use crate::action::AppAction;

/// Trait that defines how each mode handles key events.
pub trait ModeHandler {
    /// Handles a key event and returns an AppAction.
    fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction;
}

/// Represents the different modes of the editor.
pub enum Mode {
    Normal(normal_mode::NormalMode),
    Insert(insert_mode::InsertMode),
    Command(command_mode::CommandMode),
}

impl Mode {
    /// Delegates the key event handling to the current mode.
    pub fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction {
        match self {
            Mode::Normal(mode) => mode.handle_key_event(key_event),
            Mode::Insert(mode) => mode.handle_key_event(key_event),
            Mode::Command(mode) => mode.handle_key_event(key_event),
        }
    }
}
