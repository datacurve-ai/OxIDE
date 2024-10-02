use crossterm::event::KeyEvent;
use crate::modes::Mode;
use crate::action::AppAction;

/// Represents the editor component of the application.
pub struct Editor {
    pub mode: Mode,
}

impl Editor {
    /// Creates a new instance of Editor.
    pub fn new() -> Self {
        Editor {
            mode: Mode::Normal(super::modes::normal_mode::NormalMode::new()),
        }
    }

    /// Handles a key event by delegating it to the current mode and returns an `AppAction`.
    pub fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction {
        self.mode.handle_key_event(key_event)
    }

    /// Sets the current mode of the editor.
    pub fn set_mode(&mut self, mode: Mode) {
        self.mode = mode;
    }
}
