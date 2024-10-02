use super::ModeHandler;
use crossterm::event::{KeyCode, KeyEvent};
use crate::action::AppAction;
use crate::modes::{Mode, insert_mode::InsertMode, command_mode::CommandMode};

/// Represents the Normal mode of the editor.
pub struct NormalMode;

impl NormalMode {
    /// Creates a new instance of NormalMode.
    pub fn new() -> Self {
        NormalMode
    }
}

impl ModeHandler for NormalMode {
    fn handle_key_event(&mut self, key_event: KeyEvent) -> AppAction {
        match key_event.code {
            KeyCode::Char('i') => AppAction::ChangeMode(Mode::Insert(InsertMode::new())),
            KeyCode::Char('h') => AppAction::MoveCursorLeft,
            KeyCode::Char('j') => AppAction::MoveCursorDown,
            KeyCode::Char('k') => AppAction::MoveCursorUp,
            KeyCode::Char('l') => AppAction::MoveCursorRight,
            KeyCode::Char(':') => AppAction::ChangeMode(Mode::Command(CommandMode::new())),
            KeyCode::Char('q') => AppAction::Quit,
            KeyCode::Up => AppAction::ScrollFilesUp,      
            KeyCode::Down => AppAction::ScrollFilesDown, 
            KeyCode::Enter => AppAction::SelectFile,      
            KeyCode::BackTab => AppAction::GoBack,        
            _ => AppAction::None,
        }
    }
}
