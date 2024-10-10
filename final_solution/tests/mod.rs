// tests/tests.rs

#[path = "../solution/src/app.rs"]
mod app;
#[path = "../solution/src/editor.rs"]
mod editor;
#[path = "../solution/src/modes/mod.rs"]
mod modes;
#[path = "../solution/src/action.rs"]
mod action;
#[path = "../solution/src/lua_integration.rs"]
mod lua_integration;
#[path = "../solution/src/markdown_preview.rs"]
mod markdown_preview;


use crate::action::AppAction;
use crate::app::App;
use crate::modes::insert_mode::InsertMode;
use crate::modes::normal_mode::NormalMode;
use crate::modes::ModeHandler;
use crate::modes::Mode;
use crate::editor::Editor;
use crossterm::event::{KeyCode, KeyEvent};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_initialization() {
        let app = App::new();
        assert!(app.files.is_empty(), "Files should be empty upon initialization.");
        assert!(app.current_file.is_empty(), "Current file should be empty upon initialization.");
        assert!(app.is_running, "App should be running upon initialization.");
        assert_eq!(app.selected_file_index, 0, "Selected file index should start at 0.");
        assert!(app.markdown_preview.is_none(), "Markdown preview should be none upon initialization.");
    }

    #[test]
    fn test_open_browser_preview() {
        let mut app = App::new();
        assert!(app.markdown_preview.is_none(), "Markdown preview should initially be none.");

        app.process_action(AppAction::OpenBrowserPreview);

        // Check that the markdown_preview is initialized and active
        assert!(app.markdown_preview.is_some(), "Markdown preview should be initialized.");
        let preview = app.markdown_preview.as_ref().unwrap();
        assert!(preview.is_active, "Markdown preview should be active after opening.");

        // Since actual browser opening and server starting involve side effects,
        // success if `is_active` is true.
    }

    #[test]
    fn test_move_cursor() {
        let mut app = App::new();
        app.current_file = String::from("Line1\nLine2\nLine3");
        app.cursor_x = 0;
        app.cursor_y = 0;

        app.move_cursor_down();
        assert_eq!(app.cursor_y, 1, "Cursor should move down to line 2.");
        assert_eq!(app.cursor_x, 0, "Cursor X should remain at 0.");

        app.move_cursor_right();
        assert_eq!(app.cursor_x, 1, "Cursor should move right to column 2.");
        assert_eq!(app.cursor_y, 1, "Cursor Y should remain at line 2.");

        app.move_cursor_up();
        assert_eq!(app.cursor_y, 0, "Cursor should move up to line 1.");
        assert_eq!(app.cursor_x, 1, "Cursor X should remain at column 2.");

        app.move_cursor_left();
        assert_eq!(app.cursor_x, 0, "Cursor should move left to column 1.");
        assert_eq!(app.cursor_y, 0, "Cursor Y should remain at line 1.");
    }

    #[test]
    fn test_command_mode_quit() {
        let mut app = App::new();
        app.is_running = true;

        let mut command_mode = modes::command_mode::CommandMode::new();
        command_mode.input = String::from("q");

        let action = command_mode.handle_key_event(KeyEvent::from(KeyCode::Enter));
        app.process_action(action);

        assert!(!app.is_running, "App should not be running after quit command.");
    }

    #[test]
    fn test_command_mode_save() {
        let mut app = App::new();
        app.current_file = String::from("Test content");
        app.files.push("test.txt".to_string());
        app.selected_file_index = 0;
        let path = std::path::Path::new("test.txt");

        // Ensure the file doesn't exist before the test
        if path.exists() {
            std::fs::remove_file(path).expect("Failed to remove existing test file.");
        }

        let mut command_mode = modes::command_mode::CommandMode::new();
        command_mode.input = String::from("w");

        let action = command_mode.handle_key_event(KeyEvent::from(KeyCode::Enter));
        app.process_action(action);

        // Check if the file was created and contains the correct content
        assert!(path.exists(), "Test file should exist after save command.");
        let content = std::fs::read_to_string(path).expect("Failed to read test file.");
        assert_eq!(content, "Test content", "Test file content should match.");

        // Clean up the test file
        std::fs::remove_file(path).expect("Failed to remove test file.");
    }

    #[test]
    fn test_handle_key_event_insert_mode() {
        let mut app = App::new();
        app.editor.set_mode(Mode::Insert(InsertMode::new()));

        let key_event = KeyEvent::from(KeyCode::Char('a'));
        let action = app.editor.handle_key_event(key_event);

        assert!(
            matches!(action, AppAction::InsertChar('a')),
            "Insert mode should return InsertChar action."
        );
    }

    #[test]
    fn test_handle_key_event_normal_mode() {
        let mut app = App::new();
        app.editor.set_mode(Mode::Normal(NormalMode::new()));

        let key_event = KeyEvent::from(KeyCode::Char('i'));
        let action = app.editor.handle_key_event(key_event);

        // In normal mode, 'i' should switch to insert mode
        assert!(
            matches!(action, AppAction::ChangeMode(Mode::Insert(_))),
            "Normal mode should switch to Insert mode on 'i' key."
        );
    }
    #[test]
    fn test_app_new() {
        let app = App::new();
        assert!(app.files.is_empty());
        assert!(app.current_file.is_empty());
        assert!(app.is_running);
        assert_eq!(app.selected_file_index, 0);
    }

    #[test]
    fn test_editor_new() {
        let editor = Editor::new();
        assert!(matches!(editor.mode, Mode::Normal(_)));
    }

    #[test]
    fn test_insert_char() {
        let mut app = App::new();
        app.current_file = "Hello".to_string();
        app.cursor_y = 0;
        app.cursor_x = 0;
        app.insert_char('H');
        assert_eq!(app.current_file, "HHello");
        assert_eq!(app.cursor_x, 1);
    }

    #[test]
    fn test_backspace() {
        let mut app = App::new();
        app.current_file = "Hello".to_string();
        app.cursor_y = 0;
        app.cursor_x = 1;
        app.backspace();
        assert_eq!(app.current_file, "ello");
        assert_eq!(app.cursor_x, 0);
    }

    #[test]
    fn test_move_cursor_down() {
        let mut app = App::new();
        app.current_file = "Line1\nLine2".to_string();
        app.cursor_y = 0;
        app.cursor_x = 3;
        app.move_cursor_down();
        assert_eq!(app.cursor_y, 1);
        assert_eq!(app.cursor_x, 3);
    }

    #[test]
    fn test_mode_transition() {
        let mut app = App::new();
        let key_event = KeyEvent::new(KeyCode::Char('i'), crossterm::event::KeyModifiers::NONE);
        
        // Handle key event via app's editor
        let action = app.editor.handle_key_event(key_event);
        app.process_action(action);
        assert!(matches!(app.editor.mode, Mode::Insert(_)));
    }
    #[test]
    fn test_scroll_content_up() {
        let mut app = App::new();
        app.content_scroll = 1;
        app.scroll_content_up();
        assert_eq!(app.content_scroll, 0, "Content scroll should move up to 0.");
    }

    #[test]
    fn test_scroll_content_down() {
        let mut app = App::new();
        app.current_file = "Line1\nLine2\nLine3".to_string();
        app.content_scroll = 0;
        app.scroll_content_down();
        assert_eq!(app.content_scroll, 1, "Content scroll should move down to 1.");
    }

    

    #[test]
    fn test_update() {
        let mut app = App::new();
        app.load_files().unwrap();
        app.update().unwrap();
        // Add assertions based on expected behavior, e.g., loaded files are correct
        assert!(!app.files.is_empty(), "Files should be loaded after update.");
    }

}

