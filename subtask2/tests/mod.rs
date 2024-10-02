// tests/mod.rs

#[path = "../solution/src/app.rs"]
mod app;
#[path = "../solution/src/editor.rs"]
mod editor;
#[path = "../solution/src/modes/mod.rs"]
mod modes;
#[path = "../solution/src/action.rs"]
mod action;

#[cfg(test)]
mod tests {
    use super::app::App;
    use super::editor::Editor;
    use super::modes::Mode;
    use super::action::AppAction;
    use crossterm::event::{KeyCode, KeyEvent};

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

