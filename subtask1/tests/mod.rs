#[path = "../solution/src/app.rs"]
mod app;

#[cfg(test)]
mod tests {
    use super::app::App;
    use syntect::easy::HighlightLines;
    use syntect::util::LinesWithEndings;

    #[test]
    fn test_app_new() {
        let app = App::new();
        assert!(app.files.is_empty());
        assert_eq!(app.selected_file_index, 0);
        assert_eq!(app.content_scroll, 0);
    }

    #[test]
    fn test_navigation() {
        let mut app = App::new();
        app.files = vec![
            "file1.txt".to_string(),
            "file2.txt".to_string(),
            "file3.txt".to_string(),
        ];

        app.scroll_files_down();
        assert_eq!(app.selected_file_index, 1);

        app.scroll_files_up();
        assert_eq!(app.selected_file_index, 0);
    }

    #[test]
    fn test_content_scrolling() {
        let mut app = App::new();
        app.current_file = "Line1\nLine2\nLine3\nLine4\nLine5".to_string();

        app.scroll_content_down();
        assert_eq!(app.content_scroll, 1);

        app.scroll_content_up();
        assert_eq!(app.content_scroll, 0);
    }

    #[test]
    fn test_zig_syntax_highlighting() {
        let app = App::new();
        
        let zig_syntax = app.syntax_set.find_syntax_by_extension("zig");
        
        assert!(zig_syntax.is_some(), "Zig syntax should be loaded");

        let zig_code = r#"
const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, World!\n", .{});
}
"#;

        let syntax = zig_syntax.unwrap();
        let mut h = HighlightLines::new(syntax, &app.theme);
        
        for line in LinesWithEndings::from(zig_code) {
            let _ = h.highlight_line(line, &app.syntax_set).unwrap();
        }
        
        // If no panic occurred, highlighting works
    }
}