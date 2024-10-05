use crossterm::terminal::size as terminal_size;
use crate::lua_integration::LuaPluginManager;
use crate::action::AppAction;
use std::fs;
use std::path::PathBuf;
use syntect::dumps::from_binary;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::SyntaxSet;
use syntect::easy::HighlightLines;
use syntect::util::LinesWithEndings;
use tui::{
    style::{Color, Style as TuiStyle},
    text::{Span, Spans},
};
use crate::editor::Editor;
use crate::modes::Mode;

use std::rc::Rc;
use std::cell::RefCell;
use crate::markdown_preview::MarkdownPreview;

pub struct App {
    pub files: Vec<String>,
    pub current_file: String,
    pub is_running: bool,
    pub selected_file_index: usize,
    pub syntax_set: SyntaxSet,
    pub theme: Theme,
    pub content_scroll: usize,
    pub current_path: PathBuf,
    pub cursor_x: usize,
    pub cursor_y: usize,
    pub editor: Editor,
    pub lua_plugin_manager: Option<Rc<RefCell<LuaPluginManager>>>, 
    pub markdown_preview: Option<MarkdownPreview>,
}

impl App {
    pub fn new() -> Self {
        // Load default syntaxes
        let syntax_set: SyntaxSet = from_binary(include_bytes!("syntaxes/assets/packaged-syntaxes.packdump"));

        // Load the theme
        let theme_set = ThemeSet::load_defaults();
        let theme = theme_set.themes["base16-ocean.dark"].clone();

        App {
            files: Vec::new(),
            current_file: String::new(),
            is_running: true,
            selected_file_index: 0,
            syntax_set,
            theme,
            content_scroll: 0,
            current_path: PathBuf::from("."),
            editor: Editor::new(),
            cursor_x: 0,
            cursor_y: 0,
            lua_plugin_manager: None,
            markdown_preview: None,
        }
    }
    // src/app.rs

    pub fn toggle_markdown_preview(&mut self) {
        if let Some(preview) = &mut self.markdown_preview {
            // If the preview is active, deactivate it
            self.markdown_preview = None;
        } else {
            // Activate the preview
            let preview = MarkdownPreview::new();
            preview.update_content(&self.current_file);
            self.markdown_preview = Some(preview);
        }
    }


    // Updates the Markdown preview content.
    pub fn update_preview_content(&mut self) {
        if let Some(preview) = &self.markdown_preview {
            preview.update_content(&self.current_file);
        }
    }
    


    pub fn update(&mut self) -> Result<(), std::io::Error> {
        if !self.files.is_empty()
            && !self.current_path.join(&self.files[self.selected_file_index]).is_dir()
        {
            self.load_current_file()?; // Load the current file's content
        }
        Ok(())
    }
    pub fn initialize_markdown_preview(&mut self) {
        if self.markdown_preview.is_none() {
            let preview = MarkdownPreview::new();
            preview.update_content(&self.current_file);
            self.markdown_preview = Some(preview);
        }
    }

    pub fn process_action(&mut self, action: AppAction) {
        match action {
            AppAction::MoveCursorLeft => self.move_cursor_left(),
            AppAction::MoveCursorRight => self.move_cursor_right(),
            AppAction::MoveCursorUp => self.move_cursor_up(),
            AppAction::MoveCursorDown => self.move_cursor_down(),
            AppAction::ChangeMode(mode) => self.editor.set_mode(mode),
            AppAction::Quit => self.is_running = false,
            AppAction::InsertChar(c) => self.insert_char(c),
            AppAction::Backspace => self.backspace(),
            AppAction::InsertNewline => self.insert_newline(),
            AppAction::ExecuteCommand(cmd) => self.execute_command(&cmd),
            AppAction::SelectFile => {
                if let Err(e) = self.select_file() {
                    eprintln!("Error selecting file: {}", e);
                }
            },
            AppAction::GoBack => {
                if let Err(e) = self.go_back() {
                    eprintln!("Error going back: {}", e);
                }
            },
            
            AppAction::OpenBrowserPreview => {
                self.initialize_markdown_preview();

                // Start the server if it's not already running
                if !self.markdown_preview.as_ref().unwrap().is_active {
                    let preview_clone = self.markdown_preview.as_ref().unwrap().clone();
                    // Start the server in a new thread
                    std::thread::spawn(move || {
                        let rt = tokio::runtime::Runtime::new().expect("Failed to create Tokio runtime");
                        if let Err(e) = rt.block_on(preview_clone.serve()) {
                            eprintln!("Error starting preview server: {}", e);
                        }
                    });

                    // Set the is_active flag to true
                    self.markdown_preview.as_mut().unwrap().is_active = true;
                }

                if let Err(e) = open::that("http://127.0.0.1:3030") {
                    eprintln!("Failed to open browser: {}", e);
                }
            }
            AppAction::ScrollFilesUp => self.scroll_files_up(),
            AppAction::ScrollFilesDown => self.scroll_files_down(),
            AppAction::ExecuteLuaScript(script_name) => { // New match arm
                if let Some(lua_manager) = &self.lua_plugin_manager {
                    if let Err(e) = lua_manager.borrow().execute_script(&script_name) {
                        eprintln!("Error executing Lua script '{}': {:?}", script_name, e);
                    }
                } else {
                    eprintln!("Lua plugin manager is not initialized.");
                }
            },
            AppAction::None => {},
        }
    }



    pub fn select_file(&mut self) -> Result<(), std::io::Error> {
        if let Some(selected) = self.files.get(self.selected_file_index) {
            let path = self.current_path.join(selected);
            if path.is_dir() {
                self.current_path = path;
                self.selected_file_index = 0;
                self.load_files()?; // Reload files in the new directory
            } else if path.is_file() {
                self.current_file = fs::read_to_string(&path)
                    .unwrap_or_else(|_| String::from("Unable to read file"));
                self.content_scroll = 0; // Reset scroll when loading a new file

                // Disable markdown preview when selecting a new file
                self.markdown_preview = None;
            }
        }
        Ok(())
    }

    pub fn go_back(&mut self) -> Result<(), std::io::Error> {
        if let Some(parent) = self.current_path.parent() {
            self.current_path = parent.to_path_buf();
            self.selected_file_index = 0; // Reset index when going back to parent directory
            self.load_files()?; // Reload files in the parent directory
        }
        Ok(())
    }
    pub fn get_content_height(&self) -> usize {
        match terminal_size() {
            Ok((_, height)) => {
                // From ui.rs, we have:
                // - Vertical Layout: Main Content (Min 1), Status Bar (Length 1)
                // - Horizontal Layout: File List (25%), Content (75%)
                // Additionally, if Command Mode is active, reserve one more line.
                let additional_lines = if let Mode::Command(_) = self.editor.mode {
                    1
                } else {
                    0
                };
                // Ensure we don't underflow
                if height > 2 + additional_lines {
                    (height - 2 - additional_lines) as usize
                } else {
                    1 // Minimum content height
                }
            }
            Err(_) => 10, // Fallback to a default value
        }
    }
    pub fn load_files(&mut self) -> Result<(), std::io::Error> {
        self.files.clear();

        if self.current_path != PathBuf::from("/") {
            self.files.push("..".to_string());
        }

        for entry in fs::read_dir(&self.current_path)? {
            let entry = entry?;
            let file_name = entry.file_name().into_string().unwrap_or_default();
            self.files.push(file_name);
        }
        Ok(())
    }

    pub fn load_current_file(&mut self) -> Result<(), std::io::Error> {
        if let Some(filepath) = self.files.get(self.selected_file_index) {
            let path = self.current_path.join(filepath);
            if path.is_file() {
                self.current_file = fs::read_to_string(&path)
                    .unwrap_or_else(|_| String::from("Unable to read file"));
                self.content_scroll = 0; // Reset scroll when loading a new file
            } else {
                self.current_file.clear();
            }
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub fn scroll_content_up(&mut self) {
        if self.content_scroll > 0 {
            self.content_scroll -= 1;
        }
    }
    #[allow(dead_code)]
    pub fn scroll_content_down(&mut self) {
        let line_count = self.current_file.lines().count();
        if self.content_scroll < line_count.saturating_sub(1) {
            self.content_scroll += 1;
        }
    }

    // Methods for file list scrolling
    pub fn scroll_files_up(&mut self) {
        if self.selected_file_index > 0 {
            self.selected_file_index -= 1;
            if let Err(e) = self.load_current_file() {
                eprintln!("Error loading file: {}", e);
            }
        }
    }
    #[allow(dead_code)]
    pub fn scroll_files_down(&mut self) {
        if !self.files.is_empty() && self.selected_file_index < self.files.len().saturating_sub(1) {
            self.selected_file_index += 1;
            if let Err(e) = self.load_current_file() {
                eprintln!("Error loading file: {}", e);
            }
        }
    }

    pub fn move_cursor_left(&mut self) {
        if self.cursor_x > 0 {
            self.cursor_x -= 1;
        }
    }

    pub fn move_cursor_right(&mut self) {
        if let Some(line) = self.current_file.lines().nth(self.cursor_y) {
            if self.cursor_x < line.chars().count() {
                self.cursor_x += 1;
            }
        }
    }

    pub fn move_cursor_down(&mut self) {
        let max_cursor_y = self.current_file.lines().count().saturating_sub(1);
        if self.cursor_y < max_cursor_y {
            self.cursor_y += 1;
            self.adjust_cursor_x();

            // Calculate the height of the content area
            let content_height = self.get_content_height();

            // Adjust content scroll
            if self.cursor_y >= self.content_scroll + content_height {
                self.content_scroll = self.cursor_y.saturating_sub(content_height - 1);
            }
        }
    }
    pub fn move_cursor_up(&mut self) {
        if self.cursor_y > 0 {
            self.cursor_y -= 1;
            self.adjust_cursor_x();

            // Adjust content scroll
            if self.cursor_y < self.content_scroll {
                self.content_scroll = self.cursor_y;
            }
        }
    }

    fn adjust_cursor_x(&mut self) {
        if let Some(line) = self.current_file.lines().nth(self.cursor_y) {
            if self.cursor_x > line.chars().count() {
                self.cursor_x = line.chars().count();
            }
        } else {
            self.cursor_x = 0;
        }
    }

    // Editing operations
    pub fn insert_char(&mut self, c: char) {
        let mut lines: Vec<String> = self.current_file.lines().map(|s| s.to_string()).collect();
        if let Some(line) = lines.get_mut(self.cursor_y) {
            if self.cursor_x <= line.len() {
                line.insert(self.cursor_x, c);
                self.cursor_x += 1;
            }
        }
        self.current_file = lines.join("\n");
    }

    pub fn backspace(&mut self) {
        let mut lines: Vec<String> = self.current_file.lines().map(|s| s.to_string()).collect();
        if self.cursor_x > 0 {
            if let Some(line) = lines.get_mut(self.cursor_y) {
                if self.cursor_x <= line.len() && self.cursor_x > 0 {
                    line.remove(self.cursor_x - 1);
                    self.cursor_x -= 1;
                }
            }
        } else if self.cursor_y > 0 {
            // Merge current line with previous
            let current_line = lines.remove(self.cursor_y);
            if let Some(prev_line) = lines.get_mut(self.cursor_y - 1) {
                let prev_len = prev_line.len();
                prev_line.push_str(&current_line);
                self.cursor_y -= 1;
                self.cursor_x = prev_len;
            }
        }
        self.current_file = lines.join("\n");
    }

    pub fn insert_newline(&mut self) {
        let mut lines: Vec<String> = self.current_file.lines().map(|s| s.to_string()).collect();
        if self.cursor_y < lines.len() {
            let current_line = &mut lines[self.cursor_y];
            let new_line = current_line.split_off(self.cursor_x);
            lines.insert(self.cursor_y + 1, new_line);
            self.cursor_y += 1;
            self.cursor_x = 0;
            self.current_file = lines.join("\n");
        } else {
            lines.push(String::new());
            self.cursor_y = lines.len() - 1;
            self.cursor_x = 0;
            self.current_file = lines.join("\n");
        }
    }

    // Command execution
    pub fn execute_command(&mut self, command: &str) {
        let parts: Vec<&str> = command.trim().split_whitespace().collect();
        match parts.as_slice() {
            ["w"] => self.save_current_file(),
            ["w", filename] => self.save_file_as(filename),
            ["q"] => self.is_running = false,
            ["wq"] => {
                self.save_current_file();
                self.is_running = false;
            },
            ["lua", script_name] => { // Example: :lua script_name
                self.process_action(AppAction::ExecuteLuaScript(script_name.to_string()));
            },
            _ => {
                // Try to execute Lua command
                if let Some(lua_manager) = &self.lua_plugin_manager {
                    let cmd_name = parts.join(" ");
                    if let Err(e) = lua_manager.borrow().execute_command(&cmd_name) {
                        eprintln!("Error executing Lua command '{}': {:?}", cmd_name, e);
                    }
                } else {
                    eprintln!("Unknown command: {}", command);
                }
            },
        }
    }
    
    fn save_current_file(&self) {
        if let Some(filepath) = self.files.get(self.selected_file_index) {
            let path = self.current_path.join(filepath);
            let content = self.current_file.clone();
            if let Err(e) = fs::write(&path, content) {
                eprintln!("Error saving file: {}", e);
            } else {
                println!("File saved successfully.");
            }
        }
    }

    fn save_file_as(&self, filename: &str) {
        let path = self.current_path.join(filename);
        let content = self.current_file.clone();
        if let Err(e) = fs::write(&path, content) {
            eprintln!("Error saving file as {}: {}", filename, e);
        } else {
            println!("File saved as {} successfully.", filename);
        }
    }

    #[allow(dead_code)]
    pub fn highlight_content(&self, line_width: usize) -> Vec<Spans> {
        let binding = PathBuf::from(&self.files[self.selected_file_index]);
        let extension = binding.extension().and_then(|s| s.to_str()).unwrap_or("");

        let syntax = self
            .syntax_set
            .find_syntax_by_extension(extension)
            .or_else(|| self.syntax_set.find_syntax_by_name("Plain Text"))
            .unwrap();

        let mut highlighted_text = Vec::new();

        let mut h = HighlightLines::new(syntax, &self.theme);

        for line in LinesWithEndings::from(&self.current_file) {
            let ranges = h.highlight_line(line, &self.syntax_set).unwrap();
            let mut spans: Vec<Span> = ranges
                .iter()
                .map(|(style, text)| {
                    Span::styled(
                        text.to_string(),
                        TuiStyle::default()
                            .fg(Color::Rgb(
                                style.foreground.r,
                                style.foreground.g,
                                style.foreground.b,
                            ))
                            .bg(Color::Rgb(
                                style.background.r,
                                style.background.g,
                                style.background.b,
                            )),
                    )
                })
                .collect();

            // Pad the line to fill the background
            let line_length = spans.iter().map(|s| s.content.chars().count()).sum::<usize>();
            if line_length < line_width {
                let padding = " ".repeat(line_width - line_length);
                spans.push(Span::styled(
                    padding,
                    TuiStyle::default().bg(Color::Rgb(240, 231, 213)),
                ));
            }

            highlighted_text.push(Spans(spans));
        }

        highlighted_text
    }

    pub fn insert_text(&mut self, text: &str) {
        for c in text.chars() {
            if c == '\n' {
                self.insert_newline();
            } else {
                self.insert_char(c);
            }
        }
    }
}
