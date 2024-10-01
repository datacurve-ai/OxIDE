use std::fs;

use std::path::PathBuf;
use syntect::dumps::from_binary;
use syntect::highlighting::{Theme, ThemeSet};
use syntect::parsing::{SyntaxSet};
use syntect::easy::HighlightLines;
use syntect::util::LinesWithEndings;
use tui::{
    style::{Color, Style as TuiStyle},
    text::{Span, Spans},
};

pub struct App {
    pub files: Vec<String>,
    pub current_file: String,
    pub is_running: bool,
    pub selected_file_index: usize,
    pub syntax_set: SyntaxSet,
    pub theme: Theme,
    pub content_scroll: usize,
    pub current_path: PathBuf,
}

impl App {
    pub fn new() -> Self {
        // Load default syntaxes and add Zig syntax
        

        let syntax_set: SyntaxSet = from_binary(include_bytes!("syntaxes/assets/packaged-syntaxes.packdump"));

        // Load the theme as before
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

    pub fn load_files(&mut self) -> Result<(), std::io::Error> {
        self.files.clear();

        if self.current_path != PathBuf::from("/") {
            self.files.push("..".to_string());
        }

        for entry in fs::read_dir(&self.current_path)? {
            let entry = entry?;
            let file_name = entry.file_name().into_string().unwrap();
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

    // Methods for content scrolling
    pub fn scroll_content_up(&mut self) {
        if self.content_scroll > 0 {
            self.content_scroll -= 1;
        }
    }

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

    pub fn scroll_files_down(&mut self) {
        if !self.files.is_empty() && self.selected_file_index < self.files.len().saturating_sub(1) {
            self.selected_file_index += 1;
            if let Err(e) = self.load_current_file() {
                eprintln!("Error loading file: {}", e);
            }
        }
    }

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
}
