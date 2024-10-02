use tui::{
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Terminal,
};
use std::io;
use crate::modes::Mode;
use crate::app::App;
use tui::layout::Rect;

pub fn draw<B: tui::backend::Backend>(
    terminal: &mut Terminal<B>,
    app: &App
) -> Result<(), io::Error> {
    terminal.draw(|f| {
        let size = f.size();

        // Define color scheme
        let background_color = Color::Rgb(240, 231, 213); // #f0e7d5

        // Create main layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Min(1), // Main content
                    Constraint::Length(1), // Status bar
                ]
                .as_ref(),
            )
            .split(size);

        // Split main content into file list and file content
        let main_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(30), Constraint::Percentage(70)].as_ref())
            .split(chunks[0]);

        // Render file list
        let files: Vec<ListItem> = app
            .files
            .iter()
            .enumerate()
            .map(|(i, file)| {
                let style = if i == app.selected_file_index {
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(Color::Blue)
                };
                ListItem::new(file.clone()).style(style)
            })
            .collect();

        let files_list = List::new(files)
            .block(
                Block::default()
                    .title("Files")
                    .borders(Borders::ALL)
                    .style(Style::default().bg(background_color).fg(Color::Black)),
            )
            .highlight_style(Style::default().add_modifier(Modifier::BOLD));
        f.render_widget(files_list, main_chunks[0]);

        // Get content area dimensions
        let content_area = main_chunks[1];
        let content_width = content_area.width as usize;

        // Render file content with syntax highlighting
        let highlighted_text = app.highlight_content(content_width);

        let text = Paragraph::new(highlighted_text)
            .block(
                Block::default()
                    .title("Content")
                    .borders(Borders::ALL)
                    .style(Style::default().bg(background_color).fg(Color::Black)),
            )
            .scroll((app.content_scroll as u16, 0));
        f.render_widget(text, content_area);

        // Render status bar
        let mode_string = match &app.editor.mode {
            Mode::Normal(_) => "NORMAL",
            Mode::Insert(_) => "INSERT",
            Mode::Command(_) => "COMMAND",
        };

        // Handle Command Mode input
        if let Mode::Command(ref command_mode) = &app.editor.mode {
            let cmd_area = Rect {
                x: 0,
                y: size.height - 2,
                width: size.width,
                height: 1,
            };
            let cmd_input = Paragraph::new(Spans::from(vec![
                Span::styled(":", Style::default().fg(Color::Yellow)),
                Span::raw(&command_mode.input),
            ]))
            .style(Style::default().bg(Color::Black).fg(Color::White));
            f.render_widget(cmd_input, cmd_area);
        }

        let status_bar = Paragraph::new(Spans::from(vec![
            Span::styled(
                format!(" {} ", mode_string),
                Style::default()
                    .bg(Color::DarkGray)
                    .fg(Color::White)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw(" "),
            Span::styled(
                format!(
                    "File: {} ",
                    app.files.get(app.selected_file_index).unwrap_or(&String::new())
                ),
                Style::default().bg(Color::DarkGray).fg(Color::White),
            ),
            Span::raw(" "),
            Span::styled(
                format!("Line: {}, Col: {} ", app.cursor_y + 1, app.cursor_x + 1),
                Style::default().bg(Color::DarkGray).fg(Color::White),
            ),
        ]))
        .style(Style::default().bg(Color::DarkGray));

        f.render_widget(status_bar, chunks[1]);
    })?;
    Ok(())
}
