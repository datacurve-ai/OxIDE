use crossterm::terminal::size as terminal_size;
use crate::app::App;
use crate::modes::Mode;
use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Frame,
};

pub fn draw<B: Backend>(f: &mut Frame<B>, app: &App) {
    let size = f.size();

    // Always draw the editor in full-screen mode
    draw_editor(f, app, size);
}

fn draw_editor<B: Backend>(f: &mut Frame<B>, app: &App, area: Rect) {
    // Define color scheme
    let background_color = Color::Rgb(30, 30, 30); // Dark background
    
    // Create main layout
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Min(1),     // Main content
                Constraint::Length(1),  // Status bar
            ]
            .as_ref(),
        )
        .split(area);

    // Split main content into file list and file content
    let main_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(25), Constraint::Percentage(75)].as_ref())
        .split(chunks[0]);

    // Render file list with a border and title
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
                Style::default().fg(Color::White)
            };
            ListItem::new(file.clone()).style(style)
        })
        .collect();

    let files_list = List::new(files)
        .block(
            Block::default()
                .title("Files")
                .borders(Borders::ALL)
                .style(Style::default().bg(background_color).fg(Color::White)),
        )
        .highlight_style(Style::default().add_modifier(Modifier::BOLD));
    f.render_widget(files_list, main_chunks[0]);

    // Render file content with syntax highlighting
    let content_area = main_chunks[1];
    let content_width = content_area.width as usize;
    set_cursor_position(f, app, content_area);
    let highlighted_text = app.highlight_content(content_width);

    let text = Paragraph::new(highlighted_text)
        .block(
            Block::default()
                .title("Content")
                .borders(Borders::ALL)
                .style(Style::default().bg(background_color).fg(Color::White)),
        )
        .scroll((app.content_scroll as u16, 0));
    f.render_widget(text, content_area);

    // Render status bar with mode and cursor position
    let mode_string = match &app.editor.mode {
        Mode::Normal(_) => "NORMAL",
        Mode::Insert(_) => "INSERT",
        Mode::Command(_) => "COMMAND",
    };

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

    // Set cursor position
    if let Mode::Insert(_) = app.editor.mode {
        let cursor_y = app.cursor_y.saturating_sub(app.content_scroll);
        let cursor_x = app.cursor_x;
        if cursor_y >= content_area.y as usize && cursor_y < (content_area.y + content_area.height) as usize {
            f.set_cursor(
                content_area.x + cursor_x as u16 + 1,
                content_area.y + cursor_y as u16 + 1
            );
        }
    }

    // Handle Command Mode input
    if let Mode::Command(ref command_mode) = &app.editor.mode {
        let cmd_area = Rect {
            x: area.x,
            y: area.y + area.height - 2,
            width: area.width,
            height: 1,
        };
        let cmd_input = Paragraph::new(Spans::from(vec![
            Span::styled(":", Style::default().fg(Color::Yellow)),
            Span::raw(&command_mode.input),
        ]))
        .style(Style::default().bg(Color::Black).fg(Color::White));
        f.render_widget(cmd_input, cmd_area);
    }
}

/// Sets the cursor position based on the current cursor coordinates and scroll.
fn set_cursor_position<B: Backend>(f: &mut Frame<B>, app: &App, content_area: Rect) {
    // Calculate the y position relative to the scroll
    let cursor_y = app.cursor_y.saturating_sub(app.content_scroll);
    let cursor_x = app.cursor_x;

    // Ensure cursor is within the content area
    if cursor_y < content_area.height.saturating_sub(2) as usize {
        let absolute_x = content_area.x + cursor_x as u16 + 1; // +1 for the left border
        let absolute_y = content_area.y + cursor_y as u16 + 1; // +1 for the top border

        // Set the cursor only if it's within the visible content area
        if absolute_y < content_area.y + content_area.height - 1 {
            f.set_cursor(absolute_x, absolute_y);
        }
    }
}