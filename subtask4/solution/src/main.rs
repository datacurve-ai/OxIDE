use std::io;
use std::time::{Duration, Instant};
use tui::{backend::CrosstermBackend, Terminal};
use crossterm::{
    execute,
    terminal::{enable_raw_mode, disable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    event::{self, Event as CEvent, KeyCode},
};

mod app;
mod ui;

fn main() -> Result<(), io::Error> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    std::panic::set_hook(Box::new(|info| {
        // Reset terminal here
        let _ = disable_raw_mode();
        let _ = execute!(io::stdout(), LeaveAlternateScreen);
        eprintln!("Panic occurred: {:?}", info);
    }));
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Run the application and ensure cleanup on exit
    let res = run_app(&mut terminal);

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    // Handle any errors
    if let Err(err) = res {
        eprintln!("Application error: {:?}", err);
    }

    Ok(())
}


fn run_app<B: tui::backend::Backend>(_terminal: &mut Terminal<B>) -> Result<(), io::Error> {

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = app::App::new();
    if let Err(e) = app.load_files() {
        eprintln!("Error loading files: {}", e);
        return Err(e);
    }
    app.update()?;

    let tick_rate = Duration::from_millis(250);
    let mut last_tick = Instant::now();

    loop {
        ui::draw(&mut terminal, &app)?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if event::poll(timeout)? {
            if let CEvent::Key(key) = event::read()? {
                match key.code {
                    KeyCode::Char('k') => {
                        app.scroll_content_up();
                    }
                    KeyCode::Char('j') => {
                        app.scroll_content_down();
                    }
                    KeyCode::Char('q') => {
                        app.is_running = false;
                        break;
                    }
                    KeyCode::Up => {
                        app.scroll_files_up();
                    }
                    KeyCode::Down => {
                        app.scroll_files_down();
                    }
                    KeyCode::Enter => {
                        if let Err(e) = app.select_file() {
                            eprintln!("Error selecting file: {}", e);
                        }
                    }
                    KeyCode::Backspace => {
                        if let Err(e) = app.go_back() {
                            eprintln!("Error going back: {}", e);
                        }
                    }
                    _ => {}
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    Ok(())
}
