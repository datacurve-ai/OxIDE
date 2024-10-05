// src/main.rs

mod action;
mod app;
mod editor;
mod lua_integration;
mod markdown_preview;
mod modes;
mod ui;

use crossterm::terminal::size as terminal_size; 


use crate::app::App;
use crossterm::{
    event::{self, Event},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use std::cell::RefCell;
use std::io;
use std::rc::Rc;
use std::thread;
use std::time::{Duration, Instant};
use tui::{backend::CrosstermBackend, Terminal};

fn main() -> Result<(), io::Error> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();

    // Panic hook for cleanup
    std::panic::set_hook(Box::new(|info| {
        let _ = disable_raw_mode();
        let _ = execute!(io::stdout(), LeaveAlternateScreen);
        eprintln!("Panic occurred: {:?}", info);
    }));

    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Run the application
    if let Err(err) = run_app(&mut terminal) {
        eprintln!("Application error: {:?}", err);
        return Err(err);
    }

    // Restore terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    Ok(())
}

fn run_app<B: tui::backend::Backend + std::io::Write>(
    terminal: &mut Terminal<B>,
) -> Result<(), io::Error> {
    let app = Rc::new(RefCell::new(App::new()));
    app.borrow_mut().load_files()?;
    app.borrow_mut().update()?;

    // Initialize Lua plugin manager
    let mut lua_plugin_manager = lua_integration::LuaPluginManager::new(app.clone());
    if let Err(e) = lua_plugin_manager.initialize() {
        eprintln!("Error initializing Lua plugins: {:?}", e);
    }
    app.borrow_mut().lua_plugin_manager = Some(Rc::new(RefCell::new(lua_plugin_manager)));

    let tick_rate = Duration::from_millis(250);
    let mut last_tick = Instant::now();

    loop {
        terminal.draw(|f| {
            ui::draw(f, &app.borrow());
        })?;

        // Ensure the cursor is shown
        terminal.show_cursor()?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if event::poll(timeout)? {
            if let Event::Key(key_event) = event::read()? {
                let action = app.borrow_mut().editor.handle_key_event(key_event);
                app.borrow_mut().process_action(action);
            }
        }

        if last_tick.elapsed() >= tick_rate {
            // Update preview content if the preview is active
            {
                let mut app_ref_mut = app.borrow_mut();
                if app_ref_mut.markdown_preview.is_some() {
                    app_ref_mut.update_preview_content();
                }
            }

            last_tick = Instant::now();
        }

        if !app.borrow().is_running {
            break;
        }
    }

    // Clean up terminal
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;

    Ok(())
}