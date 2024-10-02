mod app;
mod ui;
mod editor;
mod modes;
mod action;
mod lua_integration;  // Updated module name

use std::io;
use std::time::{Duration, Instant};
use tui::{backend::CrosstermBackend, Terminal};
use crossterm::{
    execute,
    terminal::{enable_raw_mode, disable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    event::{self, Event as CEvent},
};

use std::rc::Rc;
use std::cell::RefCell;
use lua_integration::LuaPluginManager;

fn main() -> Result<(), io::Error> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    std::panic::set_hook(Box::new(|info| {
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
// src/main.rs


fn run_app<B: tui::backend::Backend + std::io::Write>(
    terminal: &mut Terminal<B>
) -> Result<(), io::Error> {
    enable_raw_mode()?;

    let app = Rc::new(RefCell::new(app::App::new()));
    if let Err(e) = app.borrow_mut().load_files() {
        eprintln!("Error loading files: {}", e);
        return Err(e);
    }

    app.borrow_mut().update()?;

    // Initialize Lua plugin manager
    let mut lua_plugin_manager = LuaPluginManager::new(app.clone());
    if let Err(e) = lua_plugin_manager.initialize() {
        eprintln!("Error initializing Lua plugins: {:?}", e);
    }
    app.borrow_mut().lua_plugin_manager = Some(Rc::new(RefCell::new(lua_plugin_manager)));
    let tick_rate = Duration::from_millis(250);
    let mut last_tick = Instant::now();

    loop {
        {
            // Separate scope for UI drawing to drop immutable borrow
            ui::draw(terminal, &app.borrow())?;
        }

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if event::poll(timeout)? {
            if let CEvent::Key(key_event) = event::read()? {
                let action = app.borrow_mut().editor.handle_key_event(key_event);
                app.borrow_mut().process_action(action);
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
        
        // Exit condition to break the loop and make code reachable
        if !app.borrow().is_running {
            break;
        }
    }

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    
    Ok(())
}
