// src/lua_integration.rs

use mlua::{Lua, Result as LuaResult, Function as LuaFunction, Table as LuaTable};
use crate::app::App;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

pub struct LuaPluginManager {
    lua: Lua,
    app: Weak<RefCell<App>>, // Use Weak reference
}

impl LuaPluginManager {
    /// Creates a new instance of LuaPluginManager.
    pub fn new(app: Rc<RefCell<App>>) -> Self {
        LuaPluginManager {
            lua: Lua::new(),
            app: Rc::downgrade(&app), // Convert Rc to Weak
        }
    }
    pub fn get_lua(&self) -> &Lua {
        &self.lua
    }
    pub fn initialize(&mut self) -> LuaResult<()> {
        {
            let app_weak = self.app.clone();

            let globals = self.lua.globals();

            // Expose 'editor' table to Lua solution/lua_scripts
            let editor = self.create_editor_table(app_weak.clone())?;
            globals.set("editor", editor)?;

            // Expose 'commands' table
            globals.set("commands", self.lua.create_table()?)?;

            // Expose 'register_command' function
            let register_command = self.lua.create_function(|lua_ctx, (name, func): (String, LuaFunction)| {
                let globals = lua_ctx.globals();
                let commands_table: LuaTable = globals.get("commands")?;
                commands_table.set(name, func)?;
                Ok(())
            })?;
            globals.set("register_command", register_command)?;
        }

        // Load Lua solution/lua_scripts
        self.load_scripts()
    }

    fn create_editor_table(&self, app_weak: Weak<RefCell<App>>) -> LuaResult<LuaTable> {
        let editor = self.lua.create_table()?;
    
        let insert_text = self.lua.create_function(move |_, text: String| {
            if let Some(app_rc) = app_weak.upgrade() {
                let mut app = app_rc.borrow_mut();
                app.current_file.push_str(&text); // Ensure this modifies current_file
                Ok(())
            } else {
                Err(mlua::Error::RuntimeError("Failed to access app".into()))
            }
        })?;
        editor.set("insert_text", insert_text)?;
    
        Ok(editor)
    }
    

    fn load_scripts(&mut self) -> LuaResult<()> {
        use std::fs;

        let script_dir = std::path::Path::new("solution/lua_scripts");
        if script_dir.is_dir() {
            for entry in fs::read_dir(script_dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "lua" {
                            let script_content = fs::read_to_string(&path)?;
                            self.lua.load(&script_content).exec()?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    // Allow executing a Lua command
    pub fn execute_command(&self, cmd: &str) -> LuaResult<()> {
        let globals = self.lua.globals();
        let commands_table: LuaTable = globals.get("commands")?;
        if let Some(func) = commands_table.get::<_, Option<LuaFunction>>(cmd)? {
            func.call::<_, ()>(())?;
        } else {
            println!("Unknown command: {}", cmd);
        }
        Ok(())
    }

    // Method to execute a Lua script file directly
    pub fn execute_script(&self, script_name: &str) -> LuaResult<()> {
        let script_path = format!("solution/lua_scripts/{}.lua", script_name);
        let script_content = std::fs::read_to_string(&script_path)?;
        self.lua.load(&script_content).exec()
    }
}
