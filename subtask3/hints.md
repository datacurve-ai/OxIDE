## Hint 1 (Slight):

Use a Lua integration crate like `mlua` or `rlua` to embed Lua in your Rust application. Create a `LuaPluginManager` struct that will handle the Lua context and provide methods for script execution. Consider how to safely share the `App` state between Rust and Lua.

## Hint 2 (Medium):

To expose editor functions to Lua:
1. Create a Lua table to represent the editor.
2. Define Rust functions that can be called from Lua, such as `insert_text`.
3. Add these functions to the Lua table.
4. Make the table accessible in the global Lua environment.

Remember to handle the conversion between Lua and Rust types, and manage borrowing rules when accessing the `App` state from Lua functions.

## Hint 3 (Strong):

Extend the command execution system to handle both built-in and Lua-defined commands:
1. In the `App::execute_command` method, first check for built-in commands.
2. If not a built-in command, attempt to execute it as a Lua command.
3. Implement error handling for unknown or failed Lua commands.

In the `LuaPluginManager`, create a method to execute Lua commands that:
1. Attempts to evaluate the command as Lua code.
2. Catches and logs any Lua errors without propagating them to Rust.
3. Returns a result indicating success or failure.
