# hints.md

### **Slight Hint**


*Use an embedded Lua interpreter, such as the `mlua` crate, to facilitate communication between Rust and Lua. Ensure that you expose necessary editor functions to Lua by creating Lua-accessible functions within this module. This will allow Lua scripts to interact with and manipulate the editor's state effectively.*
Especially: `mlua = { version = "0.7.4", features = ["lua54", "vendored"] }`

### **Medium Hint**

*Address Rust's ownership and lifetime rules by utilizing smart pointers like `Rc<RefCell<App>>` and `Weak<RefCell<App>>`. This will enable safe and flexible sharing of the editor's state between Rust and Lua without running into borrowing conflicts. Additionally, implement a command registration system where Lua scripts can register and execute custom commands, enhancing the editor's extensibility.*

### **Big Hint**

*Use an action-command approach and implement the two necessary functions to pass the tests:*
```
AppAction::ExecuteCommand(cmd) => self.execute_command(&cmd),
            AppAction::SelectFile => {
                if let Err(e) = self.select_file() {
                    eprintln!("Error selecting file: {}", e);
                }
            }
```
```
pub fn insert_text(&mut self, text: &str) {
        for c in text.chars() {
            if c == '\n' {
                self.insert_newline();
            } else {
                self.insert_char(c);
            }
        }
    }
    
```



