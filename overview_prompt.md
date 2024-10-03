# Terminal Text Editor in Rust

Your task is to build a sophisticated, terminal text editor in Rust. This project is divided into four subtasks, each building upon the previous one to create a feature-rich editing experience.

## Project Overview

1. **Basic Editor Functionality** (Subtask 1)
   - Implement core features like file navigation, content display, and syntax highlighting.
   - Focus on creating a solid foundation for the editor's user interface and file interaction.

2. **Advanced Editing and Mode System** (Subtask 2)
   - Introduce a mode system (Normal, Insert, Command) similar to Vim.
   - Implement advanced text editing features and file operations.

3. **Lua Scripting Integration** (Subtask 3)
   - Add Lua scripting support to allow for custom plugins and extended functionality.
   - Create a bridge between Rust and Lua for seamless integration.

4. **Markdown Preview Feature** (Subtask 4)
   - Implement a browser-based Markdown preview functionality.
   - Add real-time updating of the preview as the Markdown file is edited.


## Technology Stack

- Rust as the primary programming language
- Various Rust crates for specific functionalities (details in subtask overviews)
- Lua for scripting support
- Web technologies for Markdown preview

## Getting Started

Begin with Subtask 1 and progress sequentially. Each subtask's overview provides detailed requirements and implementation hints. Refer to the respective `overview_prompt.md` files for specific instructions on each phase of the project.


**Key Commands:**

- `q`: Quit the editor.
- `w`: Save the current file.
- `i`: Enter Insert mode for text insertion.
- `j`: Up
- `k`: Down
- `:` command mode
- `esc`: Normal mode
- `:browser`: Activate the browser-based Markdown preview.

