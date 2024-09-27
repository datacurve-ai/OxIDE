# Overview of Codebase

This Haskell project implements a terminal-based text editor with features such as syntax highlighting, cursor manipulation, and event handling. It utilizes the `vty` library to handle terminal input/output and rendering.

## Project Structure

The project is organized into several directories and files:

- **`app/Main.hs`**: Entry point of the application. It reads command-line arguments, initializes the editor, and invokes the `startEditor` function from the UI module.
- **`data/sample.hs`**: A sample Haskell file used for testing syntax highlighting in the editor.
- **`cabal.project` and `editor.cabal`**: Cabal configuration files that describe the project’s dependencies and build instructions.
- **`src/Core/`**: Contains core components of the editor:
  - `Buffer.hs`: Manages loading and interacting with the file buffer.
  - `Cursor.hs`: Handles cursor movement and manipulation.
  - `Types.hs`: Defines various data types used throughout the editor (e.g., `EditorState`, `Syntax`, `Mode`).

- **`src/UI/`**: Contains user interface-related logic:
  - `Display.hs`: Responsible for rendering the editor and handling the main editor loop.
  - `EventHandler.hs`: Handles key events and updates the editor state accordingly.
  - `Render.hs`: Responsible for rendering the editor buffer and UI components.
  - `StatusBar.hs`: Manages the rendering of the editor’s status bar.
  - `SyntaxHighlight.hs`: Highlights code based on the detected syntax (e.g., Haskell, Python).

## Main Workflow

1. **Main Entry**: The program starts in `Main.hs`, which processes the file input and initializes the editor state.
2. **Buffer Loading**: `loadBuffer` in `Core.Buffer` reads the file into memory, and `detectSyntax` determines the file's syntax based on the file extension.
3. **Rendering**: The `renderEditor` function in `UI.Render` draws the file's contents to the terminal, using `highlightLine` from `UI.SyntaxHighlight` to apply syntax-specific coloring.
4. **Event Handling**: User input is handled by `handleEvent` in `UI.EventHandler`, which updates the editor state, manages cursor movement, and switches between different modes (Normal, Insert, Command).
5. **Editor State**: The `EditorState` in `Core.Types` stores the editor’s current state, including the buffer, cursor position, mode, and screen size.

## Building and Running

To build and run the editor:

1. Ensure you have Cabal and GHC installed.
2. Run `cabal build` to build the project.
3. Execute the editor with `cabal run editor <filename>` where `<filename>` is the file you want to open.
