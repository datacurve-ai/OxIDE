# Overview for Subtask 4

Building upon the Vim-like terminal text editor from previous subtasks, enhance the editor with a browser-based Markdown preview feature.

## New Requirements

1. **Markdown Preview**:
   - Implement a browser-based preview for Markdown files.
   - Create a command (`:browser`) to open the current Markdown file in the default web browser.

2. **HTTP Server**:
   - Implement a simple HTTP server to serve the rendered Markdown content.
   - The server should update the preview in real-time as the file is edited.

3. **Markdown Rendering**:
   - Use a Markdown parsing library to convert Markdown to HTML.
   - Ensure proper rendering of common Markdown elements (headings, lists, code blocks, etc.).

4. **Browser Integration**:
   - Automatically open the default web browser when the preview command is executed.
   - Ensure the preview updates when the Markdown file is modified in the editor.


## Implementation Details

- Create a new module to handle the preview functionality.
- Implement an asynchronous HTTP server using a library.
- Utilize some crate to launch the default web browser.
- Implement the `:browser` command in the command mode to trigger the preview.






