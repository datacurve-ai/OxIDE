# Vim-like Text Editor in Haskell - Subtask 1

## Description

This subtask involves implementing a Vim-like terminal text editor in Haskell with the following features:

- Open and display text files with syntax highlighting.
- Show line numbers.
- Handle large files efficiently.
- Support window resizing and dynamic rendering.
- Display a status bar with file information.
- Basic cursor movement and editing in Normal and Insert modes.

## Building the Editor

Navigate to the `subtask1/` directory and build the project using Cabal:

```bash
cabal build
```
## Running the Editor
Run the editor with a sample file:

```
cabal run editor solution/data/sample.hs
```
## Running Tests
Navigate to the tests/test_scripts/ directory and run the testing script:

```
cd ../tests/test_scripts/
```
```
./run_tests.sh
```
View the test results:
```
cat results.json
```
## Dependencies
GHC (Glasgow Haskell Compiler)
Cabal
vty library for terminal handling
text library for efficient text manipulation
containers library for data structures
hspec and QuickCheck for testing
## Notes

Syntax highlighting supports Haskell and Python; additional languages can be added.
The editor currently supports Normal and Insert modes; additional modes can be implemented in further subtasks.


