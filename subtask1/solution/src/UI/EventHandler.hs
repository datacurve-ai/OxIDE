module UI.EventHandler (handleEvent) where

import Graphics.Vty
import Core.Types
import Core.Cursor (moveCursor, CursorDirection(..))
import Core.Buffer (getLineFromBuffer)
import qualified Data.Text as T
import qualified Data.Map as Map

handleEvent :: Event -> EditorState -> IO (Maybe EditorState)
handleEvent (EvKey key mods) state =
    case mode state of
        Normal -> handleNormalMode key mods state
        Insert -> handleInsertMode key mods state
        Command -> handleCommandMode key mods state
handleEvent (EvResize w h) state =
    return $ Just state { screenSize = (w, h) }
handleEvent _ state = return $ Just state

handleNormalMode :: Key -> [Modifier] -> EditorState -> IO (Maybe EditorState)
handleNormalMode key mods state = case key of
    KChar 'h' -> return $ Just $ moveCursor CursorLeft state
    KChar 'j' -> return $ Just $ moveCursor CursorDown state
    KChar 'k' -> return $ Just $ moveCursor CursorUp state
    KChar 'l' -> return $ Just $ moveCursor CursorRight state
    KChar 'i' -> return $ Just state { mode = Insert }
    KChar ':' -> return $ Just state { mode = Command }
    KEsc      -> return $ Just state
    _         -> return $ Just state

handleInsertMode :: Key -> [Modifier] -> EditorState -> IO (Maybe EditorState)
handleInsertMode key mods state = case key of
    KEsc    -> return $ Just state { mode = Normal }
    KChar c -> return $ Just $ insertCharacter c state
    KBS     -> return $ Just $ deleteCharacter state
    KEnter  -> return $ Just $ insertNewLine state
    _       -> return $ Just state

handleCommandMode :: Key -> [Modifier] -> EditorState -> IO (Maybe EditorState)
handleCommandMode _ _ state = return $ Just state -- Placeholder for command handling

insertCharacter :: Char -> EditorState -> EditorState
insertCharacter c state =
    let (x, y) = cursorPos state
        line = getLineFromBuffer (buffer state) y
        (before, after) = T.splitAt x line
        newLine = T.concat [before, T.singleton c, after]
        newBuffer = Map.insert y newLine (buffer state)
    in state { buffer = newBuffer, cursorPos = (x + 1, y) }

deleteCharacter :: EditorState -> EditorState
deleteCharacter state =
    let (x, y) = cursorPos state
        line = getLineFromBuffer (buffer state) y
        newLine = if x > 0 then T.concat [T.take (x - 1) line, T.drop x line] else line
        newBuffer = Map.insert y newLine (buffer state)
        newCursorX = max 0 (x - 1)
    in state { buffer = newBuffer, cursorPos = (newCursorX, y) }

insertNewLine :: EditorState -> EditorState
insertNewLine state =
    let (x, y) = cursorPos state
        line = getLineFromBuffer (buffer state) y
        (before, after) = T.splitAt x line
        newBuffer = Map.insert y before 
                  $ Map.insert (y + 1) after 
                  $ Map.mapKeys (\k -> if k > y then k + 1 else k) (buffer state)
        newFileLines = fileLines state + 1
    in state { buffer = newBuffer, cursorPos = (0, y + 1), fileLines = newFileLines }
