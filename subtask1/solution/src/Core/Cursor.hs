module Core.Cursor (moveCursor, CursorDirection(..)) where

import Core.Types (EditorState(cursorPos, fileLines, buffer))
import Core.Buffer (getLineFromBuffer)
import qualified Data.Text as T
import qualified Data.Map as Map

data CursorDirection = CursorLeft | CursorRight | CursorUp | CursorDown deriving (Eq, Show)

moveCursor :: CursorDirection -> EditorState -> EditorState
moveCursor dir state =
    let (x, y) = cursorPos state
        maxLineIndex = fileLines state - 1
        lineLength lNum = T.length $ getLineFromBuffer (buffer state) lNum
    in case dir of
        CursorLeft  -> if x > 0 then state { cursorPos = (x - 1, y) }
                                else if y > 0 then state { cursorPos = (lineLength (y - 1), y - 1) }
                                              else state
        CursorRight -> let len = lineLength y
                       in if x < len - 1 then state { cursorPos = (x + 1, y) } -- Adjusted to stay within line
                                         else state -- Do not move to next line automatically
        CursorUp    -> if y > 0 then let len = lineLength (y - 1)
                                         newX = min x len
                                     in state { cursorPos = (newX, y - 1) }
                                else state
        CursorDown  -> if y < maxLineIndex then let len = lineLength (y + 1)
                                                    newX = min x len
                                                in state { cursorPos = (newX, y + 1) }
                                         else state