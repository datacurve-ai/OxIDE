module UI.Render (renderEditor) where

import Graphics.Vty
import Core.Types
import UI.SyntaxHighlight (highlightLine)
import UI.StatusBar (renderStatusBar)
import Core.Buffer (getLineFromBuffer)
import qualified Data.Text as T

renderEditor :: EditorState -> Picture
renderEditor state =
    picForLayers [image, statusBar]
  where
    image = renderBuffer state
    statusBar = renderStatusBar state

renderBuffer :: EditorState -> Image
renderBuffer state =
    vertCat $ map renderLine visibleLines
  where
    (width, height) = screenSize state
    vpStart = snd $ viewport state
    numLines = min (height - 1) (fileLines state - vpStart)
    lineNumbersWidth = length $ show $ fileLines state
    visibleLines = [vpStart..(vpStart + numLines - 1)]
    renderLine lnNum =
        string (defAttr `withForeColor` blue) (lineNumber lnNum) <|>
        highlightLine (syntax state) (getLineFromBuffer (buffer state) lnNum)
    lineNumber lnNum = let lnStr = show (lnNum + 1)
                       in replicate (lineNumbersWidth - length lnStr) ' ' ++ lnStr ++ " "

