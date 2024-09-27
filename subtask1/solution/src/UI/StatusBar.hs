module UI.StatusBar (renderStatusBar) where

import Graphics.Vty
import Core.Types
import qualified Data.Text as T

renderStatusBar :: EditorState -> Image
renderStatusBar state =
    string statusAttr statusLine
  where
    statusAttr = defAttr `withBackColor` blue `withStyle` bold
    statusLine = " " ++ fileInfo ++ " | " ++ modeInfo ++ " | " ++ cursorInfo ++ padding
    fileInfo = filePath state
    modeInfo = show $ mode state
    cursorInfo = "Ln " ++ show (snd (cursorPos state) + 1) ++ ", Col " ++ show (fst (cursorPos state) + 1)
    screenWidth = fst $ screenSize state
    padding = replicate (screenWidth - length statusLine) ' '
