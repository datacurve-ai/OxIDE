module Main where

import System.Environment (getArgs)
import System.Exit (die)
import qualified UI.Display as Display
import Core.Types (EditorConfig(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> Display.startEditor EditorConfig { configFilePath = filename }
        _          -> die "Usage: editor <filename>"
