module Core.Buffer (loadBuffer, detectSyntax, getLineFromBuffer) where

import Core.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)
import qualified Data.Map as Map

loadBuffer :: FilePath -> IO (Buffer, Int)
loadBuffer path = do
    contents <- TIO.readFile path
    let linesList = T.lines contents
        bufferMap = Map.fromList $ zip [0..] linesList
    return (bufferMap, length linesList)

detectSyntax :: FilePath -> Syntax
detectSyntax path = case takeExtension path of
    ".hs" -> Haskell
    ".py" -> Python
    _     -> PlainText

getLineFromBuffer :: Buffer -> Int -> T.Text
getLineFromBuffer buf lineNum = Map.findWithDefault T.empty lineNum buf
