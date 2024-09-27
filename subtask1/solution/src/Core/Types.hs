module Core.Types where

import qualified Data.Text as T
import qualified Data.Map as Map

data Mode = Normal | Insert | Command deriving (Eq, Show)
data Syntax = PlainText | Haskell | Python deriving (Eq, Show)

type CursorPos = (Int, Int)
type Viewport = (Int, Int)
type Buffer = Map.Map Int T.Text

data EditorState = EditorState
    { buffer     :: Buffer
    , cursorPos  :: CursorPos
    , viewport   :: Viewport
    , mode       :: Mode
    , syntax     :: Syntax
    , filePath   :: FilePath 
    , fileLines  :: Int
    , screenSize :: (Int, Int)
    }

data EditorConfig = EditorConfig
    { configFilePath :: FilePath 
    }