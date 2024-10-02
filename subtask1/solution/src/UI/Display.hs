{-# LANGUAGE CPP #-}

module UI.Display (startEditor) where

import Graphics.Vty
import Graphics.Vty.Config (userConfig)
import Core.Types
import Core.Buffer (loadBuffer, detectSyntax)
import UI.Render (renderEditor)
import UI.StatusBar (renderStatusBar)
import UI.EventHandler (handleEvent)
import Control.Monad (void)
import Control.Concurrent (forkIO, threadDelay)
import qualified Graphics.Vty as Vty

#if defined(mingw32_HOST_OS)
import Graphics.Vty.Platform.Windows (mkVty)
#elif defined(darwin_HOST_OS)
import Graphics.Vty.Platform.MacOS (mkVty)
#else
import Graphics.Vty.Platform.Unix (mkVty)
#endif

startEditor :: EditorConfig -> IO ()
startEditor config = do
    initialBuffer <- loadBuffer (configFilePath config)
    let initialState = EditorState
            { buffer     = initialBuffer
            , cursorPos  = (0, 0)
            , viewport   = (0, 0)
            , mode       = Normal
            , syntax     = detectSyntax (configFilePath config)
            , filePath   = configFilePath config
            , fileLines  = Map.size initialBuffer  -- Initialize fileLines with the size of the buffer map
            , screenSize = (0, 0)                  -- Initialize screenSize here
            , undoStack  = []                      -- Initialize undo stack
            , redoStack  = []                      -- Initialize redo stack
            }
    
    cfg <- userConfig  -- Load user configuration with IO action
    vty <- mkVty cfg   -- Ensure mkVty is correctly imported and available
    initialSize <- displayBounds $ outputIface vty
    let stateWithSize = initialState { screenSize = (regionWidth initialSize, regionHeight initialSize) }
    
    void $ forkIO $ refreshScreen vty stateWithSize  -- Start a separate thread for screen refresh
    eventLoop vty stateWithSize  -- Use eventLoop instead of mainLoop for consistency with second code snippet
    shutdown vty

-- Refresh the screen at regular intervals.
refreshScreen :: Vty -> EditorState -> IO ()
refreshScreen vty state = do
    let pic = renderEditor state
    update vty pic
    threadDelay 50000  -- Refresh every 50 milliseconds
    refreshScreen vty state

-- Main event loop to handle user inputs and update editor state.
eventLoop :: Vty -> EditorState -> IO ()
eventLoop vty state = do
    e <- nextEvent vty
    newState <- handleEvent e state
    
    case newState of 
        Nothing -> return () 
        Just nst -> eventLoop vty nst