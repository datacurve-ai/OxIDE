{-# LANGUAGE CPP #-}

module UI.Display (startEditor) where

import Graphics.Vty
import Graphics.Vty.Config (userConfig)
import Core.Types
import Core.Buffer (loadBuffer, detectSyntax)
import UI.Render (renderEditor)
import UI.StatusBar (renderStatusBar)
import UI.EventHandler (handleEvent)
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
    (initialBuffer, totalLines) <- loadBuffer (configFilePath config)
    let initialState = EditorState
            { buffer     = initialBuffer
            , cursorPos  = (0, 0)
            , viewport   = (0, 0)
            , mode       = Normal
            , syntax     = detectSyntax (configFilePath config)
            , filePath   = configFilePath config
            , fileLines  = totalLines
            , screenSize = (0, 0)
            }
    
    cfg <- userConfig  -- Load user configuration with IO action
    vty <- mkVty cfg   -- Ensure mkVty is correctly imported and available
    initialSize <- displayBounds $ outputIface vty
    let stateWithSize = initialState { screenSize = (regionWidth initialSize, regionHeight initialSize) }
    
    mainLoop vty stateWithSize
    shutdown vty

mainLoop :: Vty -> EditorState -> IO ()
mainLoop vty state = do
    let pic = renderEditor state
    update vty pic
    e <- nextEvent vty
    newState <- handleEvent e state
    
    case newState of 
        Nothing -> return () 
        Just nst -> mainLoop vty nst
