module Main (main) where

import Test.Hspec
import Core.Types
import Core.Buffer
import Core.Cursor
import qualified Data.Text as T
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
    describe "Core.Buffer" $ do
        it "loads buffer correctly" $ do
            (buffer, totalLines) <- loadBuffer "data/sample.hs"
            totalLines `shouldSatisfy` (> 0)
            Map.size buffer `shouldBe` totalLines

        it "retrieves lines from buffer" $ do
            (buffer, _) <- loadBuffer "data/sample.hs"
            let line = getLineFromBuffer buffer 0
            T.null line `shouldBe` False

    describe "Core.Cursor" $ do
        let sampleState = EditorState
                { buffer = Map.fromList [(0, T.pack "Line1"), (1, T.pack "Line2")]
                , cursorPos = (5, 0)
                , viewport = (0, 0)
                , mode = Normal
                , syntax = PlainText
                , filePath = "test.txt"
                , fileLines = 2
                , screenSize = (80, 24)
                }

        it "moves cursor left correctly" $ do
            let newState = moveCursor Left sampleState
            cursorPos newState `shouldBe` (4, 0)

        it "moves cursor right correctly" $ do
            let newState = moveCursor Right sampleState
            cursorPos newState `shouldBe` (5, 0)

        it "moves cursor up correctly" $ do
            let stateWithCursor = sampleState { cursorPos = (0, 1) }
            let newState = moveCursor Up stateWithCursor
            cursorPos newState `shouldBe` (0, 0)

        it "moves cursor down correctly" $ do
            let newState = moveCursor Down sampleState
            cursorPos newState `shouldBe` (5, 1)
