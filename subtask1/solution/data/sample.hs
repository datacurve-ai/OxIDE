-- Sample Haskell file for testing syntax highlighting

module Sample where

import Data.List (sort)
import Control.Monad (forM_)

-- | Main function
main :: IO ()
main = do
    let numbers = [5, 2, 8, 1, 3]
    let sortedNumbers = sort numbers
    putStrLn "Sorted Numbers:"
    forM_ sortedNumbers print
