module Main where

import Control.Exception ( assert )
import qualified Test.Heap as Heap
import qualified Test.Heap.Internal as Internal
import qualified Test.Heap.Item as Item
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "Ensuring assertions are not ignored:"
    result <- quickCheckWithResult (Args Nothing 1 1 1 True maxBound) $ expectFailure (assert False True)
    putStrLn ""
    case result of
        (Success _ _ _) -> do
            putStrLn "Tests for Data.Heap.Internal:" >> Internal.runTests >> putStrLn ""
            putStrLn "Tests for Data.Heap.Item:"     >> Item.runTests     >> putStrLn ""
            putStrLn "Tests for Data.Heap:"          >> Heap.runTests
        _           -> return ()
