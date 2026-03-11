module Main where

import           System.ReadEditor

main :: IO ()
main = do
    putStrLn ">>> Opening editor for you to write me some content"
    content <- readEditor
    putStrLn ">>> You wrote:"
    putStr content
