module Main where

import           System.ReadEditor

main :: IO ()
main = do
    putStrLn ">>> Opening editor for you to write me some content"
    content <- readEditorWith "Lots of crazy\n\nStuff"
    putStrLn ">>> You wrote:"
    putStr content
