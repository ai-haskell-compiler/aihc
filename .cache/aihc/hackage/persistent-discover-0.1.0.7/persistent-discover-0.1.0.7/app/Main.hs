module Main where

import Database.Persist.Discover.Exe
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        src : _ : dest : _ ->
            discoverModels (Source src) (Destination dest)
        _ ->
            fail . mconcat $
               [ "persistent-discover: expected to be called with three arguments as an -F -pgmF preprocessor. got:"
               , show args
               ]


