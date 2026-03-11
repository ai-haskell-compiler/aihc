module Main where

import System.Environment (getArgs)
import System.Executable.Hash.Internal (injectExecutableHash)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [exe] -> injectExecutableHash exe
        _ -> error "Usage: inject-executable-hash <executable>"
