module Main where

import           Console.Gemini.Exports.Main


main :: IO ()
main = do
    args <- getArgs
    cfg  <- loadConfigFile
    run cfg args
