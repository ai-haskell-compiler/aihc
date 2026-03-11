module Main where

import           Console.Binance.Exports.Main


main :: IO ()
main = do
    args <- getArgs
    cfg  <- loadConfigFile
    run cfg args
