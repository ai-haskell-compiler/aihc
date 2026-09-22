module Main (main) where

import System.Environment (getArgs)

-- The runtime takes the options between +RTS and -RTS for itself and
-- leaves every other argument to the program, including one that merely
-- looks like a runtime option.
main :: IO ()
main = getArgs >>= mapM_ putStrLn
