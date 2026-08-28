module Main where

import Message
import System.Environment (getArgs)
import System.IO ()

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> putStrLn message
    [first, second] -> do
      putStrLn first
      putStrLn second
    _ -> putStrLn "unexpected arguments"
