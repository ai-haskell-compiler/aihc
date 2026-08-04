module Main (main) where

import System.Environment (getArgs, getProgName, withArgs, withProgName)

main :: IO ()
main = do
  putStrLn "initial:"
  printEnvironment
  putStrLn "modified:"
  withProgName "path/modified-program" (withArgs ["runtime", "changed"] printEnvironment)
  putStrLn "restored:"
  printEnvironment

printEnvironment :: IO ()
printEnvironment = do
  programName <- getProgName
  arguments <- getArgs
  putStrLn ("program name: " ++ programName)
  putStrLn "arguments:"
  printArguments arguments

printArguments :: [String] -> IO ()
printArguments [] = return ()
printArguments (argument : rest) = do
  putStrLn argument
  printArguments rest
