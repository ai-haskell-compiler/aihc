module Main (main) where

import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv, withArgs, withProgName)

main :: IO ()
main = do
  putStrLn "initial:"
  printEnvironment
  putStrLn "modified:"
  withProgName "path/modified-program" (withArgs ["runtime", "changed"] printEnvironment)
  putStrLn "restored:"
  printEnvironment
  putStrLn "variables:"
  printVariables

-- | The process environment is whatever started the program, so only what
-- holds for every environment is printed: a variable this example never sets
-- is absent, and no entry has an empty name.
printVariables :: IO ()
printVariables = do
  absent <- lookupEnv "AIHC_EXAMPLE_ABSENT"
  putStrLn ("absent variable: " ++ show absent)
  entries <- getEnvironment
  putStrLn ("every name is nonempty: " ++ show (not (any (null . fst) entries)))

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
