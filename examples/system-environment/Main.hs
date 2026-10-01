module Main (main) where

import Control.Exception (IOException, try)
import Data.List (isPrefixOf)
import System.Environment (getArgs, getEnvironment, getExecutablePath, getProgName, lookupEnv, setEnv, unsetEnv, withArgs, withProgName)

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
  putStrLn "changed variables:"
  printChangedVariables
  putStrLn "executable:"
  printExecutable

-- | The process environment is whatever started the program, so only what
-- holds for every environment is printed: a variable this example never sets
-- is absent, and no entry has an empty name.
printVariables :: IO ()
printVariables = do
  absent <- lookupEnv "AIHC_EXAMPLE_ABSENT"
  putStrLn ("absent variable: " ++ show absent)
  entries <- getEnvironment
  putStrLn ("every name is nonempty: " ++ show (not (any (null . fst) entries)))

-- | Set, replace, and remove a variable that the example owns.
printChangedVariables :: IO ()
printChangedVariables = do
  setEnv name "first"
  printVariable
  setEnv name "second"
  printVariable
  entries <- getEnvironment
  putStrLn ("entries with the name: " ++ show (length (filter ((== name) . fst) entries)))
  setEnv name ""
  printVariable
  setEnv name "third"
  unsetEnv name
  printVariable
  rejected <- try (setEnv "AIHC=EXAMPLE" "value") :: IO (Either IOException ())
  putStrLn ("name with an equals sign is rejected: " ++ show (either (const True) (const False) rejected))
  where
    name = "AIHC_EXAMPLE_CHANGED"
    printVariable = do
      value <- lookupEnv name
      putStrLn ("changed variable: " ++ show value)

-- | The path depends on where the program runs, so only its form is
-- printed. A host without an executable file cannot give a path.
printExecutable :: IO ()
printExecutable = do
  path <- try getExecutablePath :: IO (Either IOException FilePath)
  putStrLn ("absolute path or no path: " ++ show (either (const True) ("/" `isPrefixOf`) path))

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
