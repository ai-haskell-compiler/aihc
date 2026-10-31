-- | GCC response files contain quoted or escaped command arguments.
module GHC.ResponseFile
  ( getArgsWithResponseFiles,
    unescapeArgs,
    escapeArgs,
    expandResponse,
  )
where

import Control.Exception (IOException, catch)
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Prelude

-- | Get the process arguments and expand each response file once.
getArgsWithResponseFiles :: IO [String]
getArgsWithResponseFiles = getArgs >>= expandResponse

-- | Remove quotes and escape characters. Discard empty arguments.
unescapeArgs :: String -> [String]
unescapeArgs input = filter (not . null) (map reverse (reverse (parse input Nothing False [] [])))
  where
    parse :: String -> Maybe Char -> Bool -> String -> [String] -> [String]
    parse [] _ _ argument arguments = argument : arguments
    parse (character : rest) quote True argument arguments =
      parse rest quote False (character : argument) arguments
    parse ('\\' : rest) quote False argument arguments =
      parse rest quote True argument arguments
    parse (character : rest) (Just quote) False argument arguments
      | character == quote = parse rest Nothing False argument arguments
      | otherwise = parse rest (Just quote) False (character : argument) arguments
    parse (character : rest) Nothing False argument arguments
      | isSpace character = parse rest Nothing False [] (argument : arguments)
      | character == '\'' || character == '"' = parse rest (Just character) False argument arguments
      | otherwise = parse rest Nothing False (character : argument) arguments

-- | Escape spaces, quotes, and backslashes. Put each argument on a new line.
escapeArgs :: [String] -> String
escapeArgs = unlines . map (concatMap escape)
  where
    escape character
      | isSpace character || character == '\\' || character == '\'' || character == '"' = ['\\', character]
      | otherwise = [character]

-- | Replace each @\@file@ argument with the arguments from that file.
-- Do not expand response-file arguments from within another response file.
expandResponse :: [String] -> IO [String]
expandResponse arguments = concat <$> mapM expand arguments
  where
    expand ('@' : path) = unescapeArgs <$> (readFile path `catch` reportError)
    expand argument = pure [argument]

    reportError :: IOException -> IO String
    reportError exception = do
      hPutStrLn stderr ("Error while expanding response file: " ++ show exception)
      exitFailure
