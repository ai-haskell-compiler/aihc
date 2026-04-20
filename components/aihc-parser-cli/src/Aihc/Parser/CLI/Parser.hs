{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry point for aihc-parser.
--
-- This is the unified CLI that supports both parsing (default) and lexing
-- (via @--lex@ flag) modes, plus CPP preprocessing support.
module Aihc.Parser.CLI.Parser
  ( main,
  )
where

import Aihc.Parser.Run (CLIResult (..), runCLI)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.FilePath (normalise, takeFileName, (</>))
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  stdin <- TIO.getContents
  let includePaths = extractIncludePaths args
  includeMap <- preloadIncludes includePaths
  let result = runCLI includeMap args stdin
  putStr (T.unpack (cliStdout result))
  hPutStr stderr (T.unpack (cliStderr result))
  exitWith (cliExitCode result)

-- | Extract --include=path values from raw arguments.
-- This is a simple parser that only looks for --include flags.
extractIncludePaths :: [String] -> [FilePath]
extractIncludePaths = go []
  where
    go acc [] = reverse acc
    go acc ("--include=" : rest) = go acc rest
    go acc ("--include" : p : rest) = go (p : acc) rest
    go acc (arg : rest) =
      case T.breakOn "=" (T.pack arg) of
        (prefix, suffix)
          | prefix == "--include" && T.length suffix > 1 ->
              go (T.unpack (T.drop 1 suffix) : acc) rest
          | otherwise -> go acc rest

-- | Preload include files from the given paths.
-- File paths are loaded directly.
-- Directory paths are recursively traversed and all files are loaded.
-- Keys are normalized absolute paths.
preloadIncludes :: [FilePath] -> IO (Map FilePath Text)
preloadIncludes = foldr go (pure M.empty)
  where
    go path acc = do
      m <- acc
      contents <- loadPath path
      pure (M.union contents m)

-- | Load files from a path (file or directory).
-- File paths are loaded with both absolute and basename keys.
-- Directory paths are recursively traversed and all files are loaded.
-- Keys are normalized absolute paths.
loadPath :: FilePath -> IO (Map FilePath Text)
loadPath path = do
  isDir <- doesDirectoryExist path
  if isDir
    then loadDirectory path
    else do
      content <- TIO.readFile path
      let absPath = normalise path
          baseName = takeFileName path
       in pure (M.fromList [(absPath, content), (baseName, content)])

-- | Recursively load all files in a directory.
-- Each file is indexed by both its absolute path and its basename.
loadDirectory :: FilePath -> IO (Map FilePath Text)
loadDirectory dir = do
  entries <- listDirectory dir
  foldr go (pure M.empty) entries
  where
    go entry acc = do
      m <- acc
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then do
          subMap <- loadDirectory path
          pure (M.union subMap m)
        else do
          content <- TIO.readFile path
          let absPath = normalise path
              baseName = takeFileName path
           in pure (M.union (M.fromList [(absPath, content), (baseName, content)]) m)
