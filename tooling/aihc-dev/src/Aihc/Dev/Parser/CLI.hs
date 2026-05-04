-- | CLI runner for @aihc-dev parser@.
--
-- This is the unified CLI that supports both parsing (default) and lexing
-- (via @--lex@ flag) modes, plus CPP preprocessing support.
module Aihc.Dev.Parser.CLI
  ( optionsParser,
    run,
  )
where

import Aihc.Dev.Parser.Run (CLIResult (..), Options (..), optionsParser, runParsedCLI)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitWith)
import System.FilePath (normalise, takeFileName, (</>))
import System.IO (hPutStr, stderr)

run :: Options -> IO ()
run opts = do
  input <- maybe TIO.getContents TIO.readFile (optInputFile opts)
  includeMap <- preloadIncludes (optIncludePaths opts)
  let result = runParsedCLI includeMap opts input
  putStr (T.unpack (cliStdout result))
  hPutStr stderr (T.unpack (cliStderr result))
  exitWith (cliExitCode result)

-- | Preload include files from the given paths.
-- File paths are loaded directly.
-- Directory paths are recursively traversed and all files are loaded.
-- Keys are normalized paths.
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
-- Keys are normalized paths.
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
