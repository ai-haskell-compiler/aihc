module Main (main) where

import Control.Exception (finally)
import Control.Monad (unless)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile, readFile')
import System.IO.Error (catchIOError)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi
          buildHook simpleUserHooks pkg lbi hooks flags,
        replHook = \pkg lbi hooks flags args -> do
          generateIdentity pkg lbi
          replHook simpleUserHooks pkg lbi hooks flags args,
        haddockHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi
          haddockHook simpleUserHooks pkg lbi hooks flags
      }

generateIdentity :: PackageDescription -> LocalBuildInfo -> IO ()
generateIdentity pkg lbi = do
  identity <- catchIOError workingTreeIdentity (const (pure ""))
  withLibLBI pkg lbi $ \_ clbi -> do
    let output = getSymbolicPath (autogenComponentModulesDir lbi clbi) </> "Aihc/CompilerBuildIdentity.hs"
        content = "module Aihc.CompilerBuildIdentity (compilerBuildIdentity) where\n\ncompilerBuildIdentity :: String\ncompilerBuildIdentity = " <> show identity <> "\n"
    exists <- doesFileExist output
    unchanged <- if exists then (== content) <$> readFile' output else pure False
    unless unchanged $ do
      createDirectoryIfMissing True (takeDirectory output)
      writeFile output content

-- | The Git tree hash of the working tree: the committed sources with
-- every uncommitted change and untracked file that is not ignored. Two
-- checkouts with the same sources get the same identity; a checkout with
-- an uncommitted change gets its own, so the store never mixes artifacts
-- of two compilers built from one commit. The tree is written from a copy
-- of the index, which leaves the index and the working tree as they are.
workingTreeIdentity :: IO String
workingTreeIdentity = do
  topLevel <- git [] ["rev-parse", "--show-toplevel"]
  index <- git ["-C", topLevel] ["rev-parse", "--path-format=absolute", "--git-path", "index"]
  (temporaryIndex, handle) <- openBinaryTempFile (takeDirectory index) "index.aihc-identity"
  hClose handle
  copyFile index temporaryIndex
  ( do
      _ <- gitWithIndex temporaryIndex topLevel ["add", "--all", "--", "."]
      gitWithIndex temporaryIndex topLevel ["write-tree"]
    )
    `finally` removeFile temporaryIndex

git :: [String] -> [String] -> IO String
git global arguments = do
  (status, output, _) <- readProcessWithExitCode "git" (global <> arguments) ""
  unless (status == ExitSuccess) (ioError (userError ("git " <> unwords arguments <> " failed")))
  pure (takeWhile (/= '\n') output)

gitWithIndex :: FilePath -> FilePath -> [String] -> IO String
gitWithIndex index topLevel arguments = do
  environment <- getEnvironment
  let process =
        (proc "git" (["-C", topLevel] <> arguments))
          { env = Just (("GIT_INDEX_FILE", index) : filter ((/= "GIT_INDEX_FILE") . fst) environment)
          }
  (status, output, _) <- readCreateProcessWithExitCode process ""
  unless (status == ExitSuccess) (ioError (userError ("git " <> unwords arguments <> " failed")))
  pure (takeWhile (/= '\n') output)
