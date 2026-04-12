{-# LANGUAGE OverloadedStrings #-}

module Aihc.BaseApi.Extractor.Bindist
  ( PreparedBindist (..),
    downloadAndPrepareBindist,
    inferArchiveUrl,
    validateLocalCompilerVersion,
  )
where

import Control.Monad (unless, when)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    listDirectory,
    makeAbsolute,
  )
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode (..))

data PreparedBindist = PreparedBindist
  { preparedArchivePath :: !FilePath,
    preparedRootDir :: !FilePath,
    preparedLibdir :: !FilePath,
    preparedPackageDb :: !FilePath
  }
  deriving (Eq, Show)

downloadAndPrepareBindist :: FilePath -> Text -> Text -> Maybe Text -> IO PreparedBindist
downloadAndPrepareBindist cacheDir ghcVersion target archiveUrl = do
  createDirectoryIfMissing True cacheDir
  curl <- requireProgram "curl"
  tar <- requireProgram "tar"
  let archivePath = cacheDir </> T.unpack ("ghc-" <> ghcVersion <> "-" <> target <> ".tar.xz")
      unpackRoot = cacheDir </> T.unpack ("ghc-" <> ghcVersion <> "-" <> target <> "-unpacked")
      url = maybe (inferArchiveUrl ghcVersion target) id archiveUrl
  archiveExists <- doesFileExist archivePath
  unless archiveExists $
    runProgram curl ["-L", "--fail", "--output", archivePath, T.unpack url]
  unpackExists <- doesDirectoryExist unpackRoot
  unless unpackExists $ do
    createDirectoryIfMissing True unpackRoot
    runProgram tar ["-xf", archivePath, "-C", unpackRoot]
  libdir <- findLibdir unpackRoot
  packageDb <- findPackageDb libdir
  pure
    PreparedBindist
      { preparedArchivePath = archivePath,
        preparedRootDir = unpackRoot,
        preparedLibdir = libdir,
        preparedPackageDb = packageDb
      }

inferArchiveUrl :: Text -> Text -> Text
inferArchiveUrl ghcVersion target =
  "https://downloads.haskell.org/~ghc/"
    <> ghcVersion
    <> "/ghc-"
    <> ghcVersion
    <> "-"
    <> target
    <> ".tar.xz"

validateLocalCompilerVersion :: Text -> IO ()
validateLocalCompilerVersion requested = do
  ghc <- requireProgram "ghc"
  version <- fmap (T.strip . T.pack) (readProcess ghc ["--numeric-version"] "")
  when (version /= requested) $
    fail
      ( "requested GHC "
          <> T.unpack requested
          <> " but local compiler is "
          <> T.unpack version
          <> "; the extractor currently requires a matching local GHC to read interfaces safely"
      )

requireProgram :: FilePath -> IO FilePath
requireProgram name = do
  mPath <- findExecutable name
  maybe (fail ("required program not found: " <> name)) pure mPath

runProgram :: FilePath -> [String] -> IO ()
runProgram exe args = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode exe args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      fail
        ( unlines
            [ "command failed: " <> unwords (exe : args),
              "exit code: " <> show code,
              stderr
            ]
        )

findLibdir :: FilePath -> IO FilePath
findLibdir unpackRoot = do
  settingsFiles <- findFilesNamed "settings" unpackRoot
  case sortOn length settingsFiles of
    [] -> fail ("could not find GHC libdir under " <> unpackRoot)
    path : _ -> makeAbsolute (takeDirectory path)

findPackageDb :: FilePath -> IO FilePath
findPackageDb libdir = do
  let candidate = libdir </> "package.conf.d"
  exists <- doesDirectoryExist candidate
  if exists
    then makeAbsolute candidate
    else fail ("could not find package.conf.d under " <> libdir)

findFilesNamed :: FilePath -> FilePath -> IO [FilePath]
findFilesNamed target root = do
  entries <- listDirectory root
  fmap concat $
    mapM
      ( \entry -> do
          let path = root </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then findFilesNamed target path
            else pure [path | entry == target]
      )
      entries
