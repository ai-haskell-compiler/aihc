{-# LANGUAGE OverloadedStrings #-}

-- | Metadata-only index of the shared compiler test fixtures.
--
-- The full fixture loaders in @Aihc.Testing.EvalFixture@ and @FcGolden@ need the
-- compiler to parse, typecheck and desugar every fixture. Reporting extension
-- coverage only needs the declared extensions and the declared status of each
-- fixture, so this module reads that much and nothing else.
module Aihc.Testing.FixtureIndex
  ( FixtureSuite (..),
    FixtureStatus (..),
    FixtureEntry (..),
    suiteName,
    suiteRelativeRoot,
    suiteRootVariable,
    allFixtureSuites,
    fixtureSuiteRoot,
    loadFixtureIndex,
    loadFixtureSuiteFrom,
    parseFixtureEntry,
  )
where

import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

-- | A directory of fixtures that declares extensions and a status per file.
data FixtureSuite
  = -- | The shared source-to-runtime evaluation fixtures.
    EvalSuite
  | -- | The System FC desugaring golden fixtures.
    FcGoldenSuite
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every indexed suite.
allFixtureSuites :: [FixtureSuite]
allFixtureSuites = [minBound .. maxBound]

-- | How a suite is named in reports.
suiteName :: FixtureSuite -> String
suiteName EvalSuite = "eval"
suiteName FcGoldenSuite = "fc"

-- | Where a suite lives, relative to the repository root.
suiteRelativeRoot :: FixtureSuite -> FilePath
suiteRelativeRoot EvalSuite = "test" </> "Test" </> "Fixtures" </> "eval"
suiteRelativeRoot FcGoldenSuite =
  "bin" </> "aihc" </> "compiler" </> "fc" </> "test" </> "Test" </> "Fixtures" </> "golden"

-- | The environment variable that overrides a suite's root.
suiteRootVariable :: FixtureSuite -> String
suiteRootVariable EvalSuite = "AIHC_EVAL_FIXTURES"
suiteRootVariable FcGoldenSuite = "AIHC_FC_FIXTURES"

-- | The status a fixture declares for itself.
--
-- @Pass@ and @Fail@ are both working tests: @Fail@ asserts an expected
-- compilation or evaluation error. @XFail@ and @XPass@ mark known bugs.
data FixtureStatus
  = StatusPass
  | StatusFail
  | StatusXPass
  | StatusXFail
  deriving (Eq, Ord, Show)

-- | One fixture, reduced to what extension reporting needs.
data FixtureEntry = FixtureEntry
  { entrySuite :: !FixtureSuite,
    entryPath :: !FilePath,
    entryExtensions :: ![Extension],
    entryStatus :: !FixtureStatus
  }
  deriving (Eq, Show)

-- | A suite's fixture directory, from its environment variable or by searching
-- upwards for the repository root.
fixtureSuiteRoot :: FixtureSuite -> IO FilePath
fixtureSuiteRoot suite = do
  configured <- lookupEnv (suiteRootVariable suite)
  case configured of
    Just root -> pure root
    Nothing -> do
      repoRoot <- findRepositoryRoot suite
      pure (repoRoot </> suiteRelativeRoot suite)

-- | The nearest enclosing directory that holds the suite.
--
-- Falls back to the current directory so that a missing suite is reported
-- against a path the caller recognises.
findRepositoryRoot :: FixtureSuite -> IO FilePath
findRepositoryRoot suite = do
  start <- getCurrentDirectory
  findUp start start
  where
    findUp original dir = do
      exists <- doesDirectoryExist (dir </> suiteRelativeRoot suite)
      if exists
        then pure dir
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure original
            else findUp original parent

-- | Index every fixture of every suite.
loadFixtureIndex :: IO [FixtureEntry]
loadFixtureIndex =
  concat <$> mapM (\suite -> fixtureSuiteRoot suite >>= loadFixtureSuiteFrom suite) allFixtureSuites

-- | Index every fixture of one suite, rooted at the given directory.
loadFixtureSuiteFrom :: FixtureSuite -> FilePath -> IO [FixtureEntry]
loadFixtureSuiteFrom suite root = do
  exists <- doesDirectoryExist root
  if not exists
    then fail ("Fixture root for the " <> suiteName suite <> " suite does not exist: " <> root)
    else listFixtureFiles root >>= mapM (loadEntry suite root)

loadEntry :: FixtureSuite -> FilePath -> FilePath -> IO FixtureEntry
loadEntry suite root path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFixtureEntry suite (makeRelative root path) value of
      Left err -> fail err
      Right entry -> pure entry

-- | Read the extensions and status of one fixture document.
parseFixtureEntry :: FixtureSuite -> FilePath -> Y.Value -> Either String FixtureEntry
parseFixtureEntry suite path value = do
  (extNames, statusText) <-
    parseEither
      ( withObject "fixture" $ \obj -> do
          exts <- obj .:? "extensions" .!= []
          status <- obj .: "status"
          pure (exts, status)
      )
      value
  extensions <- traverse (parseExtension path) extNames
  status <- parseStatus path statusText
  pure
    FixtureEntry
      { entrySuite = suite,
        entryPath = path,
        entryExtensions = extensions,
        entryStatus = status
      }

parseExtension :: FilePath -> Text -> Either String Extension
parseExtension path raw =
  maybe (Left ("Unknown extension " <> show raw <> " in " <> path)) Right (parseExtensionName raw)

parseStatus :: FilePath -> Text -> Either String FixtureStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
