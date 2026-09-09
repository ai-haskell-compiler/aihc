{-# LANGUAGE OverloadedStrings #-}

-- | Metadata-only index of the shared evaluation fixtures.
--
-- The full fixture loader in @Aihc.Testing.EvalFixture@ needs the compiler to
-- parse, typecheck and desugar every fixture. Reporting extension coverage only
-- needs the declared extensions and the declared status of each fixture, so this
-- module reads that much and nothing else.
module Aihc.Testing.EvalFixtureIndex
  ( FixtureStatus (..),
    EvalFixtureEntry (..),
    evalFixtureRoot,
    loadEvalFixtureIndex,
    loadEvalFixtureIndexFrom,
    parseEvalFixtureEntry,
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
data EvalFixtureEntry = EvalFixtureEntry
  { entryPath :: !FilePath,
    entryExtensions :: ![Extension],
    entryStatus :: !FixtureStatus
  }
  deriving (Eq, Show)

-- | The fixture directory, from @AIHC_EVAL_FIXTURES@ or by searching upwards.
evalFixtureRoot :: IO FilePath
evalFixtureRoot = do
  configured <- lookupEnv "AIHC_EVAL_FIXTURES"
  maybe defaultEvalFixtureRoot pure configured

defaultEvalFixtureRoot :: IO FilePath
defaultEvalFixtureRoot = getCurrentDirectory >>= findUp
  where
    findUp dir = do
      let candidate = dir </> "test" </> "Test" </> "Fixtures" </> "eval"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure candidate
            else findUp parent

-- | Index every fixture under the default fixture root.
loadEvalFixtureIndex :: IO [EvalFixtureEntry]
loadEvalFixtureIndex = evalFixtureRoot >>= loadEvalFixtureIndexFrom

-- | Index every fixture under the given root.
loadEvalFixtureIndexFrom :: FilePath -> IO [EvalFixtureEntry]
loadEvalFixtureIndexFrom root = do
  exists <- doesDirectoryExist root
  if not exists
    then fail ("Shared eval fixture root does not exist: " <> root)
    else listFixtureFiles root >>= mapM (loadEntry root)

loadEntry :: FilePath -> FilePath -> IO EvalFixtureEntry
loadEntry root path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML eval fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseEvalFixtureEntry (makeRelative root path) value of
      Left err -> fail err
      Right entry -> pure entry

-- | Read the extensions and status of one fixture document.
parseEvalFixtureEntry :: FilePath -> Y.Value -> Either String EvalFixtureEntry
parseEvalFixtureEntry path value = do
  (extNames, statusText) <-
    parseEither
      ( withObject "eval fixture" $ \obj -> do
          exts <- obj .:? "extensions" .!= []
          status <- obj .: "status"
          pure (exts, status)
      )
      value
  extensions <- traverse (parseExtension path) extNames
  status <- parseStatus path statusText
  pure
    EvalFixtureEntry
      { entryPath = path,
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
