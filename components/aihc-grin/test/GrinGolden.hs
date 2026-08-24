{-# LANGUAGE OverloadedStrings #-}

-- | Source-to-GRIN fixture support.
module GrinGolden
  ( GrinCase (..),
    Outcome (..),
    evaluateGrinCase,
    loadGrinCases,
  )
where

import Aihc.Fc2.TypeOf (typeEnvFromPrograms)
import Aihc.Grin (lintProgram, lowerProgram, renderProgram)
import Aihc.Parser.Syntax (Extension, parseExtensionName)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Fc2Golden qualified
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

data ExpectedStatus = StatusPass | StatusFail | StatusXPass | StatusXFail
  deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail
  deriving (Eq, Show)

data GrinCase = GrinCase
  { caseId :: !String,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus
  }
  deriving (Eq, Show)

loadGrinCases :: IO [GrinCase]
loadGrinCases = do
  root <- fixtureRoot
  paths <- listFixtureFiles root
  mapM (loadCase root) paths

evaluateGrinCase :: GrinCase -> (Outcome, String)
evaluateGrinCase fixture =
  case renderCase fixture of
    Left problem -> classifyFailure fixture problem
    Right actual -> classifySuccess fixture actual

renderCase :: GrinCase -> Either String String
renderCase fixture = do
  let fc2Case =
        Fc2Golden.Fc2Case
          { Fc2Golden.caseId = caseId fixture,
            Fc2Golden.casePath = caseId fixture,
            Fc2Golden.caseExtensions = caseExtensions fixture,
            Fc2Golden.caseModules = caseModules fixture,
            Fc2Golden.caseExpected = "",
            Fc2Golden.caseStatus = Fc2Golden.StatusPass,
            Fc2Golden.caseReason = ""
          }
  (allPrograms, targetPrograms) <- Fc2Golden.buildFc2CasePrograms fc2Case
  let types = typeEnvFromPrograms allPrograms
  lowered <- traverse (lowerProgram types) targetPrograms
  case concatMap lintProgram lowered of
    [] -> pure (trim (unlines (map renderProgram lowered)))
    errors -> Left ("GRIN lint error: " <> show errors)

loadCase :: FilePath -> FilePath -> IO GrinCase
loadCase root path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> fail ("Invalid GRIN fixture " <> path <> ": " <> Y.prettyPrintParseException problem)
    Right value -> either fail pure (parseFixture root path value)

parseFixture :: FilePath -> FilePath -> Y.Value -> Either String GrinCase
parseFixture root path value = do
  (extensionNames, source, modules, expected, statusText) <-
    parseEither
      ( withObject "GRIN fixture" $ \object ->
          (,,,,)
            <$> object .:? "extensions" .!= []
            <*> object .:? "source"
            <*> object .:? "modules"
            <*> object .:? "expected" .!= ""
            <*> object .: "status"
      )
      value
  extensions <- traverse parseExtension extensionNames
  status <- parseStatus statusText
  moduleTexts <-
    case (source, modules) of
      (Just one, Nothing) -> Right [one]
      (Nothing, Just many) -> Right many
      _ -> Left ("GRIN fixture must contain source or modules: " <> path)
  pure
    GrinCase
      { caseId = makeRelative root path,
        caseExtensions = extensions,
        caseModules = moduleTexts,
        caseExpected = trim (T.unpack expected),
        caseStatus = status
      }
  where
    parseExtension name = maybe (Left ("Unknown extension in " <> path <> ": " <> T.unpack name)) Right (parseExtensionName name)

parseStatus :: Text -> Either String ExpectedStatus
parseStatus value =
  case T.toLower value of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid GRIN fixture status: " <> T.unpack value)

classifySuccess :: GrinCase -> String -> (Outcome, String)
classifySuccess fixture actual =
  case caseStatus fixture of
    StatusPass
      | actual == caseExpected fixture -> (OutcomePass, "")
      | otherwise -> (OutcomeFail, "output mismatch\nexpected:\n" <> caseExpected fixture <> "\nactual:\n" <> actual)
    StatusFail -> (OutcomeFail, "expected failure")
    StatusXFail
      | actual == caseExpected fixture -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | actual == caseExpected fixture -> (OutcomeXPass, "")
      | otherwise -> (OutcomeFail, "expected xpass output")

classifyFailure :: GrinCase -> String -> (Outcome, String)
classifyFailure fixture problem =
  case caseStatus fixture of
    StatusPass -> (OutcomeFail, problem)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, problem)

fixtureRoot :: IO FilePath
fixtureRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "test" </> "Test" </> "Fixtures" </> "grin"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else
          let parent = takeDirectory directory
           in if parent == directory then fail "GRIN fixture root does not exist" else findUp parent

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles directory = do
  entries <- sort <$> listDirectory directory
  concat <$> traverse select entries
  where
    select entry = do
      let path = directory </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then listFixtureFiles path
        else pure [path | map toLower (takeExtension path) `elem` [".yaml", ".yml"]]

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
