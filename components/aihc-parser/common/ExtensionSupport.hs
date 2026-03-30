{-# LANGUAGE OverloadedStrings #-}

module ExtensionSupport
  ( Expected (..),
    Outcome (..),
    ExtensionSpec (..),
    CaseMeta (..),
    fixtureDirFor,
    hasManifest,
    loadRegistry,
    loadManifest,
    classifyOutcome,
    finalizeOutcome,
  )
where

import qualified Aihc.Parser.Syntax as Syntax
import CppSupport (moduleHeaderExtensionSettings)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, takeDirectory, takeExtension, (</>))

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data ExtensionSpec = ExtensionSpec
  { extName :: !String,
    extFixtureDir :: !FilePath,
    extNotes :: !String
  }
  deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String,
    caseExtensions :: ![String]
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"

registryPath :: FilePath
registryPath = fixtureRoot </> "extensions.tsv"

fixtureDirFor :: ExtensionSpec -> FilePath
fixtureDirFor spec = fixtureRoot </> extFixtureDir spec

hasManifest :: ExtensionSpec -> IO Bool
hasManifest spec = do
  let dir = fixtureDirFor spec
  exists <- doesDirectoryExist dir
  if not exists
    then pure False
    else do
      files <- listFixtureFiles dir
      pure (not (null files))

loadRegistry :: IO [ExtensionSpec]
loadRegistry = do
  raw <- TIO.readFile registryPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRegistryRow rows

parseRegistryRow :: Text -> IO ExtensionSpec
parseRegistryRow row =
  case T.splitOn "\t" row of
    [nameTxt, dirTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = ""
          }
    [nameTxt, dirTxt, notesTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = T.unpack (T.strip notesTxt)
          }
    _ -> fail ("Invalid extension registry row (expected 2 or 3 tab-separated columns): " <> T.unpack row)

loadManifest :: ExtensionSpec -> IO [CaseMeta]
loadManifest spec = do
  let dir = fixtureDirFor spec
  files <- listFixtureFiles dir
  cases <- mapM (loadCaseMeta dir) files
  pure (sortOn casePath cases)

classifyOutcome :: Expected -> Bool -> Bool -> (Outcome, String)
classifyOutcome expected oracleOk roundtripOk =
  case expected of
    ExpectPass
      | not oracleOk -> (OutcomeFail, "oracle rejected pass case")
      | not roundtripOk -> (OutcomeFail, "roundtrip mismatch against oracle AST")
      | otherwise -> (OutcomePass, "")
    ExpectXFail
      | not oracleOk ->
          ( OutcomeFail,
            "oracle rejected xfail case (fixture invalid or missing oracle extension mapping)"
          )
      | roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
      | otherwise -> (OutcomeXFail, "")

finalizeOutcome :: CaseMeta -> Bool -> Bool -> (CaseMeta, Outcome, String)
finalizeOutcome meta oracleOk roundtripOk =
  let (outcome, details) = classifyOutcome (caseExpected meta) oracleOk roundtripOk
   in (meta, outcome, details)

stripComment :: Text -> Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles = go
  where
    go dir = do
      entries <- sort <$> listDirectory dir
      concat
        <$> mapM
          ( \entry -> do
              let path = dir </> entry
              isDir <- doesDirectoryExist path
              if isDir
                then go path
                else
                  if takeExtension path == ".hs"
                    then pure [path]
                    else pure []
          )
          entries

loadCaseMeta :: FilePath -> FilePath -> IO CaseMeta
loadCaseMeta root path = do
  source <- Utf8.readFile path
  (expected, reason) <- parseOracleTestBlock path source
  let relPath = makeRelative root path
      cid = dropExtension relPath
      categoryRaw = takeDirectory relPath
      category = if categoryRaw == "." then "fixture" else categoryRaw
  pure
    CaseMeta
      { caseId = cid,
        caseCategory = category,
        casePath = relPath,
        caseExpected = expected,
        caseReason = reason,
        caseExtensions = enabledExtensionNames source
      }

parseOracleTestBlock :: FilePath -> Text -> IO (Expected, String)
parseOracleTestBlock path source =
  case extractOracleBlock source of
    Nothing ->
      fail ("Fixture is missing an ORACLE_TEST block: " <> path)
    Just block ->
      case T.words block of
        ["pass"] -> pure (ExpectPass, "")
        ("xfail" : rest) ->
          let reason = trim (T.unpack (T.unwords rest))
           in if null reason
                then fail ("ORACLE_TEST xfail case requires a reason in " <> path)
                else pure (ExpectXFail, reason)
        _ ->
          fail
            ( "Invalid ORACLE_TEST block in "
                <> path
                <> " (expected `pass` or `xfail <reason>`): "
                <> T.unpack block
            )

extractOracleBlock :: Text -> Maybe Text
extractOracleBlock source = do
  remainder <- T.stripPrefix "{- ORACLE_TEST" (T.stripStart source)
  let (block, suffix) = T.breakOn "-}" remainder
  if T.null suffix
    then Nothing
    else Just (T.strip block)

enabledExtensionNames :: Text -> [String]
enabledExtensionNames =
  reverse
    . map (T.unpack . Syntax.extensionName)
    . foldl apply []
    . moduleHeaderExtensionSettings
  where
    apply acc setting =
      case setting of
        Syntax.EnableExtension ext -> ext : filter (/= ext) acc
        Syntax.DisableExtension ext -> filter (/= ext) acc
