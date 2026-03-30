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

import Data.Char (isSpace)
import Data.List (dropWhileEnd, group, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

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
    caseReason :: !String
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
  assertUniqueCaseIds (extName spec) cases
  pure (sortOn caseId cases)

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
  (cid, cat, expected, reason) <- parseOracleTestBlock path source
  pure
    CaseMeta
      { caseId = cid,
        caseCategory = cat,
        casePath = makeRelative root path,
        caseExpected = expected,
        caseReason = reason
      }

parseOracleTestBlock :: FilePath -> Text -> IO (String, String, Expected, String)
parseOracleTestBlock path source =
  case extractOracleBlock source of
    Nothing ->
      fail ("Fixture is missing an ORACLE_TEST block: " <> path)
    Just block -> do
      fields <- mapM parseField (filter (not . T.null) (map T.strip (T.lines block)))
      let findValue key = [value | (field, value) <- fields, field == key]
          getRequired key =
            case findValue key of
              [value] -> pure value
              [] -> fail ("ORACLE_TEST in " <> path <> " is missing required field: " <> T.unpack key)
              _ -> fail ("ORACLE_TEST in " <> path <> " has duplicate field: " <> T.unpack key)
      cid <- getRequired "id"
      cat <- getRequired "category"
      expectedTxt <- getRequired "expected"
      expected <-
        case T.toLower expectedTxt of
          "pass" -> pure ExpectPass
          "xfail" -> pure ExpectXFail
          _ -> fail ("Unknown ORACLE_TEST expected value in " <> path <> ": " <> T.unpack expectedTxt)
      let reasonValues = findValue "reason"
          reason =
            case reasonValues of
              [] -> ""
              [value] -> trim (T.unpack value)
              _ -> ""
      case reasonValues of
        (_ : _ : _) ->
          fail ("ORACLE_TEST in " <> path <> " has duplicate field: reason")
        _ -> pure ()
      case expected of
        ExpectXFail
          | null reason ->
              fail ("ORACLE_TEST xfail case requires a reason in " <> path)
        _ -> pure ()
      pure (T.unpack cid, T.unpack cat, expected, reason)

extractOracleBlock :: Text -> Maybe Text
extractOracleBlock source = do
  remainder <- T.stripPrefix "{- ORACLE_TEST" (T.stripStart source)
  let (block, suffix) = T.breakOn "-}" remainder
  if T.null suffix
    then Nothing
    else Just (T.strip block)

parseField :: Text -> IO (Text, Text)
parseField line =
  case T.breakOn ":" line of
    (rawKey, rawValue)
      | T.null rawValue ->
          fail ("Invalid ORACLE_TEST line (expected key: value): " <> T.unpack line)
      | otherwise -> do
          let key = T.toLower (T.strip rawKey)
              value = T.strip (T.drop 1 rawValue)
              canonical =
                case key of
                  "id" -> Just "id"
                  "label" -> Just "id"
                  "category" -> Just "category"
                  "subset" -> Just "category"
                  "expected" -> Just "expected"
                  "status" -> Just "expected"
                  "reason" -> Just "reason"
                  "description" -> Just "reason"
                  _ -> Nothing
          case canonical of
            Nothing ->
              fail ("Unknown ORACLE_TEST field: " <> T.unpack rawKey)
            Just canonicalKey -> pure (canonicalKey, value)

assertUniqueCaseIds :: String -> [CaseMeta] -> IO ()
assertUniqueCaseIds suiteName cases =
  case duplicateIds of
    [] -> pure ()
    ids ->
      fail
        ( "Duplicate ORACLE_TEST ids in "
            <> suiteName
            <> ": "
            <> unwords ids
        )
  where
    duplicateIds =
      [cid | (cid : _ : _) <- group (sort (map caseId cases))]
