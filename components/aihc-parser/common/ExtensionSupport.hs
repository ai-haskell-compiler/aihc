{-# LANGUAGE OverloadedStrings #-}

module ExtensionSupport
  ( Expected (..),
    Outcome (..),
    CaseMeta (..),
    oracleFixtureRoot,
    caseSourcePath,
    loadOracleCases,
    classifyOutcome,
    finalizeOutcome,
    evaluateCaseFromFile,
    evaluateCaseText,
  )
where

import Aihc.Cpp (resultOutput)
import qualified Aihc.Parser.Syntax as Syntax
import CppSupport (moduleHeaderExtensionSettings, preprocessForParserWithoutIncludesIfEnabled)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import GhcOracle (oracleModuleAstFingerprint)
import ParserValidation (validateParser)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, takeDirectory, takeExtension, (</>))

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String,
    caseExtensions :: ![Syntax.ExtensionSetting]
  }
  deriving (Eq, Show)

oracleFixtureRoot :: FilePath
oracleFixtureRoot = "test/Test/Fixtures/oracle"

caseSourcePath :: CaseMeta -> FilePath
caseSourcePath meta = oracleFixtureRoot </> casePath meta

loadOracleCases :: IO [CaseMeta]
loadOracleCases = do
  files <- listFixtureFiles oracleFixtureRoot
  cases <- mapM (loadCaseMeta dir) files
  pure (sortOn casePath cases)
  where
    dir = oracleFixtureRoot

classifyOutcome :: Expected -> Maybe Text -> Maybe String -> (Outcome, String)
classifyOutcome _expected (Just oracleErr) _roundtripOk = (OutcomeFail, T.unpack oracleErr)
classifyOutcome ExpectPass Nothing (Just err) = (OutcomeFail, err)
classifyOutcome ExpectPass Nothing Nothing = (OutcomePass, "")
classifyOutcome ExpectXFail Nothing Just {} = (OutcomeXFail, "")
classifyOutcome ExpectXFail Nothing Nothing = (OutcomeXPass, "test case passed unexpectedly. Maybe update testcase from xfail to pass.")

finalizeOutcome :: CaseMeta -> Maybe Text -> Maybe String -> (CaseMeta, Outcome, String)
finalizeOutcome meta oracleOk roundtripOk =
  let (outcome, details) = classifyOutcome (caseExpected meta) oracleOk roundtripOk
   in (meta, outcome, details)

evaluateCaseFromFile :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCaseFromFile meta = do
  source <- Utf8.readFile (caseSourcePath meta)
  pure (evaluateCaseText meta source)

evaluateCaseText :: CaseMeta -> Text -> (CaseMeta, Outcome, String)
evaluateCaseText meta source =
  -- Use Haskell2010 as the base language for oracle tests, as these fixtures
  -- are meant to be valid Haskell2010 code (possibly with extensions)
  let exts = caseExtensions meta
      source' =
        resultOutput
          ( preprocessForParserWithoutIncludesIfEnabled
              (caseExtensions meta)
              []
              (casePath meta)
              source
          )
      oracleOk = either Just (const Nothing) (oracleModuleAstFingerprint (casePath meta) Syntax.Haskell2010Edition exts source')
      validationOk = fmap show (validateParser (casePath meta) Syntax.Haskell2010Edition exts source')
   in finalizeOutcome meta oracleOk validationOk

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
  let (expected, reason) = parseOracleTestBlock path source
      relPath = makeRelative root path
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
        caseExtensions = moduleHeaderExtensionSettings source
      }

parseOracleTestBlock :: FilePath -> Text -> (Expected, String)
parseOracleTestBlock path source =
  case extractOracleBlock source of
    Nothing ->
      error ("Fixture is missing an ORACLE_TEST block: " <> path)
    Just block ->
      case T.words block of
        ["pass"] -> (ExpectPass, "")
        ("xfail" : rest) ->
          let reason = trim (T.unpack (T.unwords rest))
           in if null reason
                then error ("ORACLE_TEST xfail case requires a reason in " <> path)
                else (ExpectXFail, reason)
        _ ->
          error
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
