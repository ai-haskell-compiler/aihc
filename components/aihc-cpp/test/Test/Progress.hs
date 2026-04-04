{-# LANGUAGE OverloadedStrings #-}

module Test.Progress
  ( CaseMeta (..),
    Outcome (..),
    evaluateCase,
    loadManifest,
    progressSummary,
  )
where

import Aihc.Cpp (Config (..), Diagnostic (..), IncludeRequest (..), Result (..), Severity (..), Step (..), defaultConfig, preprocess)
import qualified Control.Exception as E
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Preprocessor.Cpphs (BoolOptions (..), CpphsOptions (..), defaultCpphsOptions, runCpphs)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/progress"

manifestPath :: FilePath
manifestPath = fixtureRoot </> "manifest.tsv"

progressSummary :: [(CaseMeta, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  let sourcePath = fixtureRoot </> casePath meta
  source <- TIO.readFile sourcePath
  ours <- runOurs sourcePath source
  oracle <- runOracle sourcePath
  let (outcome, details) = classify (caseExpected meta) ours oracle
  pure (meta, outcome, details)

classify :: Expected -> Either String Text -> Either String Text -> (Outcome, String)
classify expected ours oracle =
  case expected of
    ExpectPass ->
      case (ours, oracle) of
        (Right oursOut, Right oracleOut) ->
          case compareLocatedOutput oursOut oracleOut of
            Nothing -> (OutcomePass, "")
            Just details -> (OutcomeFail, "preprocessed output differs from cpphs oracle: " <> details)
        (Left _, Left _) -> (OutcomePass, "")
        (Left oursErr, _) -> (OutcomeFail, "ours failed: " <> oursErr)
        (_, Left oracleErr) -> (OutcomeFail, "oracle failed: " <> oracleErr)
    ExpectXFail ->
      case (ours, oracle) of
        (Right oursOut, Right oracleOut) ->
          case compareLocatedOutput oursOut oracleOut of
            Nothing -> (OutcomeXPass, "expected xfail but now matches cpphs")
            Just _ -> (OutcomeXFail, "")
        (Left _, Left _) -> (OutcomeXPass, "expected xfail but now matches cpphs")
        _ -> (OutcomeXFail, "")

data LocatedLine = LocatedLine
  { locatedLineNo :: !Int,
    locatedFile :: !FilePath,
    locatedText :: !Text
  }
  deriving (Eq, Show)

compareLocatedOutput :: Text -> Text -> Maybe String
compareLocatedOutput oursOut oracleOut =
  firstDifference (toLocatedLines oursOut) (toLocatedLines oracleOut)

firstDifference :: [LocatedLine] -> [LocatedLine] -> Maybe String
firstDifference = go (1 :: Int)
  where
    go :: Int -> [LocatedLine] -> [LocatedLine] -> Maybe String
    go _ [] [] = Nothing
    go n (o : os) (r : rs)
      | o == r = go (n + 1) os rs
      | otherwise =
          Just
            ( "first mismatch at output record "
                <> show n
                <> ": ours="
                <> showLocated o
                <> ", oracle="
                <> showLocated r
            )
    go n [] (r : _) =
      Just
        ( "ours ended early at output record "
            <> show n
            <> ", oracle has "
            <> showLocated r
        )
    go n (o : _) [] =
      Just
        ( "oracle ended early at output record "
            <> show n
            <> ", ours has "
            <> showLocated o
        )

showLocated :: LocatedLine -> String
showLocated (LocatedLine ln fp txt) =
  "(" <> show ln <> ", " <> show fp <> ", " <> show (T.unpack txt) <> ")"

toLocatedLines :: Text -> [LocatedLine]
toLocatedLines = go 1 "<unknown>" . T.lines
  where
    go _ _ [] = []
    go lineNo filePath (line : rest) =
      case parseLinePragma line of
        Just (nextLineNo, mFilePath) ->
          let filePath' = fromMaybe filePath mFilePath
           in go nextLineNo filePath' rest
        Nothing ->
          LocatedLine lineNo filePath line : go (lineNo + 1) filePath rest

parseLinePragma :: Text -> Maybe (Int, Maybe FilePath)
parseLinePragma raw =
  let line = T.strip raw
   in case T.stripPrefix "#line" line of
        Nothing -> Nothing
        Just rest0 ->
          let rest1 = T.stripStart rest0
              (lineNoTxt, rest2) = T.span isDigit rest1
           in if T.null lineNoTxt
                then Nothing
                else case reads (T.unpack lineNoTxt) of
                  [(lineNo, "")] ->
                    let rest3 = T.stripStart rest2
                     in if T.null rest3
                          then Just (lineNo, Nothing)
                          else case T.uncons rest3 of
                            Just ('"', quoted) ->
                              let (filePath, suffix) = T.breakOn "\"" quoted
                               in if T.null suffix
                                    then Nothing
                                    else Just (lineNo, Just (T.unpack filePath))
                            _ -> Nothing
                  _ -> Nothing

runOurs :: FilePath -> Text -> IO (Either String Text)
runOurs sourcePath source = do
  result <- drive (preprocess defaultConfig {configInputFile = sourcePath} source)
  let errors = [diagMessage d | d <- resultDiagnostics result, diagSeverity d == Error]
  case errors of
    [] -> pure (Right (resultOutput result))
    (msg : _) -> pure (Left (T.unpack msg))
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = do
      let includeAbsPath = resolveIncludePath sourcePath req
      exists <- doesFileExist includeAbsPath
      content <- if exists then Just <$> TIO.readFile includeAbsPath else pure Nothing
      drive (k content)

resolveIncludePath :: FilePath -> IncludeRequest -> FilePath
resolveIncludePath rootPath req =
  includeBaseDir </> includePath req
  where
    includeFromDir = takeDirectory (includeFrom req)
    includeBaseDir =
      if null includeFromDir
        then takeDirectory rootPath
        else includeFromDir

runOracle :: FilePath -> IO (Either String Text)
runOracle sourcePath = do
  source <- TIO.readFile sourcePath
  let cpphsOptions =
        defaultCpphsOptions
          { boolopts =
              (boolopts defaultCpphsOptions)
                { stripC89 = True,
                  warnings = False
                }
          }
  oracleOut <-
    (E.try (runCpphs cpphsOptions sourcePath (T.unpack source)) :: IO (Either E.SomeException String))
  pure $
    case oracleOut of
      Right out -> Right (T.pack out)
      Left err -> Left ("cpphs failed: " <> show err)

loadManifest :: IO [CaseMeta]
loadManifest = do
  raw <- TIO.readFile manifestPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRow rows

stripComment :: Text -> Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

parseRow :: Text -> IO CaseMeta
parseRow row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] -> parseRowWithReason cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] -> parseRowWithReason cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 tab-separated columns): " <> T.unpack row)

parseRowWithReason :: Text -> Text -> Text -> Text -> Text -> IO CaseMeta
parseRowWithReason cid cat pathTxt expectedTxt reasonTxt = do
  let path = T.unpack pathTxt
  exists <- doesFileExist (fixtureRoot </> path)
  if not exists
    then fail ("Manifest references missing case file: " <> path)
    else do
      expected <-
        case expectedTxt of
          "pass" -> pure ExpectPass
          "xfail" -> pure ExpectXFail
          _ -> fail ("Unknown expected value in manifest: " <> T.unpack expectedTxt)
      let reason = trim (T.unpack reasonTxt)
      case expected of
        ExpectXFail | null reason -> fail ("xfail case requires reason: " <> T.unpack cid)
        _ -> pure ()
      pure
        CaseMeta
          { caseId = T.unpack cid,
            caseCategory = T.unpack cat,
            casePath = path,
            caseExpected = expected,
            caseReason = reason
          }

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
