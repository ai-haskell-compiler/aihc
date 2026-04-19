{-# LANGUAGE OverloadedStrings #-}

module LexerGolden
  ( ExpectedStatus (..),
    Outcome (..),
    LexerCase (..),
    fixtureRoot,
    loadLexerCases,
    parseLexerCaseText,
    evaluateLexerCase,
    progressSummary,
  )
where

import Aihc.Parser.Lex
  ( LexToken (..),
    LexTokenKind (..),
    lexModuleTokensWithExtensions,
    lexTokensWithExtensions,
  )
import Aihc.Parser.Syntax (Extension, FloatType, parseExtensionName)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isDigit, isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf, sort)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Read (readMaybe)

data ExpectedStatus
  = StatusPass
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data LexerCase = LexerCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseIsModule :: !Bool,
    caseExtensions :: ![Extension],
    caseInput :: !Text,
    caseTokens :: ![LexTokenKind],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/lexer"

loadLexerCases :: IO [LexerCase]
loadLexerCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadLexerCase paths

loadLexerCase :: FilePath -> IO LexerCase
loadLexerCase path = do
  source <- Utf8.readFile path
  case parseLexerCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseLexerCaseText :: FilePath -> Text -> Either String LexerCase
parseLexerCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, tokenTexts, statusText, reasonText) <- parseYamlFixture path value
  exts <- mapM (parseFixtureExtensionName path) extNames
  toks <- mapM (parseTokenKind path) tokenTexts
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
      isModuleFixture = isModulePath relPath
  pure
    LexerCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseIsModule = isModuleFixture,
        caseExtensions = exts,
        caseInput = inputText,
        caseTokens = toks,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], Text, [Text], Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "lexer fixture" $ \obj -> do
        exts <- obj .: "extensions"
        inputText <- obj .: "input"
        tokenTexts <- obj .: "tokens"
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, inputText, tokenTexts, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid lexer fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateLexerCase :: LexerCase -> (Outcome, String)
evaluateLexerCase meta =
  let expectedKinds = caseTokens meta
      actualTokens =
        if caseIsModule meta
          then lexModuleTokensWithExtensions (caseExtensions meta) (caseInput meta)
          else lexTokensWithExtensions (caseExtensions meta) (caseInput meta)
      actualKinds = map lexTokenKind actualTokens
      tokenMatch = actualKinds == expectedKinds
   in case caseStatus meta of
        StatusPass
          | tokenMatch -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "expected successful lex with matching token kinds"
                  <> detailsSuffix actualKinds expectedKinds
              )
        StatusXFail
          | tokenMatch -> (OutcomeFail, "expected xfail (known failing bug), but tokens now match")
          | otherwise ->
              ( OutcomeXFail,
                "known bug still present"
                  <> detailsSuffix actualKinds expectedKinds
              )
        StatusXPass
          | tokenMatch -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass (known passing bug), but tokens no longer match")

progressSummary :: [(LexerCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

detailsSuffix :: [LexTokenKind] -> [LexTokenKind] -> String
detailsSuffix actual expected =
  if actual == expected
    then ""
    else " (expected=" <> show expected <> ", actual=" <> show actual <> ")"

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

parseFixtureExtensionName :: FilePath -> Text -> Either String Extension
parseFixtureExtensionName path name =
  case parseExtensionName name of
    Just ext -> Right ext
    Nothing -> Left ("Unknown lexer extension in " <> path <> ": " <> T.unpack name)

parseTokenKind :: FilePath -> Text -> Either String LexTokenKind
parseTokenKind path raw =
  case parseFloatTokenKind (T.strip raw) of
    Just parsed -> Right parsed
    Nothing -> case readMaybe (T.unpack (T.strip raw)) of
      Just parsed -> Right parsed
      Nothing -> Left ("Invalid token constructor in " <> path <> ": " <> T.unpack raw)

parseFloatTokenKind :: Text -> Maybe LexTokenKind
parseFloatTokenKind raw = do
  body <- T.stripPrefix "TkFloat " raw
  let (valueTxt, typeTxt0) = T.break isSpace body
      typeTxt = T.strip typeTxt0
  value <- parseDecimalRational valueTxt
  floatType <- readMaybe (T.unpack typeTxt) :: Maybe FloatType
  pure (TkFloat value floatType)

parseDecimalRational :: Text -> Maybe Rational
parseDecimalRational txt = do
  let (sign, unsigned) =
        case T.uncons txt of
          Just ('-', rest) -> (-1, rest)
          Just ('+', rest) -> (1, rest)
          _ -> (1, txt)
      (mantissaTxt, exponentTxt0) = T.break (`elem` ['e', 'E']) unsigned
      exponentTxt = T.drop 1 exponentTxt0
  mantissa <- parseMantissa mantissaTxt
  exponentValue <-
    if T.null exponentTxt0
      then Just 0
      else readMaybe (T.unpack exponentTxt)
  pure (applyExponent (fromInteger sign * mantissa) exponentValue)

parseMantissa :: Text -> Maybe Rational
parseMantissa txt = do
  let (whole, frac0) = T.break (== '.') txt
      frac = T.drop 1 frac0
  if T.null whole && T.null frac
    then Nothing
    else do
      wholeDigits <- parseDigitsAllowEmpty whole
      fracDigits <- parseDigitsAllowEmpty frac
      let fracScale = 10 ^ T.length frac
      pure ((wholeDigits * fracScale + fracDigits) % fracScale)

parseDigitsAllowEmpty :: Text -> Maybe Integer
parseDigitsAllowEmpty digits
  | T.null digits = Just 0
  | T.all isDigit digits = readMaybe (T.unpack digits)
  | otherwise = Nothing

applyExponent :: Rational -> Integer -> Rational
applyExponent value exponentValue
  | exponentValue >= 0 = value * fromInteger (10 ^ exponentValue)
  | otherwise = value / fromInteger (10 ^ negate exponentValue)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "lexer"
    dir -> dir

isModulePath :: FilePath -> Bool
isModulePath path =
  "module/" `isPrefixOf` path

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
