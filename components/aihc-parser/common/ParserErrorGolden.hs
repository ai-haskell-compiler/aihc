{-# LANGUAGE OverloadedStrings #-}

module ParserErrorGolden
  ( Outcome (..),
    ErrorMessageCase (..),
    fixtureRoot,
    loadErrorMessageCases,
    parseErrorMessageCaseText,
    evaluateErrorMessageCase,
    progressSummary,
  )
where

import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import qualified Aihc.Parser.Syntax as Syntax
import Data.Aeson ((.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import GhcOracle (oracleModuleAstFingerprint)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

data Outcome
  = OutcomePass
  | OutcomeFail
  deriving (Eq, Show)

data ErrorMessageCase = ErrorMessageCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseSource :: !Text,
    caseExpectedGhc :: !Text,
    caseExpectedAihc :: !Text
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/error-messages"

loadErrorMessageCases :: IO [ErrorMessageCase]
loadErrorMessageCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadErrorMessageCase paths

loadErrorMessageCase :: FilePath -> IO ErrorMessageCase
loadErrorMessageCase path = do
  source <- TIO.readFile path
  case parseErrorMessageCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseErrorMessageCaseText :: FilePath -> Text -> Either String ErrorMessageCase
parseErrorMessageCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (inputText, ghcText, aihcText) <- parseYamlFixture path value
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    ErrorMessageCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseSource = inputText,
        caseExpectedGhc = normalizeText ghcText,
        caseExpectedAihc = normalizeText aihcText
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String (Text, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "parser error fixture" $ \obj -> do
        case validateAllowedKeys path obj ["src", "ghc", "aihc"] of
          Left err -> fail err
          Right () -> pure ()
        inputText <- obj .: "src"
        ghcText <- obj .: "ghc"
        aihcText <- obj .: "aihc"
        pure (inputText, ghcText, aihcText)
    )
    value of
    Left err -> Left ("Invalid parser error fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateErrorMessageCase :: ErrorMessageCase -> (Outcome, String)
evaluateErrorMessageCase meta =
  case ghcMismatch meta of
    Just details -> (OutcomeFail, details)
    Nothing ->
      case renderAihcMessage meta of
        Left details -> (OutcomeFail, details)
        Right actual
          | actual == caseExpectedAihc meta -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "aihc error mismatch.\nEXPECTED:\n"
                  <> T.unpack (caseExpectedAihc meta)
                  <> "\nACTUAL:\n"
                  <> T.unpack actual
              )

ghcMismatch :: ErrorMessageCase -> Maybe String
ghcMismatch meta =
  case oracleModuleAstFingerprint sourceName Syntax.Haskell2010Edition [] (caseSource meta) of
    Right {} ->
      Just "expected GHC parse failure, but parser accepted the input."
    Left actual
      | normalizeText actual == caseExpectedGhc meta -> Nothing
      | otherwise ->
          Just
            ( "GHC error mismatch. expected="
                <> show (T.unpack (caseExpectedGhc meta))
                <> " actual="
                <> show (T.unpack (normalizeText actual))
            )

renderAihcMessage :: ErrorMessageCase -> Either String Text
renderAihcMessage meta =
  let (errs, _) = parseModule defaultConfig {parserSourceName = sourceName} (caseSource meta)
   in case errs of
        _ : _ -> Right (normalizeText (T.pack (formatParseErrors sourceName (Just (caseSource meta)) errs)))
        [] -> Left "aihc parser accepted the input"

progressSummary :: [(ErrorMessageCase, Outcome, String)] -> (Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

sourceName :: FilePath
sourceName = "test.hs"

normalizeText :: Text -> Text
normalizeText = normalizeLines . dropTrailingNewlines . normalizeLineEndings

normalizeLineEndings :: Text -> Text
normalizeLineEndings = T.replace "\r\n" "\n"

dropTrailingNewlines :: Text -> Text
dropTrailingNewlines = T.dropWhileEnd (`elem` ['\n', '\r'])

normalizeLines :: Text -> Text
normalizeLines = T.intercalate "\n" . map T.stripEnd . T.lines

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles root = do
  entries <- listDirectory root
  paths <- mapM visit (sort entries)
  pure (concat paths)
  where
    visit name
      | isHidden name = pure []
      | otherwise = do
          let path = root </> name
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else pure [path | isYamlFile path]

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix = makeRelative fixtureRoot

categoryFromPath :: FilePath -> String
categoryFromPath relPath =
  case takeDirectory relPath of
    "." -> "error-messages"
    dir -> dir

isYamlFile :: FilePath -> Bool
isYamlFile path =
  let ext = map toLower (takeExtension path)
   in ext == ".yaml" || ext == ".yml"

isHidden :: FilePath -> Bool
isHidden [] = False
isHidden (c : _) = c == '.'

validateAllowedKeys :: FilePath -> KeyMap.KeyMap v -> [Text] -> Either String ()
validateAllowedKeys path obj allowed =
  let allowedSet = sort allowed
      extras =
        sort
          [ Key.toText key
          | key <- KeyMap.keys obj,
            Key.toText key `notElem` allowedSet
          ]
   in case extras of
        [] -> Right ()
        _ -> Left ("Unexpected keys " <> show extras <> " in " <> path)
