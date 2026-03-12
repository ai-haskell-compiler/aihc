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

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
  ( LexToken (..),
    LexTokenKind,
    LexerExtension (..),
    lexTokensWithExtensions,
  )
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Read (readMaybe)

data ExpectedStatus
  = StatusPass
  | StatusFail
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
    caseExtensions :: ![LexerExtension],
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
  source <- TIO.readFile path
  case parseLexerCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseLexerCaseText :: FilePath -> Text -> Either String LexerCase
parseLexerCaseText path source = do
  sections <- parseSections path source
  extensionsText <- requiredSection path "extensions" sections
  inputText <- requiredSection path "input" sections
  tokensText <- requiredSection path "tokens" sections
  statusText <- requiredSection path "status" sections
  let reasonText = maybe "" T.unpack (M.lookup "reason" sections)
  exts <- parseExtensions path extensionsText
  toks <- parseReadSection path "tokens" tokensText
  status <- parseStatus path statusText
  reason <- validateReason path status reasonText
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    LexerCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseInput = inputText,
        caseTokens = toks,
        caseStatus = status,
        caseReason = reason
      }

evaluateLexerCase :: LexerCase -> (Outcome, String)
evaluateLexerCase meta =
  let expectedKinds = caseTokens meta
      actual = lexTokensWithExtensions (caseExtensions meta) (caseInput meta)
      actualKinds = fmap (map lexTokenKind) actual
      lexOk = either (const False) (const True) actual
      tokenMatch = actualKinds == Right expectedKinds
      lexFail = either (const True) (const False) actual
   in case caseStatus meta of
        StatusPass
          | tokenMatch -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "expected successful lex with matching token kinds"
                  <> detailsSuffix actualKinds expectedKinds
              )
        StatusFail
          | lexFail -> (OutcomePass, "")
          | otherwise -> (OutcomeFail, "expected lex failure but lexing succeeded")
        StatusXFail
          | lexFail -> (OutcomeXFail, "")
          | otherwise -> (OutcomeFail, "expected xfail (known failing bug), but lexing succeeded")
        StatusXPass
          | lexOk && tokenMatch -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass (known passing bug), but case no longer matches xpass expectation")

progressSummary :: [(LexerCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

detailsSuffix :: Either String [LexTokenKind] -> [LexTokenKind] -> String
detailsSuffix actual expected =
  case actual of
    Left err -> " (lexer error: " <> err <> ")"
    Right actualKinds ->
      if actualKinds == expected
        then ""
        else " (expected=" <> show expected <> ", actual=" <> show actualKinds <> ")"

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
              if takeExtension path == ".lexer"
                then pure [path]
                else pure []
      )
      entries

parseSections :: FilePath -> Text -> Either String (M.Map String Text)
parseSections path source =
  go (T.lines source) Nothing [] M.empty
  where
    go remaining currentHeader currentLines acc =
      case remaining of
        [] ->
          case currentHeader of
            Nothing ->
              Left ("Fixture has no sections: " <> path)
            Just name ->
              Right (M.insert name (renderSection currentLines) acc)
        line : rest ->
          case parseHeaderLine line of
            Just header ->
              case currentHeader of
                Nothing -> go rest (Just header) [] acc
                Just name ->
                  let acc' = M.insert name (renderSection currentLines) acc
                   in go rest (Just header) [] acc'
            Nothing ->
              case currentHeader of
                Nothing -> Left ("Fixture content appears before first section header in " <> path)
                Just _ -> go rest currentHeader (currentLines <> [line]) acc

parseHeaderLine :: Text -> Maybe String
parseHeaderLine line =
  let trimmed = T.strip line
   in case (T.uncons trimmed, T.unsnoc trimmed) of
        (Just ('[', _), Just (_, ']'))
          | T.length trimmed >= 2 ->
              let inner =
                    T.unpack
                      (T.strip (T.drop 1 (T.take (T.length trimmed - 1) trimmed)))
                  normalized = map toLower inner
               in if normalized `elem` ["extensions", "input", "tokens", "status", "reason"]
                    then Just normalized
                    else Nothing
        _ -> Nothing

renderSection :: [Text] -> Text
renderSection rows = T.dropWhileEnd isSpaceText (T.unlines rows)
  where
    isSpaceText ch = ch == '\n' || isSpace ch

requiredSection :: FilePath -> String -> M.Map String Text -> Either String Text
requiredSection path name sections =
  case M.lookup name sections of
    Just value -> Right value
    Nothing -> Left ("Missing [" <> name <> "] section in " <> path)

parseReadSection :: (Read a) => FilePath -> String -> Text -> Either String a
parseReadSection path sectionName raw =
  case readMaybe (T.unpack (T.strip raw)) of
    Just parsed -> Right parsed
    Nothing -> Left ("Invalid [" <> sectionName <> "] section in " <> path)

parseExtensions :: FilePath -> Text -> Either String [LexerExtension]
parseExtensions path raw =
  let trimmed = T.strip raw
   in if trimmed == "[]"
        then Right []
        else case T.stripPrefix "[" trimmed >>= T.stripSuffix "]" of
          Just body -> do
            let names = map T.strip (T.splitOn "," body)
            mapM (parseExtensionName path) (filter (not . T.null) names)
          _ -> Left ("Invalid [extensions] section in " <> path)

parseExtensionName :: FilePath -> Text -> Either String LexerExtension
parseExtensionName path name =
  case T.unpack name of
    "NegativeLiterals" -> Right NegativeLiterals
    other -> Left ("Unknown lexer extension in " <> path <> ": " <> other)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
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

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
