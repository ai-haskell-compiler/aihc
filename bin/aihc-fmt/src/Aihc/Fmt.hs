{-# LANGUAGE OverloadedStrings #-}

module Aihc.Fmt
  ( FormatOptions (..),
    defaultFormatOptions,
    FormatError (..),
    formatErrorMessage,
    formatText,
  )
where

import Aihc.Fmt.Comment (CommentScanError, formatCommentScanError)
import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseModule,
  )
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Aihc.Parser.Token
  ( LexToken (..),
    LexTokenKind (..),
    TokenOrigin (..),
    lexModuleTokensWithSourceNameAndExtensions,
    readModuleHeaderPragmas,
  )
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

newtype FormatOptions = FormatOptions
  { formatExtensions :: [ExtensionSetting]
  }
  deriving (Eq, Show)

defaultFormatOptions :: FormatOptions
defaultFormatOptions = FormatOptions {formatExtensions = []}

data FormatError
  = OriginalParseFailure !FilePath !Text
  | FormattedParseFailure !FilePath !Text
  | SemanticMismatch !FilePath !String
  | CommentScanFailure !FilePath !CommentScanError
  | TokenStreamMismatch !FilePath
  | IdempotenceFailure !FilePath !Text !Text
  deriving (Eq, Show)

formatText :: FormatOptions -> FilePath -> Text -> Either FormatError Text
formatText opts sourceName input = do
  formatted <- formatTextOnce opts sourceName input
  formattedAgain <- formatTextOnce opts sourceName formatted
  if formattedAgain == formatted
    then Right formatted
    else Left (IdempotenceFailure sourceName formatted formattedAgain)

formatTextOnce :: FormatOptions -> FilePath -> Text -> Either FormatError Text
formatTextOnce opts sourceName input = do
  original <- parseInput OriginalParseFailure sourceName opts input
  rendered <- mergePrettyLayout opts sourceName input original
  let originalTokens = tokenFingerprint opts sourceName input
      renderedTokens = tokenFingerprint opts sourceName rendered
  if originalTokens == renderedTokens
    then pure ()
    else Left (TokenStreamMismatch sourceName)
  reparsed <- parseInput FormattedParseFailure sourceName opts rendered
  if normalizeModule original == normalizeModule reparsed
    then Right rendered
    else
      Left
        ( SemanticMismatch
            sourceName
            "formatted module reparsed to a different AST"
        )

parseInput :: (FilePath -> Text -> FormatError) -> FilePath -> FormatOptions -> Text -> Either FormatError Module
parseInput mkErr sourceName opts input =
  let cfg = parserConfig sourceName opts input
      (errs, modu) = parseModule cfg input
   in if null errs
        then Right modu
        else Left (mkErr sourceName (T.pack (formatParseErrors sourceName (Just input) errs)))

parserConfig :: FilePath -> FormatOptions -> Text -> ParserConfig
parserConfig sourceName opts input =
  defaultConfig
    { parserSourceName = sourceName,
      parserExtensions = formatBaseExtensions opts input
    }

formatBaseExtensions :: FormatOptions -> Text -> [Extension]
formatBaseExtensions opts input =
  effectiveExtensions edition (formatExtensions opts <> headerExtensionSettings headerPragmas)
  where
    headerPragmas = readModuleHeaderPragmas input
    defaultEdition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (formatExtensions opts))
    edition = fromMaybe defaultEdition (headerLanguageEdition headerPragmas)

normalizeModule :: Module -> Module
normalizeModule = stripAnnotations . addModuleParens

data TokenFingerprint = TokenFingerprint
  { tokenKind :: !LexTokenKind,
    tokenText :: !Text,
    tokenOrigin :: !TokenOrigin
  }
  deriving (Eq, Show)

tokenFingerprint :: FormatOptions -> FilePath -> Text -> [TokenFingerprint]
tokenFingerprint opts sourceName input =
  [ TokenFingerprint
      { tokenKind = lexTokenKind tok,
        tokenText = lexTokenText tok,
        tokenOrigin = lexTokenOrigin tok
      }
  | tok <- lexModuleTokensWithSourceNameAndExtensions sourceName (formatBaseExtensions opts input) input
  ]

mergePrettyLayout :: FormatOptions -> FilePath -> Text -> Module -> Either FormatError Text
mergePrettyLayout opts sourceName input modu = do
  aligned <- alignTokenStreams sourceName sourceTokensAll prettyTokensAll
  physical <- alignPhysicalSourceTokens sourceName prettyGaps aligned sourcePhysicalTokens
  Right (renderFromSourceTokens input physical)
  where
    exts = formatBaseExtensions opts input
    prettyInput = renderPretty modu
    sourceTokensAll = lexModuleTokensWithSourceNameAndExtensions sourceName exts input
    prettyTokensAll = lexModuleTokensWithSourceNameAndExtensions sourceName exts prettyInput
    sourcePhysicalTokens = physicalSourceTokens sourceTokensAll
    prettyGaps = tokenGaps prettyInput (physicalSourceTokens prettyTokensAll)

renderPretty :: (Pretty a) => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

physicalSourceTokens :: [LexToken] -> [LexToken]
physicalSourceTokens tokens =
  [ tok
  | tok@LexToken {lexTokenOrigin = FromSource, lexTokenKind = kind} <- tokens,
    kind /= TkEOF,
    isRealSpan (lexTokenSpan tok)
  ]

data PhysicalToken = PhysicalToken
  { physicalToken :: !LexToken,
    physicalPrettyGap :: !(Maybe Text)
  }
  deriving (Eq, Show)

alignTokenStreams :: FilePath -> [LexToken] -> [LexToken] -> Either FormatError [(LexToken, LexToken)]
alignTokenStreams sourceName source pretty' =
  go (semanticTokens source) (semanticTokens pretty')
  where
    go [] [] = Right []
    go (s : ss) (p : ps)
      | tokenSemanticsMatch s p = ((s, p) :) <$> go ss ps
      | otherwise = Left (TokenStreamMismatch sourceName)
    go _ _ = Left (TokenStreamMismatch sourceName)

semanticTokens :: [LexToken] -> [LexToken]
semanticTokens =
  filter
    ( \tok ->
        lexTokenKind tok /= TkEOF
          && not (isCommentToken tok)
    )

tokenSemanticsMatch :: LexToken -> LexToken -> Bool
tokenSemanticsMatch source prettyTok =
  lexTokenKind source == lexTokenKind prettyTok

alignPhysicalSourceTokens :: FilePath -> [(LexToken, Text)] -> [(LexToken, LexToken)] -> [LexToken] -> Either FormatError [PhysicalToken]
alignPhysicalSourceTokens sourceName prettyGaps =
  go
  where
    go _ [] = Right []
    go pairs (tok : toks)
      | isCommentToken tok =
          (PhysicalToken tok Nothing :) <$> go pairs toks
      | otherwise =
          case break ((== tok) . fst) pairs of
            (_, []) -> Left (TokenStreamMismatch sourceName)
            (_, (_, prettyTok) : restPairs) ->
              (PhysicalToken tok (prettyGapBefore prettyGaps prettyTok) :) <$> go restPairs toks

prettyGapBefore :: [(LexToken, Text)] -> LexToken -> Maybe Text
prettyGapBefore prettyGaps tok
  | lexTokenOrigin tok /= FromSource = Nothing
  | not (isRealSpan (lexTokenSpan tok)) = Nothing
  | otherwise = lookup tok prettyGaps

tokenGaps :: Text -> [LexToken] -> [(LexToken, Text)]
tokenGaps input =
  go 0
  where
    bytes = TE.encodeUtf8 input

    go _offset [] = []
    go offset (tok : rest) =
      let (start, end) = tokenOffsets tok
          gap = TE.decodeUtf8 (BS.take (start - offset) (BS.drop offset bytes))
       in (tok, gap) : go end rest

renderFromSourceTokens :: Text -> [PhysicalToken] -> Text
renderFromSourceTokens input tokens =
  TE.decodeUtf8 (go Nothing 0 tokens)
  where
    bytes = TE.encodeUtf8 input
    total = BS.length bytes

    go _previous offset [] = sliceBytes offset total
    go previous offset (PhysicalToken tok prettyGap : rest) =
      let (start, end) = tokenOffsets tok
          gap = TE.decodeUtf8 (sliceBytes offset start)
       in TE.encodeUtf8 (renderGap previous tok prettyGap gap)
            <> sliceBytes start end
            <> go (Just tok) end rest

    sliceBytes start end = BS.take (end - start) (BS.drop start bytes)

renderGap :: Maybe LexToken -> LexToken -> Maybe Text -> Text -> Text
renderGap Nothing _current _prettyGap originalGap = originalGap
renderGap (Just previous) current prettyGap originalGap
  | isCommentToken previous || isCommentToken current = originalGap
  | otherwise = fromMaybe originalGap prettyGap

isCommentToken :: LexToken -> Bool
isCommentToken tok =
  case lexTokenKind tok of
    TkLineComment -> True
    TkBlockComment -> True
    _ -> False

isRealSpan :: SourceSpan -> Bool
isRealSpan SourceSpan {} = True
isRealSpan NoSourceSpan = False

tokenOffsets :: LexToken -> (Int, Int)
tokenOffsets LexToken {lexTokenSpan = SourceSpan {sourceSpanStartOffset = start, sourceSpanEndOffset = end}} =
  (start, end)
tokenOffsets LexToken {lexTokenSpan = NoSourceSpan} =
  error "tokenOffsets: expected source token with real span"

formatErrorMessage :: FormatError -> Text
formatErrorMessage err =
  case err of
    OriginalParseFailure sourceName msg ->
      T.pack sourceName <> ": failed to parse input\n" <> msg
    FormattedParseFailure sourceName msg ->
      T.pack sourceName <> ": formatted output did not parse\n" <> msg
    SemanticMismatch sourceName msg ->
      T.pack sourceName <> ": semantic sanity check failed: " <> T.pack msg
    CommentScanFailure sourceName scanErr ->
      T.pack sourceName <> ": comment scan failed: " <> T.pack (formatCommentScanError scanErr)
    TokenStreamMismatch sourceName ->
      T.pack sourceName <> ": token stream sanity check failed"
    IdempotenceFailure sourceName _ _ ->
      T.pack sourceName <> ": idempotence sanity check failed"
