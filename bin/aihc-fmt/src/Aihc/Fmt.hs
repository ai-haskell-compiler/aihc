{-# LANGUAGE OverloadedStrings #-}

module Aihc.Fmt
  ( FormatOptions (..),
    defaultFormatOptions,
    FormatError (..),
    formatErrorMessage,
    formatText,
  )
where

import Aihc.Fmt.Comment
import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseModule,
  )
import Aihc.Parser.Lex
  ( LexToken (..),
    LexTokenKind (..),
    lexModuleTokensWithSourceNameAndExtensions,
    readModuleHeaderPragmas,
  )
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Syntax
import Data.List (partition, unsnoc)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutPretty, (<+>))
import Prettyprinter.Render.String (renderString)

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
  comments <- firstCommentError (collectComments sourceName input)
  original <- parseInput OriginalParseFailure sourceName opts input
  let rendered = renderModuleWithComments sourceName input original comments
  reparsed <- parseInput FormattedParseFailure sourceName opts rendered
  if normalizeModule original == normalizeModule reparsed
    then Right rendered
    else
      Left
        ( SemanticMismatch
            sourceName
            "formatted module reparsed to a different AST"
        )
  where
    firstCommentError =
      either (Left . CommentScanFailure sourceName) Right

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
      parserExtensions = finalExts
    }
  where
    headerPragmas = readModuleHeaderPragmas input
    defaultEdition = fromMaybe Haskell2010Edition (editionFromExtensionSettings (formatExtensions opts))
    edition = fromMaybe defaultEdition (headerLanguageEdition headerPragmas)
    finalExts = effectiveExtensions edition (formatExtensions opts <> headerExtensionSettings headerPragmas)

normalizeModule :: Module -> Module
normalizeModule = stripAnnotations . addModuleParens

renderModuleWithComments :: FilePath -> Text -> Module -> [SourceComment] -> Text
renderModuleWithComments sourceName input modu comments =
  ensureTrailingNewline . T.unlines . placeComments comments $ sections
  where
    sections =
      languagePragmaSections sourceName input
        <> moduleHeadSections modu
        <> importSections modu
        <> declSections modu

data Section = Section
  { sectionSpan :: !(Maybe SourceSpan),
    sectionLines :: ![Text]
  }
  deriving (Eq, Show)

languagePragmaSections :: FilePath -> Text -> [Section]
languagePragmaSections sourceName input =
  case pragmaSpans of
    [] -> []
    spans ->
      [ Section
          { sectionSpan = Just (foldr1 mergeSourceSpans spans),
            sectionLines = map renderLanguagePragma settings
          }
      ]
  where
    tokens = lexModuleTokensWithSourceNameAndExtensions sourceName [] input
    pragmaSpans =
      [ sp
      | LexToken {lexTokenKind = TkPragma Pragma {pragmaType = PragmaLanguage {}}, lexTokenSpan = sp} <- tokens
      ]
    headerPragmas = readModuleHeaderPragmas input
    settings = headerExtensionSettings headerPragmas

renderLanguagePragma :: ExtensionSetting -> Text
renderLanguagePragma ext =
  T.pack . renderString . layoutPretty defaultLayoutOptions $
    "{-# LANGUAGE" <+> pretty (extensionSettingName ext) <+> "#-}"

moduleHeadSections :: Module -> [Section]
moduleHeadSections modu =
  case moduleHead modu of
    Nothing -> []
    Just head' ->
      singletonSection (annListSpan (moduleHeadAnns head')) (renderOnlyHeader modu)

importSections :: Module -> [Section]
importSections modu =
  [ Section
      { sectionSpan = annListSpan (importDeclAnns importDecl),
        sectionLines = textLines (renderModuleWithoutPreamble modu {moduleHead = Nothing, moduleImports = [importDecl], moduleDecls = []})
      }
  | importDecl <- moduleImports modu
  ]

declSections :: Module -> [Section]
declSections modu =
  [ Section
      { sectionSpan = Just (getDeclSourceSpan decl),
        sectionLines = textLines (renderPretty decl)
      }
  | decl <- moduleDecls modu
  ]

singletonSection :: Maybe SourceSpan -> Text -> [Section]
singletonSection _ "" = []
singletonSection sp txt = [Section sp (textLines txt)]

renderOnlyHeader :: Module -> Text
renderOnlyHeader modu =
  renderModuleWithoutPreamble
    modu
      { moduleLanguagePragmas = [],
        moduleImports = [],
        moduleDecls = []
      }

renderModuleWithoutPreamble :: Module -> Text
renderModuleWithoutPreamble modu =
  renderPretty modu {moduleLanguagePragmas = []}

renderPretty :: (Pretty a) => a -> Text
renderPretty =
  T.pack . renderString . layoutPretty defaultLayoutOptions . pretty

textLines :: Text -> [Text]
textLines txt = filter (not . T.null) (T.lines txt)

annListSpan :: [Annotation] -> Maybe SourceSpan
annListSpan = firstRealSpan . mapMaybe fromAnnotation

firstRealSpan :: [SourceSpan] -> Maybe SourceSpan
firstRealSpan spans =
  case filter (/= NoSourceSpan) spans of
    [] -> Nothing
    sp : _ -> Just sp

placeComments :: [SourceComment] -> [Section] -> [Text]
placeComments comments [] =
  concatMap renderStandaloneComment comments
placeComments comments (section : rest) =
  let (sameLine, notSameLine) = partition (commentFollowsSection section) comments
      (before, after) = partition (commentPrecedesSection section) notSameLine
      current = renderSection section before sameLine
   in current <> placeComments after rest

renderSection :: Section -> [SourceComment] -> [SourceComment] -> [Text]
renderSection section leading trailing =
  concatMap renderStandaloneComment leading
    <> appendTrailingComments (sectionLines section) trailing

appendTrailingComments :: [Text] -> [SourceComment] -> [Text]
appendTrailingComments [] comments = concatMap renderStandaloneComment comments
appendTrailingComments lines0 [] = lines0
appendTrailingComments lines0 comments =
  let rendered = T.intercalate " " (map commentText comments)
   in case unsnoc lines0 of
        Nothing -> concatMap renderStandaloneComment comments
        Just (prefix, lastLine) -> prefix <> [lastLine <> " " <> rendered]

renderStandaloneComment :: SourceComment -> [Text]
renderStandaloneComment comment =
  T.lines (commentText comment)

commentPrecedesSection :: Section -> SourceComment -> Bool
commentPrecedesSection section comment =
  case sectionSpan section of
    Nothing -> False
    Just sp -> sourceSpanEndLine (commentSpan comment) < sourceSpanStartLine sp

commentFollowsSection :: Section -> SourceComment -> Bool
commentFollowsSection section comment =
  case sectionSpan section of
    Nothing -> False
    Just sp ->
      commentHasCodeBefore comment
        && sourceSpanStartLine (commentSpan comment) == sourceSpanEndLine sp

ensureTrailingNewline :: Text -> Text
ensureTrailingNewline txt
  | T.null txt = ""
  | "\n" `T.isSuffixOf` txt = txt
  | otherwise = txt <> "\n"

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
    IdempotenceFailure sourceName _ _ ->
      T.pack sourceName <> ": idempotence sanity check failed"
