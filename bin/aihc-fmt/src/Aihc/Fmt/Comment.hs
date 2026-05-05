{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Fmt.Comment
  ( CommentKind (..),
    SourceComment (..),
    CommentScanError (..),
    collectComments,
    formatCommentScanError,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Data.Char (GeneralCategory (..), generalCategory, isAlphaNum, isSpace)
import Data.Text (Text, pattern Empty, pattern (:<))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data CommentKind
  = LineComment
  | BlockComment
  deriving (Eq, Ord, Show)

data SourceComment = SourceComment
  { commentSpan :: !SourceSpan,
    commentText :: !Text,
    commentKind :: !CommentKind,
    commentHasCodeBefore :: !Bool
  }
  deriving (Eq, Ord, Show)

data CommentScanError
  = UnterminatedBlockComment !SourceSpan
  | UnterminatedStringLiteral !SourceSpan
  | UnterminatedCharLiteral !SourceSpan
  deriving (Eq, Show)

formatCommentScanError :: CommentScanError -> String
formatCommentScanError err =
  case err of
    UnterminatedBlockComment sp -> "unterminated block comment at " <> formatSpan sp
    UnterminatedStringLiteral sp -> "unterminated string literal at " <> formatSpan sp
    UnterminatedCharLiteral sp -> "unterminated character literal at " <> formatSpan sp

collectComments :: FilePath -> Text -> Either CommentScanError [SourceComment]
collectComments sourceName input = go initialState []
  where
    initialState =
      ScanState
        { scanInput = input,
          scanLine = 1,
          scanCol = 1,
          scanOffset = 0,
          scanLineHasCode = False
        }

    go st acc =
      case scanInput st of
        Empty -> Right (reverse acc)
        chars
          | Just st' <- consumeWhitespace st ->
              go st' acc
          | "{-#" `T.isPrefixOf` chars,
            Just st' <- consumePragma st ->
              go st' acc
          | "{-" `T.isPrefixOf` chars ->
              case consumeNestedBlockComment sourceName st of
                Left err -> Left err
                Right (comment, st') -> go st' (comment : acc)
          | Just rest <- T.stripPrefix "--" chars,
            isLineCommentRest rest ->
              let (comment, st') = consumeLineComment sourceName st
               in go st' (comment : acc)
          | "\"" `T.isPrefixOf` chars ->
              case consumeQuoted sourceName '"' UnterminatedStringLiteral st of
                Left err -> Left err
                Right st' -> go (markCode st') acc
          | "'" `T.isPrefixOf` chars ->
              case consumeQuoted sourceName '\'' UnterminatedCharLiteral st of
                Left err -> Left err
                Right st' -> go (markCode st') acc
          | otherwise ->
              go (markCode (advanceOne st)) acc

data ScanState = ScanState
  { scanInput :: !Text,
    scanLine :: !Int,
    scanCol :: !Int,
    scanOffset :: !Int,
    scanLineHasCode :: !Bool
  }
  deriving (Eq, Show)

consumeWhitespace :: ScanState -> Maybe ScanState
consumeWhitespace st =
  case scanInput st of
    c :< _
      | isSpace c ->
          Just (advanceOne st)
    _ -> Nothing

consumePragma :: ScanState -> Maybe ScanState
consumePragma st =
  let (prefix, suffix) = T.breakOn "#-}" (scanInput st)
   in if T.null suffix
        then Nothing
        else Just (advanceText (prefix <> "#-}") st)

consumeLineComment :: FilePath -> ScanState -> (SourceComment, ScanState)
consumeLineComment sourceName st =
  let (raw, rest) = T.break (== '\n') (scanInput st)
      st' = advanceText raw st
      span' = mkScanSpan sourceName st st'
      comment =
        SourceComment
          { commentSpan = span',
            commentText = raw,
            commentKind = LineComment,
            commentHasCodeBefore = scanLineHasCode st
          }
   in (comment, st' {scanInput = rest})

consumeNestedBlockComment :: FilePath -> ScanState -> Either CommentScanError (SourceComment, ScanState)
consumeNestedBlockComment sourceName st =
  case scanBlock 0 0 (scanInput st) of
    Nothing -> Left (UnterminatedBlockComment (mkPointSpan sourceName st))
    Just len ->
      let raw = T.take len (scanInput st)
          st' = advanceText raw st
          comment =
            SourceComment
              { commentSpan = mkScanSpan sourceName st st',
                commentText = raw,
                commentKind = BlockComment,
                commentHasCodeBefore = scanLineHasCode st
              }
       in Right (comment, st')
  where
    scanBlock :: Int -> Int -> Text -> Maybe Int
    scanBlock depth n input =
      case T.uncons input of
        Nothing -> Nothing
        Just (c, rest1) ->
          case T.uncons rest1 of
            Just (c2, rest2)
              | c == '{' && c2 == '-' ->
                  scanBlock (depth + 1) (n + 2) rest2
              | c == '-' && c2 == '}' ->
                  if depth == 1
                    then Just (n + 2)
                    else scanBlock (depth - 1) (n + 2) rest2
            _ -> scanBlock depth (n + 1) rest1

consumeQuoted ::
  FilePath ->
  Char ->
  (SourceSpan -> CommentScanError) ->
  ScanState ->
  Either CommentScanError ScanState
consumeQuoted sourceName quote mkErr st =
  go False (advanceOne st)
  where
    go escaped current =
      case scanInput current of
        Empty -> Left (mkErr (mkPointSpan sourceName st))
        c :< _
          | escaped -> go False (advanceOne current)
          | c == '\\',
            quote == '"',
            Just afterGap <- consumeStringGap (advanceOne current) ->
              go False afterGap
          | c == '\\' -> go True (advanceOne current)
          | c == quote -> Right (advanceOne current)
          | c == '\n' -> Left (mkErr (mkPointSpan sourceName st))
          | otherwise -> go False (advanceOne current)

consumeStringGap :: ScanState -> Maybe ScanState
consumeStringGap = go False False
  where
    go seenWhitespace seenNewline st =
      case scanInput st of
        c :< _
          | c == '\\' && seenWhitespace && seenNewline -> Just (advanceOne st)
          | isSpace c -> go True (seenNewline || c == '\n') (advanceOne st)
        _ -> Nothing

advanceOne :: ScanState -> ScanState
advanceOne st =
  case T.uncons (scanInput st) of
    Nothing -> st
    Just (c, rest) ->
      let bytes = byteLength (T.singleton c)
       in if c == '\n'
            then
              st
                { scanInput = rest,
                  scanLine = scanLine st + 1,
                  scanCol = 1,
                  scanOffset = scanOffset st + bytes,
                  scanLineHasCode = False
                }
            else
              st
                { scanInput = rest,
                  scanCol = scanCol st + 1,
                  scanOffset = scanOffset st + bytes
                }

advanceText :: Text -> ScanState -> ScanState
advanceText txt st = T.foldl' (\acc _ -> advanceOne acc) st txt

markCode :: ScanState -> ScanState
markCode st = st {scanLineHasCode = True}

mkScanSpan :: FilePath -> ScanState -> ScanState -> SourceSpan
mkScanSpan sourceName start end =
  SourceSpan
    { sourceSpanSourceName = sourceName,
      sourceSpanStartLine = scanLine start,
      sourceSpanStartCol = scanCol start,
      sourceSpanEndLine = scanLine end,
      sourceSpanEndCol = scanCol end,
      sourceSpanStartOffset = scanOffset start,
      sourceSpanEndOffset = scanOffset end
    }

mkPointSpan :: FilePath -> ScanState -> SourceSpan
mkPointSpan sourceName st = mkScanSpan sourceName st st

formatSpan :: SourceSpan -> String
formatSpan sp =
  case sp of
    NoSourceSpan -> "<unknown>"
    SourceSpan {sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol} ->
      sourceSpanSourceName <> ":" <> show sourceSpanStartLine <> ":" <> show sourceSpanStartCol

byteLength :: Text -> Int
byteLength = T.length . TE.decodeLatin1 . TE.encodeUtf8

isLineCommentRest :: Text -> Bool
isLineCommentRest rest =
  case T.dropWhile (== '-') rest of
    Empty -> True
    c :< _ -> not (isSymbolicOpChar c)

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c =
  c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
    || (not (isAlphaNum c) && not (isSpace c) && isSymbolicCategory (generalCategory c))

isSymbolicCategory :: GeneralCategory -> Bool
isSymbolicCategory category =
  case category of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    DashPunctuation -> True
    OtherPunctuation -> True
    ConnectorPunctuation -> True
    _ -> False
