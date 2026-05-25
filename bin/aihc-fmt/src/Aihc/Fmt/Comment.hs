{-# LANGUAGE OverloadedStrings #-}

module Aihc.Fmt.Comment
  ( CommentKind (..),
    SourceComment (..),
    CommentScanError (..),
    collectComments,
    formatCommentScanError,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Parser.Token
  ( LexToken (..),
    LexTokenKind (..),
    TokenOrigin (..),
    lexModuleTokensWithSourceNameAndExtensions,
  )
import Data.Text (Text)

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
collectComments sourceName input = go Nothing [] tokens
  where
    tokens = lexModuleTokensWithSourceNameAndExtensions sourceName [] input

    go _lineHasCode acc [] = Right (reverse acc)
    go lineHasCode acc (tok : rest) =
      case lexTokenKind tok of
        TkLineComment ->
          let comment = mkComment lineHasCode LineComment tok
           in go lineHasCode (comment : acc) rest
        TkBlockComment ->
          let comment = mkComment lineHasCode BlockComment tok
           in go lineHasCode (comment : acc) rest
        TkError "unterminated block comment" ->
          Left (UnterminatedBlockComment (lexTokenSpan tok))
        _
          | tokenCountsAsCode tok ->
              go (sourceSpanEndLineMaybe (lexTokenSpan tok)) acc rest
          | otherwise ->
              go lineHasCode acc rest

mkComment :: Maybe Int -> CommentKind -> LexToken -> SourceComment
mkComment lineHasCode kind tok =
  SourceComment
    { commentSpan = lexTokenSpan tok,
      commentText = lexTokenText tok,
      commentKind = kind,
      commentHasCodeBefore = lineHasCode == sourceSpanStartLineMaybe (lexTokenSpan tok)
    }

tokenCountsAsCode :: LexToken -> Bool
tokenCountsAsCode tok =
  case (lexTokenOrigin tok, lexTokenKind tok) of
    (FromSource, TkPragma _) -> False
    (FromSource, TkEOF) -> False
    (FromSource, TkError _) -> False
    (FromSource, _) -> True
    (InsertedLayout, _) -> False

sourceSpanStartLineMaybe :: SourceSpan -> Maybe Int
sourceSpanStartLineMaybe sp =
  case sp of
    SourceSpan {sourceSpanStartLine = line} -> Just line
    NoSourceSpan -> Nothing

sourceSpanEndLineMaybe :: SourceSpan -> Maybe Int
sourceSpanEndLineMaybe sp =
  case sp of
    SourceSpan {sourceSpanEndLine = line} -> Just line
    NoSourceSpan -> Nothing

formatSpan :: SourceSpan -> String
formatSpan sp =
  case sp of
    NoSourceSpan -> "<unknown>"
    SourceSpan {sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol} ->
      sourceSpanSourceName <> ":" <> show sourceSpanStartLine <> ":" <> show sourceSpanStartCol
