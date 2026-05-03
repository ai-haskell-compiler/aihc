{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Parser
  ( Directive (..),
    parseDirective,
    parseDirectiveBody,
    parseDefine,
    parseInclude,
    parseLineDirective,
    parseIdentifier,
    parseQuotedText,
    parseDefineParams,
    isIdentStart,
    isIdentChar,
    isOpChar,
  )
where

import Aihc.Cpp.Types (IncludeKind (..))
import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

data Directive
  = DirDefineObject !Text !Text
  | DirDefineFunction !Text ![Text] !Text
  | DirUndef !Text
  | DirInclude !IncludeKind !Text
  | DirIf !Text
  | DirIfDef !Text
  | DirIfNDef !Text
  | DirElif !Text
  | DirElse
  | DirEndIf
  | DirLine !Int !(Maybe FilePath)
  | DirPragmaOnce
  | DirWarning !Text
  | DirError !Text
  | DirUnsupported !Text

parseDirective :: Text -> Maybe Directive
parseDirective raw =
  let trimmed = T.stripStart raw
   in if "#" `T.isPrefixOf` trimmed
        then
          let body = T.stripStart (T.drop 1 trimmed)
           in case T.uncons body of
                Just (c, _) | isLetter c || isDigit c -> parseDirectiveBody body
                _ -> Nothing
        else Nothing

parseDirectiveBody :: Text -> Maybe Directive
parseDirectiveBody body =
  let (name, rest0) = T.span isIdentChar body
      rest = T.stripStart rest0
   in if T.null name
        then case T.uncons body of
          Just (c, _) | isDigit c -> parseLineDirective body
          _ -> Nothing
        else case name of
          "define" -> parseDefine rest
          "undef" -> DirUndef <$> parseIdentifier rest
          "include" -> parseInclude rest
          "if" -> Just (DirIf rest)
          "ifdef" -> DirIfDef <$> parseIdentifier rest
          "ifndef" -> DirIfNDef <$> parseIdentifier rest
          -- Keep `#isndef` as explicitly unsupported for diagnostics on common typo input.
          "isndef" -> Just (DirUnsupported "isndef")
          "elif" -> Just (DirElif rest)
          "elseif" -> Just (DirElif rest)
          "else" -> Just DirElse
          "endif" -> Just DirEndIf
          "line" -> parseLineDirective rest
          "pragma" -> parsePragma rest
          "warning" -> Just (DirWarning rest)
          "error" -> Just (DirError rest)
          _ -> Nothing

parseLineDirective :: Text -> Maybe Directive
parseLineDirective body =
  case TR.decimal body of
    Left _ -> Nothing
    Right (lineNumber, rest0) ->
      let rest = T.stripStart rest0
       in case parseQuotedText rest of
            Nothing -> Just (DirLine lineNumber Nothing)
            Just path -> Just (DirLine lineNumber (Just (T.unpack path)))

parsePragma :: Text -> Maybe Directive
parsePragma body =
  case T.words body of
    ["once"] -> Just DirPragmaOnce
    _ -> Nothing

parseDefine :: Text -> Maybe Directive
parseDefine rest = do
  let (name, rest0) = T.span isIdentChar rest
  if T.null name
    then Nothing
    else case T.uncons rest0 of
      Just ('(', afterOpen) ->
        let (params, restAfterParams) = parseDefineParams afterOpen
         in case params of
              Nothing -> Just (DirUnsupported "define-function-macro")
              Just names -> Just (DirDefineFunction name names (T.stripStart restAfterParams))
      _ -> Just (DirDefineObject name (T.stripStart rest0))

parseDefineParams :: Text -> (Maybe [Text], Text)
parseDefineParams input =
  let (inside, suffix) = T.breakOn ")" input
   in if T.null suffix
        then (Nothing, "")
        else
          let rawParams = T.splitOn "," inside
              params = map (T.takeWhile isIdentChar . T.strip) rawParams
           in if T.null (T.strip inside)
                then (Just [], T.drop 1 suffix)
                else
                  if any T.null params
                    then (Nothing, T.drop 1 suffix)
                    else (Just params, T.drop 1 suffix)

parseIdentifier :: Text -> Maybe Text
parseIdentifier txt =
  let ident = T.takeWhile isIdentChar (T.stripStart txt)
   in if T.null ident then Nothing else Just ident

parseInclude :: Text -> Maybe Directive
parseInclude txt =
  case T.uncons (T.stripStart txt) of
    Just ('"', rest) ->
      let (path, suffix) = T.breakOn "\"" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeLocal path)
    Just ('<', rest) ->
      let (path, suffix) = T.breakOn ">" rest
       in if T.null suffix then Nothing else Just (DirInclude IncludeSystem path)
    _ -> Nothing

parseQuotedText :: Text -> Maybe Text
parseQuotedText txt = do
  ('"', rest) <- T.uncons txt
  let (path, suffix) = T.breakOn "\"" rest
  if T.null suffix then Nothing else Just path

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isLetter c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || isAlphaNum c

isOpChar :: Char -> Bool
isOpChar c =
  c == '+'
    || c == '-'
    || c == '*'
    || c == '/'
    || c == '%'
    || c == '&'
    || c == '|'
    || c == '!'
    || c == '='
    || c == '<'
    || c == '>'
