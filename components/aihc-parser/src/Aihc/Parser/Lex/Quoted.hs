{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Quoted
  ( processMultilineString,
    readMaybeChar,
    scanMultilineString,
    scanQuoted,
  )
where

import Data.Char (isSpace)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Text (Text, pattern Empty, pattern (:<))
import Data.Text qualified as T

scanQuoted :: Char -> Text -> Either Text (Text, Text)
scanQuoted endCh input = go 0 input
  where
    go !n remaining =
      case remaining of
        c :< rest
          | c == endCh -> Right (T.take n input, rest)
          | c == '\\' ->
              case rest of
                _ :< rest' -> go (n + 2) rest'
                Empty -> Left (T.take (n + 1) input)
          | otherwise -> go (n + 1) rest
        Empty -> Left (T.take n input)

scanMultilineString :: Text -> Either Text (Text, Text)
scanMultilineString input = go 0 input
  where
    go !n remaining =
      case remaining of
        '"' :< ('"' :< ('"' :< rest'')) ->
          Right (T.take n input, rest'')
        '\\' :< rest ->
          case rest of
            _ :< rest' -> go (n + 2) rest'
            Empty -> Left (T.take (n + 1) input)
        _ :< rest -> go (n + 1) rest
        Empty -> Left (T.take n input)

processMultilineString :: String -> String
processMultilineString =
  resolveEscapes
    . stripTrailingNewline
    . stripLeadingNewline
    . List.intercalate "\n"
    . map blankToEmpty
    . stripCommonIndent
    . map expandLeadingTabs
    . splitMultilineNewlines
    . collapseStringGaps

collapseStringGaps :: String -> String
collapseStringGaps [] = []
collapseStringGaps ('\\' : rest)
  | not (null ws), '\\' : rest'' <- rest' = collapseStringGaps rest''
  where
    (ws, rest') = span (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') rest
collapseStringGaps (c : rest) = c : collapseStringGaps rest

splitMultilineNewlines :: String -> [String]
splitMultilineNewlines = go []
  where
    go acc [] = [reverse acc]
    go acc ('\r' : '\n' : rest) = reverse acc : go [] rest
    go acc ('\r' : rest) = reverse acc : go [] rest
    go acc ('\n' : rest) = reverse acc : go [] rest
    go acc ('\f' : rest) = reverse acc : go [] rest
    go acc (c : rest) = go (c : acc) rest

expandLeadingTabs :: String -> String
expandLeadingTabs = go 0
  where
    go col ('\t' : rest) =
      let spaces = 8 - (col `mod` 8)
       in replicate spaces ' ' ++ go (col + spaces) rest
    go col (' ' : rest) = ' ' : go (col + 1) rest
    go _ rest = rest

stripCommonIndent :: [String] -> [String]
stripCommonIndent lns =
  case mapMaybe indentOf nonBlank of
    [] -> lns
    indents -> map (dropPrefix (minimum indents)) lns
  where
    nonBlank = filter (not . all isSpace) (drop 1 lns)
    indentOf s = Just (length (takeWhile isSpace s))
    dropPrefix = drop

blankToEmpty :: String -> String
blankToEmpty s
  | all isSpace s = ""
  | otherwise = s

stripLeadingNewline :: String -> String
stripLeadingNewline ('\n' : rest) = rest
stripLeadingNewline s = s

stripTrailingNewline :: String -> String
stripTrailingNewline s
  | not (null s) && last s == '\n' = init s
  | otherwise = s

resolveEscapes :: String -> String
resolveEscapes s =
  case reads ('"' : s ++ "\"") of
    [(str, "")] -> str
    _ -> s

readMaybeChar :: Text -> Maybe Char
readMaybeChar raw =
  case reads (T.unpack raw) of
    [(c, "")] -> Just c
    _ -> Nothing
