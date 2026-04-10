{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Lex.Quoted
  ( decodeStringBody,
    processMultilineString,
    readMaybeChar,
    scanMultilineString,
    scanQuoted,
  )
where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace, ord)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

-- | Scan a quoted string body (after the opening delimiter) until the
-- unescaped closing character.  Returns @Right (body, rest)@ on success or
-- @Left body@ if the input ends without a closing delimiter.
scanQuoted :: Char -> Text -> Either Text (Text, Text)
scanQuoted endCh input = go 0 input
  where
    go consumed rest =
      case T.findIndex (\c -> c == '\\' || c == endCh) rest of
        Nothing -> Left (T.take (consumed + T.length rest) input)
        Just i ->
          let consumed' = consumed + i
              special = T.drop i rest
           in if T.null special
                then Left (T.take consumed' input)
                else case T.head special of
                  c
                    | c == endCh -> Right (T.take consumed' input, T.drop 1 special)
                    | otherwise ->
                        case consumedEscapeTail (T.drop 1 special) of
                          Just tailLen -> go (consumed' + 1 + tailLen) (T.drop tailLen (T.drop 1 special))
                          Nothing -> Left (T.take (consumed' + 1) input)

-- | Scan a multiline string body (after the opening @\"\"\"@) until an
-- unescaped closing @\"\"\"@.  Returns @Right (body, rest)@ on success or
-- @Left body@ if the input ends without a closing delimiter.
scanMultilineString :: Text -> Either Text (Text, Text)
scanMultilineString input = go 0 input
  where
    go consumed rest =
      case T.findIndex (\c -> c == '\\' || c == '"') rest of
        Nothing -> Left (T.take (consumed + T.length rest) input)
        Just i ->
          let consumed' = consumed + i
              special = T.drop i rest
           in if T.null special
                then Left (T.take consumed' input)
                else case T.head special of
                  '\\' ->
                    case consumedEscapeTail (T.drop 1 special) of
                      Just tailLen -> go (consumed' + 1 + tailLen) (T.drop tailLen (T.drop 1 special))
                      Nothing -> Left (T.take (consumed' + 1) input)
                  '"' ->
                    if "\"\"\"" `T.isPrefixOf` special
                      then Right (T.take consumed' input, T.drop 3 special)
                      else
                        if "\"\"" `T.isPrefixOf` special
                          then go (consumed' + 2) (T.drop 2 special)
                          else go (consumed' + 1) (T.drop 1 special)
                  _ -> error "unreachable: findIndex only returns backslash or quote"

-- | Determine how much text after a backslash belongs to the current
-- escape-like sequence for delimiter scanning purposes.
--
-- We intentionally recognize string gaps here so a gap-closing backslash does
-- not incorrectly escape the following quote.
consumedEscapeTail :: Text -> Maybe Int
consumedEscapeTail rest =
  if T.null rest
    then Nothing
    else
      let c = T.head rest
       in if isSpace c
            then case T.findIndex (not . isSpace) rest of
              Just i | T.index rest i == '\\' -> Just (i + 1)
              _ -> Just 1
            else Just 1

-- | Decode the body of a Haskell string literal (content between the quotes,
-- without the surrounding @\"@ characters) natively on 'Text', avoiding the
-- round-trip through 'String'.  Returns 'Nothing' if the body contains an
-- invalid escape sequence, in which case the caller should fall back to the
-- raw body.
decodeStringBody :: Text -> Maybe Text
decodeStringBody inp
  | not ('\\' `T.elem` inp) = Just inp -- fast path: no escapes, no allocation
  | otherwise = TL.toStrict . TLB.toLazyText <$> go mempty inp
  where
    go :: TLB.Builder -> Text -> Maybe TLB.Builder
    go !acc t =
      let (plain, rest) = T.break (== '\\') t
          acc' = acc <> TLB.fromText plain
       in case T.uncons rest of
            Nothing -> Just acc'
            Just ('\\', after) -> case parseEscape after of
              Nothing -> Nothing
              Just (mc, rest') ->
                go (maybe acc' (\c -> acc' <> TLB.singleton c) mc) rest'
            _ -> Just acc' -- unreachable: T.break stops at '\\'
    parseEscape :: Text -> Maybe (Maybe Char, Text)
    parseEscape t = case T.uncons t of
      Nothing -> Nothing
      Just (c, rest) -> case c of
        'a' -> Just (Just '\a', rest)
        'b' -> Just (Just '\b', rest)
        'f' -> Just (Just '\f', rest)
        'n' -> Just (Just '\n', rest)
        'r' -> Just (Just '\r', rest)
        't' -> Just (Just '\t', rest)
        'v' -> Just (Just '\v', rest)
        '\\' -> Just (Just '\\', rest)
        '"' -> Just (Just '"', rest)
        '\'' -> Just (Just '\'', rest)
        '&' -> Just (Nothing, rest) -- empty escape
        '^' -> case T.uncons rest of -- control character \^X
          Just (cc, rest')
            | cc >= '@' && cc <= '_' ->
                Just (Just (chr (ord cc - 64)), rest')
          _ -> Nothing
        'x' ->
          -- hex escape \xNN  (use Integer to prevent Int overflow on long inputs)
          let (digits, rest') = T.span isHexDigit rest
           in if T.null digits
                then Nothing
                else
                  let n = T.foldl' (\a d -> a * 16 + toInteger (digitToInt d)) (0 :: Integer) digits
                   in if n > 0x10FFFF then Nothing else Just (Just (chr (fromIntegral n)), rest')
        'o' ->
          -- octal escape \oNN  (use Integer to prevent Int overflow on long inputs)
          let (digits, rest') = T.span isOctDigit rest
           in if T.null digits
                then Nothing
                else
                  let n = T.foldl' (\a d -> a * 8 + toInteger (digitToInt d)) (0 :: Integer) digits
                   in if n > 0x10FFFF then Nothing else Just (Just (chr (fromIntegral n)), rest')
        _
          | isDigit c -> -- decimal escape \NNN  (use Integer to prevent Int overflow on long inputs)
              let (moreDigits, rest') = T.span isDigit rest
                  n = T.foldl' (\a d -> a * 10 + toInteger (digitToInt d)) (toInteger (digitToInt c)) moreDigits
               in if n > 0x10FFFF then Nothing else Just (Just (chr (fromIntegral n)), rest')
          | isSpace c -> -- gap escape \ whitespace \
              let rest' = T.dropWhile isSpace rest
               in case T.uncons rest' of
                    Just ('\\', rest'') -> Just (Nothing, rest'')
                    _ -> Nothing
          | otherwise -> parseNamedEscape t

    parseNamedEscape :: Text -> Maybe (Maybe Char, Text)
    parseNamedEscape t = foldr tryMatch Nothing namedEscapeTable
      where
        tryMatch (name, ch) fallback =
          case T.stripPrefix name t of
            Just rest -> Just (Just ch, rest)
            Nothing -> fallback

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

-- Named ASCII escape sequences per the Haskell 2010 report.
-- SOH must appear before SO so the longest prefix wins.
namedEscapeTable :: [(Text, Char)]
namedEscapeTable =
  [ ("NUL", '\NUL'),
    ("SOH", '\SOH'), -- must precede SO
    ("STX", '\STX'),
    ("ETX", '\ETX'),
    ("EOT", '\EOT'),
    ("ENQ", '\ENQ'),
    ("ACK", '\ACK'),
    ("BEL", '\BEL'),
    ("BS", '\BS'),
    ("HT", '\HT'),
    ("LF", '\LF'),
    ("VT", '\VT'),
    ("FF", '\FF'),
    ("CR", '\CR'),
    ("SO", '\SO'),
    ("SI", '\SI'),
    ("DLE", '\DLE'),
    ("DC1", '\DC1'),
    ("DC2", '\DC2'),
    ("DC3", '\DC3'),
    ("DC4", '\DC4'),
    ("NAK", '\NAK'),
    ("SYN", '\SYN'),
    ("ETB", '\ETB'),
    ("CAN", '\CAN'),
    ("EM", '\EM'),
    ("SUB", '\SUB'),
    ("ESC", '\ESC'),
    ("FS", '\FS'),
    ("GS", '\GS'),
    ("RS", '\RS'),
    ("US", '\US'),
    ("SP", '\SP'),
    ("DEL", '\DEL')
  ]

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
