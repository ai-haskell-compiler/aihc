-- |
-- Module      : Aihc.PackagePlan.Toml
-- Description : A reader and a writer for a subset of TOML
--
-- The lock file is TOML. This module reads and writes the part of TOML 1.0
-- that the lock file and similar configuration files need:
--
-- * key/value pairs with bare, quoted and dotted keys;
-- * @[table]@ headers;
-- * basic and literal strings on one line, decimal integers and booleans;
-- * arrays (on one line or on many lines, with a trailing comma) and inline
--   tables;
-- * comments.
--
-- The reader rejects the other parts of TOML with an error: multi-line
-- strings, floats, dates and times, integers that are not decimal, and
-- arrays of tables (@[[table]]@).
--
-- The lock file of format 1 was JSON. 'parseJson' reads JSON into the same
-- 'Value' type, so that one decoder reads the two formats.
module Aihc.PackagePlan.Toml
  ( Value (..),
    Table,
    parseToml,
    parseJson,
    renderKey,
    renderValue,
    renderInlineTable,
  )
where

import Control.Monad (foldM, unless, void, when)
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Numeric (readHex, showHex)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, string)

data Value
  = String !Text
  | Integer !Integer
  | Bool !Bool
  | Array ![Value]
  | Table !Table
  deriving (Eq, Show)

type Table = Map Text Value

type Parser = Parsec Void Text

-- | One line of a document after the parse, before the keys are put into
-- tables.
data Line
  = HeaderLine [Text]
  | PairLine [Text] Value

-- | Read a document. The file path is only for the error messages.
parseToml :: FilePath -> Text -> Either String Table
parseToml path source = do
  documentLines <- either (Left . errorBundlePretty) Right (parse document path source)
  (_, root, _) <- foldM addLine ([], Map.empty, Set.empty) documentLines
  pure root
  where
    addLine :: ([Text], Table, Set [Text]) -> Line -> Either String ([Text], Table, Set [Text])
    addLine (current, root, headers) line =
      case line of
        HeaderLine header -> do
          when (header `Set.member` headers) $
            Left ("the table " <> renderDottedKey header <> " is defined two times")
          root' <- ensureTable header root
          pure (header, root', Set.insert header headers)
        PairLine key value -> do
          root' <- insertAt (current <> key) value root
          pure (current, root', headers)

-- | Put a value at a dotted key. Each part of the key before the last
-- names a table, which the function makes if it does not exist.
insertAt :: [Text] -> Value -> Table -> Either String Table
insertAt key value = go key
  where
    go parts table =
      case parts of
        [] -> Left "an empty key"
        [name] ->
          case Map.lookup name table of
            Nothing -> Right (Map.insert name value table)
            Just _ -> Left ("the key " <> renderDottedKey key <> " is defined two times")
        name : rest ->
          case Map.findWithDefault (Table Map.empty) name table of
            Table inner -> do
              inner' <- go rest inner
              Right (Map.insert name (Table inner') table)
            _ -> Left ("the key " <> renderDottedKey key <> " is in a value that is not a table")

-- | Make sure that the tables of a header exist.
ensureTable :: [Text] -> Table -> Either String Table
ensureTable header = go header
  where
    go parts table =
      case parts of
        [] -> Right table
        name : rest ->
          case Map.findWithDefault (Table Map.empty) name table of
            Table inner -> do
              inner' <- go rest inner
              Right (Map.insert name (Table inner') table)
            _ -> Left ("the table " <> renderDottedKey header <> " has the name of a value that is not a table")

renderDottedKey :: [Text] -> String
renderDottedKey = intercalate "." . map (T.unpack . renderKey)

document :: Parser [Line]
document = do
  documentLines <- manyTill (lineContent <* lineEnd) eof
  pure (catMaybes documentLines)
  where
    lineContent = do
      spaces
      content <- optional (header <|> pair)
      spaces
      _ <- optional comment
      pure content
    lineEnd = void eol <|> lookAhead eof
    header = do
      _ <- char '['
      notFollowedBy (char '[') <?> "a table header, as arrays of tables are not supported"
      spaces
      name <- dottedKey
      spaces
      _ <- char ']'
      pure (HeaderLine name)
    pair = uncurry PairLine <$> keyValue

keyValue :: Parser ([Text], Value)
keyValue = do
  name <- dottedKey
  spaces
  _ <- char '='
  spaces
  content <- tomlValue
  pure (name, content)

dottedKey :: Parser [Text]
dottedKey = sepBy1 simpleKey (try (spaces *> char '.') *> spaces)

simpleKey :: Parser Text
simpleKey = bareKey <|> basicString <|> literalString <?> "a key"

bareKey :: Parser Text
bareKey = takeWhile1P (Just "a bare key character") isBareKeyChar

isBareKeyChar :: Char -> Bool
isBareKeyChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '-'

tomlValue :: Parser Value
tomlValue =
  choice
    [ String <$> basicString,
      String <$> literalString,
      Bool True <$ string "true",
      Bool False <$ string "false",
      Array <$> array,
      Table <$> inlineTable,
      Integer <$> integer
    ]
    <?> "a value"

basicString :: Parser Text
basicString = do
  _ <- char '"'
  notFollowedBy (string "\"\"") <?> "a string on one line, as multi-line strings are not supported"
  content <- many (escape <|> satisfy plain)
  _ <- char '"' <?> "the end of the string"
  pure (T.pack content)
  where
    plain c = c /= '"' && c /= '\\' && not (isControl c)
    escape = do
      _ <- char '\\'
      choice
        [ '\b' <$ char 'b',
          '\t' <$ char 't',
          '\n' <$ char 'n',
          '\f' <$ char 'f',
          '\r' <$ char 'r',
          '"' <$ char '"',
          '\\' <$ char '\\',
          char 'u' *> codePoint 4,
          char 'U' *> codePoint 8
        ]
        <?> "an escape sequence"
    codePoint digits = do
      hex <- count digits (satisfy isHexDigit)
      case readHex hex of
        [(point, "")]
          | point <= 0x10FFFF && (point < 0xD800 || point > 0xDFFF) -> pure (chr point)
        _ -> fail ("the code point " <> hex <> " is not a Unicode scalar value")

literalString :: Parser Text
literalString = do
  _ <- char '\''
  notFollowedBy (string "''") <?> "a string on one line, as multi-line strings are not supported"
  content <- takeWhileP (Just "a string character") (\c -> c /= '\'' && (c == '\t' || not (isControl c)))
  _ <- char '\'' <?> "the end of the string"
  pure content

-- | A decimal integer, with an optional sign and with underscores between
-- digits. A leading zero is not permitted.
integer :: Parser Integer
integer = do
  sign <- optional (char '+' <|> char '-')
  first <- satisfy isDigit <?> "a digit"
  rest <-
    if first == '0'
      then pure []
      else many (optional (char '_') *> satisfy isDigit)
  notFollowedBy (satisfy (\c -> isBareKeyChar c || c == '.' || c == ':'))
    <?> "the end of the integer, as only decimal integers are supported"
  let magnitude = read (first : rest)
  pure (if sign == Just '-' then negate magnitude else magnitude)

-- | Spaces, comments and line ends are permitted between the items.
array :: Parser [Value]
array = do
  _ <- char '['
  arraySpace
  items <- many (tomlValue <* arraySpace <* (void (char ',') <|> lookAhead (void (char ']'))) <* arraySpace)
  _ <- char ']' <?> "the end of the array"
  pure items
  where
    arraySpace = skipMany (void (takeWhile1P Nothing isSpaceChar) <|> comment <|> void eol)

-- | Inline tables are on one line and have no comma after the last pair.
inlineTable :: Parser Table
inlineTable = do
  _ <- char '{'
  spaces
  pairs <- sepBy (keyValue <* spaces) (char ',' *> spaces)
  _ <- char '}' <?> "the end of the inline table"
  either fail pure (foldM (\table (name, content) -> insertAt name content table) Map.empty pairs)

comment :: Parser ()
comment = do
  _ <- char '#'
  void (takeWhileP (Just "a comment character") (\c -> c == '\t' || not (isControl c)))

spaces :: Parser ()
spaces = void (takeWhileP Nothing isSpaceChar)

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t'

isControl :: Char -> Bool
isControl c = c < ' ' || c == '\DEL'

-- | Read a JSON document into a 'Value'. An object becomes a table. The
-- reader rejects @null@ and numbers that are not integers, because 'Value'
-- has no form for them. The file path is only for the error messages.
parseJson :: FilePath -> Text -> Either String Value
parseJson path = either (Left . errorBundlePretty) Right . parse (jsonSpace *> jsonValue <* eof) path
  where
    jsonValue :: Parser Value
    jsonValue =
      choice
        [ String <$> jsonString,
          Bool True <$ string "true",
          Bool False <$ string "false",
          Array <$> between (char '[' *> jsonSpace) (char ']') (sepBy (jsonValue <* jsonSpace) (char ',' *> jsonSpace)),
          Table <$> jsonObject,
          Integer <$> jsonInteger
        ]
        <* jsonSpace
        <?> "a JSON value"
    jsonObject = do
      _ <- char '{' *> jsonSpace
      pairs <- sepBy ((,) <$> (jsonString <* jsonSpace <* char ':' <* jsonSpace) <*> jsonValue) (char ',' *> jsonSpace)
      _ <- char '}' <?> "the end of the object"
      let table = Map.fromList pairs
      when (Map.size table /= length pairs) (fail "an object has the same key two times")
      pure table
    jsonInteger = do
      sign <- optional (char '-')
      digits <- (pure <$> char '0') <|> ((:) <$> satisfy (\c -> isDigit c && c /= '0') <*> many (satisfy isDigit))
      notFollowedBy (satisfy (`elem` (".eE" :: String))) <?> "the end of the integer, as only integers are supported"
      let magnitude = read digits
      pure (if sign == Just '-' then negate magnitude else magnitude)
    jsonString = do
      _ <- char '"'
      content <- many (jsonEscape <|> satisfy (\c -> c /= '"' && c /= '\\' && c >= ' '))
      _ <- char '"' <?> "the end of the string"
      pure (T.pack content)
    jsonEscape = do
      _ <- char '\\'
      choice
        [ '\b' <$ char 'b',
          '\t' <$ char 't',
          '\n' <$ char 'n',
          '\f' <$ char 'f',
          '\r' <$ char 'r',
          '"' <$ char '"',
          '\\' <$ char '\\',
          '/' <$ char '/',
          char 'u' *> unicodeEscape
        ]
        <?> "an escape sequence"
    -- A code point outside the Basic Multilingual Plane is a surrogate pair.
    unicodeEscape = do
      high <- hexUnit
      if high >= 0xD800 && high <= 0xDBFF
        then do
          low <- string "\\u" *> hexUnit
          unless (low >= 0xDC00 && low <= 0xDFFF) (fail "a high surrogate without a low surrogate")
          pure (chr (0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)))
        else do
          when (high >= 0xDC00 && high <= 0xDFFF) (fail "a low surrogate without a high surrogate")
          pure (chr high)
    hexUnit = do
      hex <- count 4 (satisfy isHexDigit)
      case readHex hex of
        [(unit, "")] -> pure unit
        _ -> fail ("the escape " <> hex <> " is not hexadecimal")
    jsonSpace = void (takeWhileP Nothing (`elem` (" \t\r\n" :: String)))

-- | A key, bare if it can be, as a basic string if not.
renderKey :: Text -> Text
renderKey name
  | not (T.null name), T.all isBareKeyChar name = name
  | otherwise = renderString name

-- | A value on one line. The pairs of a table come in key order.
renderValue :: Value -> Text
renderValue content =
  case content of
    String text -> renderString text
    Integer number -> T.pack (show number)
    Bool True -> "true"
    Bool False -> "false"
    Array items -> "[" <> T.intercalate ", " (map renderValue items) <> "]"
    Table table -> renderInlineTable (Map.toAscList table)

-- | An inline table with its pairs in the given order.
renderInlineTable :: [(Text, Value)] -> Text
renderInlineTable pairs =
  "{" <> T.intercalate ", " [renderKey name <> " = " <> renderValue content | (name, content) <- pairs] <> "}"

renderString :: Text -> Text
renderString text = "\"" <> T.concatMap escape text <> "\""
  where
    escape c =
      case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\b' -> "\\b"
        '\t' -> "\\t"
        '\n' -> "\\n"
        '\f' -> "\\f"
        '\r' -> "\\r"
        _
          | isControl c -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
          | otherwise -> T.singleton c
