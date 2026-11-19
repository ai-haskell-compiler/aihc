-- |
-- Module      : Aihc.PackagePlan.Json
-- Description : A small JSON reader and writer for the lock file
--
-- The lock file is the only JSON that the package plan reads and writes.
-- This module holds only what the lock file needs, so that @aihc@ does not
-- depend on @aeson@ when it is built without Hackage access.
module Aihc.PackagePlan.Json
  ( Value (..),
    decode,
    encode,
    encodeString,
  )
where

import Data.Bifunctor (first)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (readHex, showHex)

-- | A JSON value. A number is an integer, because the lock file has no
-- other numbers. If a key occurs two times in an object, the last value
-- is kept.
data Value
  = Null
  | Bool !Bool
  | Number !Integer
  | String !Text
  | Array ![Value]
  | Object !(Map Text Value)
  deriving (Eq, Show)

-- | Read one JSON value from UTF-8 bytes.
decode :: BL.ByteString -> Either String Value
decode bytes = do
  text <- first show (TE.decodeUtf8' (BL.toStrict bytes))
  (value, rest) <- parseValue (skipSpace text)
  if T.null (skipSpace rest)
    then pure value
    else Left ("unexpected text after the JSON value: " <> preview rest)

-- | Write a JSON value without white space.
encode :: Value -> BL.ByteString
encode = Builder.toLazyByteString . encodeValue

encodeValue :: Value -> Builder
encodeValue value =
  case value of
    Null -> Builder.string7 "null"
    Bool True -> Builder.string7 "true"
    Bool False -> Builder.string7 "false"
    Number number -> Builder.integerDec number
    String text -> encodeString (T.unpack text)
    Array values -> bracketed '[' ']' (map encodeValue values)
    Object fields ->
      bracketed '{' '}' [encodeString (T.unpack key) <> Builder.char7 ':' <> encodeValue field | (key, field) <- Map.toAscList fields]
  where
    bracketed open close items =
      Builder.char7 open <> mconcat (intersperse (Builder.char7 ',') items) <> Builder.char7 close

-- | Write a JSON string, with quotes and escapes, as UTF-8.
encodeString :: String -> Builder
encodeString text = Builder.char7 '"' <> foldMap escape text <> Builder.char7 '"'
  where
    escape char =
      case char of
        '"' -> Builder.string7 "\\\""
        '\\' -> Builder.string7 "\\\\"
        '\n' -> Builder.string7 "\\n"
        '\r' -> Builder.string7 "\\r"
        '\t' -> Builder.string7 "\\t"
        _
          | ord char < 0x20 -> Builder.string7 ("\\u" <> padHex (showHex (ord char) ""))
          | otherwise -> Builder.charUtf8 char
    padHex digits = replicate (4 - length digits) '0' <> digits

type Parser a = Text -> Either String (a, Text)

parseValue :: Parser Value
parseValue input =
  case T.uncons input of
    Nothing -> Left "unexpected end of input"
    Just (char, rest) ->
      case char of
        '{' -> parseObject (skipSpace rest)
        '[' -> parseArray (skipSpace rest)
        '"' -> first String <$> parseString rest
        't' -> literal "true" (Bool True)
        'f' -> literal "false" (Bool False)
        'n' -> literal "null" Null
        _
          | char == '-' || isDigit char -> parseNumber input
          | otherwise -> Left ("unexpected text: " <> preview input)
  where
    literal word value =
      case T.stripPrefix (T.pack word) input of
        Just rest -> Right (value, rest)
        Nothing -> Left ("unexpected text: " <> preview input)

parseObject :: Parser Value
parseObject input
  | Just rest <- T.stripPrefix (T.pack "}") input = Right (Object Map.empty, rest)
  | otherwise = go Map.empty input
  where
    go fields text = do
      afterQuote <- expect '"' text
      (key, afterKey) <- parseString afterQuote
      afterColon <- expect ':' (skipSpace afterKey)
      (value, afterValue) <- parseValue (skipSpace afterColon)
      let fields' = Map.insert key value fields
      case T.uncons (skipSpace afterValue) of
        Just (',', rest) -> go fields' (skipSpace rest)
        Just ('}', rest) -> Right (Object fields', rest)
        _ -> Left ("expected ',' or '}': " <> preview afterValue)

parseArray :: Parser Value
parseArray input
  | Just rest <- T.stripPrefix (T.pack "]") input = Right (Array [], rest)
  | otherwise = go [] input
  where
    go values text = do
      (value, afterValue) <- parseValue text
      case T.uncons (skipSpace afterValue) of
        Just (',', rest) -> go (value : values) (skipSpace rest)
        Just (']', rest) -> Right (Array (reverse (value : values)), rest)
        _ -> Left ("expected ',' or ']': " <> preview afterValue)

-- | Read the rest of a string after its opening quote.
parseString :: Parser Text
parseString = go []
  where
    go chunks text =
      let (plain, rest) = T.break (\char -> char == '"' || char == '\\' || ord char < 0x20) text
          chunks' = plain : chunks
       in case T.uncons rest of
            Just ('"', after) -> Right (T.concat (reverse chunks'), after)
            Just ('\\', after) -> do
              (char, after') <- parseEscape after
              go (T.singleton char : chunks') after'
            Just _ -> Left "control character in a string"
            Nothing -> Left "unterminated string"

parseEscape :: Parser Char
parseEscape text =
  case T.uncons text of
    Just ('"', rest) -> Right ('"', rest)
    Just ('\\', rest) -> Right ('\\', rest)
    Just ('/', rest) -> Right ('/', rest)
    Just ('b', rest) -> Right ('\b', rest)
    Just ('f', rest) -> Right ('\f', rest)
    Just ('n', rest) -> Right ('\n', rest)
    Just ('r', rest) -> Right ('\r', rest)
    Just ('t', rest) -> Right ('\t', rest)
    Just ('u', rest) -> do
      (high, afterHigh) <- hex4 rest
      if high >= 0xD800 && high < 0xDC00
        then case T.stripPrefix (T.pack "\\u") afterHigh of
          Just afterMarker -> do
            (low, afterLow) <- hex4 afterMarker
            if low >= 0xDC00 && low < 0xE000
              then Right (chr (0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)), afterLow)
              else Left "invalid surrogate pair"
          Nothing -> Left "invalid surrogate pair"
        else
          if high >= 0xDC00 && high < 0xE000
            then Left "invalid surrogate pair"
            else Right (chr high, afterHigh)
    _ -> Left ("invalid escape: " <> preview text)
  where
    hex4 input =
      let (digits, rest) = T.splitAt 4 input
       in case readHex (T.unpack digits) of
            [(value, "")] | T.length digits == 4, T.all isHexDigit digits -> Right (value, rest)
            _ -> Left ("invalid escape: " <> preview input)

parseNumber :: Parser Value
parseNumber input =
  let (sign, afterSign) = maybe ("", input) ((,) "-") (T.stripPrefix (T.pack "-") input)
      (digits, rest) = T.span isDigit afterSign
   in if T.null digits || (T.length digits > 1 && T.head digits == '0')
        then Left ("invalid number: " <> preview input)
        else case T.uncons rest of
          Just (char, _)
            | char `elem` (".eE" :: String) -> Left ("only integers are supported: " <> preview input)
          _ -> Right (Number (read (sign <> T.unpack digits)), rest)

expect :: Char -> Text -> Either String Text
expect char text =
  case T.uncons text of
    Just (found, rest) | found == char -> Right rest
    _ -> Left ("expected '" <> [char] <> "': " <> preview text)

skipSpace :: Text -> Text
skipSpace = T.dropWhile (`elem` (" \t\n\r" :: String))

preview :: Text -> String
preview text
  | T.null text = "end of input"
  | otherwise = show (T.take 20 text)
