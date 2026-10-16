{-# LANGUAGE OverloadedStrings #-}

-- | A small JSON encoder.
--
-- The helper writes one JSON value for each request. It does not read
-- JSON, so it needs no decoder and no JSON library.
module Aihc.Play.Json
  ( Json (..),
    renderJson,
    renderJsonLine,
  )
where

import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Numeric (showHex)

data Json
  = JNull
  | JBool !Bool
  | JInt !Int
  | JString !Text
  | JArray [Json]
  | JObject [(Text, Json)]

-- | The value on one line.
renderJson :: Json -> Text
renderJson = TL.toStrict . B.toLazyText . build

-- | The value on one line, with a final newline.
renderJsonLine :: Json -> Text
renderJsonLine value = renderJson value <> "\n"

build :: Json -> B.Builder
build value =
  case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JInt n -> B.fromString (show n)
    JString text -> string text
    JArray items -> "[" <> commas (map build items) <> "]"
    JObject fields -> "{" <> commas [string key <> ":" <> build item | (key, item) <- fields] <> "}"

commas :: [B.Builder] -> B.Builder
commas [] = mempty
commas (first : rest) = first <> mconcat [", " <> item | item <- rest]

string :: Text -> B.Builder
string text = "\"" <> T.foldr (\c acc -> escape c <> acc) mempty text <> "\""

escape :: Char -> B.Builder
escape c =
  case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | ord c < 0x20 -> B.fromString ("\\u" <> pad (showHex (ord c) ""))
      | otherwise -> B.singleton c
  where
    pad digits = replicate (4 - length digits) '0' <> digits
