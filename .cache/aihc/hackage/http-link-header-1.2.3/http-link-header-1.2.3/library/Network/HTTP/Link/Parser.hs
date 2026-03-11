{-# LANGUAGE OverloadedStrings, UnicodeSyntax, Safe, CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | The parser for the HTTP Link header as defined in RFC 5988.
-- More liberal than the RFC though:
-- does not validate URLs and other deep stuff,
-- accepts whitespace in weird places.
module Network.HTTP.Link.Parser (
  linkHeader
, parseLinkHeader'
, parseLinkHeader
, parseLinkHeaderBS'
, parseLinkHeaderBS
) where

import           Prelude hiding (takeWhile, take)
import           Control.Applicative ((<|>), many)
import           Control.Error.Util (hush)
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString (ByteString)
import           Data.Char (isSpace)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (mconcat)
#endif
import           Data.Attoparsec.Text (Parser, parseOnly, char, sepBy', many', skipSpace, takeWhile, takeWhile1, take, inClass)
import           Network.URI (unEscapeString)
import           Network.HTTP.Link.Types

allConditions ∷ [a → Bool] → a → Bool
allConditions cs x = and $ map ($ x) cs

charWS ∷ Char → Parser ()
charWS x = skipSpace >> char x >> skipSpace

quotedString ∷ Parser Text
quotedString = do
  char '"'
  v ← many stringPart
  char '"'
  return $ pack $ unEscapeString $ unpack $ mconcat v
  where stringPart = takeWhile1 (allConditions [(/= '"'), (/= '\\')]) <|> escapedChar
        escapedChar = char '\\' >> take 1

paramName ∷ Text → LinkParam
paramName "rel"       = Rel
paramName "anchor"    = Anchor
paramName "rev"       = Rev
paramName "hreflang"  = Hreflang
paramName "media"     = Media
paramName "title"     = Title
paramName "title*"    = Title'
paramName "type"      = ContentType
paramName x           = Other x

relType ∷ Parser Text
relType = takeWhile1 $ inClass "-0-9a-z."

paramValue ∷ LinkParam → Parser Text
paramValue Rel    = quotedString <|> relType
paramValue Rev    = quotedString <|> relType
paramValue Title' = takeWhile (allConditions [not . isSpace])
paramValue _      = quotedString

param ∷ Parser (LinkParam, Text)
param = do
  charWS ';'
  n ← takeWhile (allConditions [(/= '='), not . isSpace])
  let n' = paramName n
  charWS '='
  v ← paramValue n'
  return (n', v)

link ∷ (IsURI uri) ⇒ Parser (Link uri)
link = do
  charWS '<'
  linkText ← takeWhile1 $ allConditions [(/= '>'), not . isSpace]
  charWS '>'
  params ← many' $ param
  skipSpace
  case uriFromText linkText of
    Right u → return $ Link u params
    Left e → fail $ "Couldn't parse the URI " ++ show linkText ++ if e == "" then "" else ": " ++ e

-- | The Attoparsec parser for the Link header.
linkHeader ∷ (IsURI uri) ⇒ Parser [Link uri]
linkHeader = link `sepBy'` (char ',')

-- | Parses a Link header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseLinkHeader' ∷ (IsURI uri) ⇒ Text → Either String [Link uri]
parseLinkHeader' = parseOnly linkHeader

-- | Parses a Link header, returns a Maybe.
parseLinkHeader ∷ (IsURI uri) ⇒ Text → Maybe [Link uri]
parseLinkHeader = hush . parseLinkHeader'

-- | Parses a Link header, returns an Either, where Left is the Attoparsec
-- error string (probably not a useful one).
parseLinkHeaderBS' ∷ (IsURI uri) ⇒ ByteString → Either String [Link uri]
parseLinkHeaderBS' = parseLinkHeader' . decodeUtf8

-- | Parses a Link header, returns a Maybe.
parseLinkHeaderBS ∷ (IsURI uri) ⇒ ByteString → Maybe [Link uri]
parseLinkHeaderBS = parseLinkHeader . decodeUtf8
