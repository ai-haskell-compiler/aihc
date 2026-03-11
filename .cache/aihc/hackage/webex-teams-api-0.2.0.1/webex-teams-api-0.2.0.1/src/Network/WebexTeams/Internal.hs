{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.WebexTeams.Internal
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Implements some internal functions for Cisco Webex Teams API.
-}
module Network.WebexTeams.Internal where

import           Prelude                    hiding (concat, takeWhile)

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString, concat, pack,
                                             singleton)
import           Data.ByteString.Char8      (unpack)
import           Data.Char                  (toLower)
import           Data.Either                (rights)
import           Data.Maybe                 (listToMaybe)
import           Data.Word                  (Word8)

import           Data.BitSetWord8           (member, rfc3986UriReference,
                                             rfc7230QDText, rfc7230QuotedPair,
                                             rfc7230TChar)

import           Network.HTTP.Simple        (Response, getResponseHeader)
import           Network.URI                (URI (..), URIAuth (..), parseURI)


{-|
    Drop given number of characters from the given 'String' then
    change first character of the remaining string to lowercase.
    This function is intended to be used for 'fieldLabelModifier' and 'constructorTagModifier'
    argument of 'DeriveJSON' from DATA.Aeson.TH.
    You can find how this function is used in 'Network.WebexTeams' source code.

-}
dropAndLow
    :: Int      -- ^ Number of characters to drop
    -> String   -- ^ Field name to be modified to JSON field name
    -> String
dropAndLow n = toLowerHead . drop n
  where
    toLowerHead []     = []
    toLowerHead (c:cs) = toLower c : cs

{-
    From here, defining Attoparsec parser of RFC5988 HTTP Link Header.
    Link header is defined in RFC5988 https://tools.ietf.org/html/rfc5988.
    This parser doesn't parse complete spec of RFC5988 but only parses rel="next" link for simple pagination.
    It doesn't parse obs-fold defined in RFC7230 https://tools.ietf.org/html/rfc7230.
    It assumes Header in Response never contains CRLF or LF.
-}

-- | Parsed Link header parameter.  Convert only rel param to 'Rel' and keeps other params as-is.
data LinkParam = Rel | Other ByteString deriving (Eq, Show)

-- | Parsed entire Link header value.  It keeps URI-Reference part untouched for farther processing in different way.
data LinkHeader = LinkHeader
    { linkHeaderUrl    :: ByteString
    , linkHeaderParams :: [(LinkParam, ByteString)]
    }deriving (Eq, Show)

-- | Matches to double quote.
dQuote :: Parser Word8
dQuote = word8 0x22         -- '"'

-- | Matches to semicolon.
semicolon :: Parser Word8
semicolon = word8 0x3b      -- ';'

-- | Matches to equal character.
equalChar :: Parser Word8
equalChar = word8 0x3d      -- '='

-- | Matches to less-than character.
lessThan :: Parser Word8
lessThan = word8 0x3c       -- '<'

-- | Matches to greater-than character.
greaterThan :: Parser Word8
greaterThan = word8 0x3e    -- '>'

-- | Skips white spaces.
skipSpace :: Parser ()
skipSpace = skipWhile (\c -> c == 0x20 || c == 0x09)    -- white space or tab

-- | Parse RFC7230 token.
token :: Parser ByteString
token = takeWhile1 (member rfc7230TChar)

-- | Parse RFC7230 quoted-pair.
quotedPair :: Parser Word8
quotedPair = word8 0x5c >> satisfy (member rfc7230QuotedPair)

-- | Parse RFC7230 quoted-string.
quotedString :: Parser ByteString
quotedString = do
    dQuote
    bss <- many' $ takeWhile1 (member rfc7230QDText) <|> (singleton <$> quotedPair)
    dQuote
    pure $ concat bss

-- | Parse a parameter value in Link header.
paramValue :: Parser ByteString
paramValue = quotedString <|> token

-- | Convert parameter name string to LinkParam.
paramName :: ByteString -> LinkParam
paramName "rel" = Rel
paramName x     = Other x

-- | Parse parameter part of Link header.
param :: Parser (LinkParam, ByteString)
param = do
    semicolon
    skipSpace
    name <- paramName <$> token
    skipSpace
    equalChar
    skipSpace
    val <- paramValue
    pure (name, val)

{-|
    Attoparsec parser of RFC5988 HTTP Link Header.
    Link header is defined in RFC5988 https://tools.ietf.org/html/rfc5988.
    This parser doesn't parse complete spec of RFC5988 but only parses rel="next" link for simple pagination.
    It doesn't parse obs-fold defined in RFC7230 https://tools.ietf.org/html/rfc7230.
    It assumes Header in Response never contains CRLF or LF.
-}

linkHeader :: Parser LinkHeader
linkHeader = do
    skipSpace
    lessThan
    uri <- takeWhile (member rfc3986UriReference)
    greaterThan
    skipSpace
    params <- many' param
    pure $ LinkHeader uri params

-- | Accept all RFC5988 Link HTTP header, filter first header containing rel="next" param, parse URL part.
extractNextUrl :: [ByteString] -> [ByteString]
extractNextUrl = map linkHeaderUrl . filter isNextRel . rights . map (parseOnly linkHeader)
  where
    isNextRel = any (\(param, str) -> param == Rel && str == "next") . linkHeaderParams

-- | Return URL for next page if it exists in given response.
getNextUrl :: Response a -> Maybe ByteString
getNextUrl = listToMaybe . extractNextUrl . getResponseHeader "Link"

{-|
    Validate extracted URL from HTTP Link Header by 'getNextUrl'.
    Check if it has same scheme and URI authority as original request.
-}
validateUrl :: String -> URIAuth -> ByteString -> Maybe ByteString
validateUrl scheme uriAuth url = do
    uri <- parseURI $ unpack url
    auth <- uriAuthority uri
    if (uriScheme uri == scheme) && (auth == uriAuth) then pure url else Nothing
