{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Iri.Parsing.ByteString
  ( uri,
    httpUri,
    regName,
    uriQuery,
  )
where

import qualified Data.Attoparsec.ByteString as B
import qualified Data.ByteString as ByteString
import Iri.Data
import qualified Iri.Parsing.Attoparsec.ByteString as A
import Iri.Prelude

parser :: B.Parser a -> ByteString -> Either Text a
parser parser =
  either (Left . fromString) Right
    . B.parseOnly (parser <* B.endOfInput)

-- |
-- Parser of a well-formed URI conforming to the RFC3986 standard into IRI.
-- Performs URL- and Punycode-decoding.
uri :: ByteString -> Either Text Iri
uri =
  parser A.uri

-- |
-- Same as 'uri', but optimized specifially for the case of HTTP URIs.
httpUri :: ByteString -> Either Text HttpIri
httpUri =
  parser A.httpUri

-- |
-- Domain name parser.
regName :: ByteString -> Either Text RegName
regName =
  parser A.regName

-- |
-- Assuming we have a valid URI as input, extract the query part from it.
uriQuery :: ByteString -> Either Text Query
uriQuery x =
  case ByteString.break (== 63) x of
    (_, questionAndAfterQuestion) -> case ByteString.uncons questionAndAfterQuestion of
      Just (_, afterQuestion) -> case ByteString.break (== 35) afterQuestion of
        (beforeHash, _) -> Right (Query beforeHash)
      Nothing ->
        Left "Does not start with a question mark"

-- |
-- Assuming we have a valid URI as input, extract the fragment part from it.
uriFragment :: ByteString -> Fragment
uriFragment = ByteString.break (== 35) >>> snd >>> ByteString.drop 1 >>> Fragment
