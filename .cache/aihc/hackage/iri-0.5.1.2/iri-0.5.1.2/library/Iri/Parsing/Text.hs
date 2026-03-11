module Iri.Parsing.Text
  ( iri,
    httpIri,
    hierarchy,
    scheme,
    host,
    regName,
    domainLabel,
    port,
    path,
    pathSegment,
    query,
    fragment,
  )
where

import qualified Data.Attoparsec.Text as B
import Iri.Data
import qualified Iri.Parsing.Attoparsec.Text as A
import Iri.Prelude

fromParser :: B.Parser a -> Text -> Either Text a
fromParser parser =
  either (Left . fromString) Right
    . B.parseOnly (parser <* B.endOfInput)

-- |
-- Parser of a well-formed IRI conforming to the RFC3987 standard into 'Iri'.
-- Performs URL-decoding.
iri :: Text -> Either Text Iri
iri = fromParser A.iri

-- |
-- Same as 'iri', but optimized specifially for the case of HTTP IRIs.
httpIri :: Text -> Either Text HttpIri
httpIri = fromParser A.httpIri

hierarchy :: Text -> Either Text Hierarchy
hierarchy = fromParser A.hierarchy

scheme :: Text -> Either Text Scheme
scheme = fromParser A.scheme

host :: Text -> Either Text Host
host = fromParser A.host

regName :: Text -> Either Text RegName
regName = fromParser A.regName

domainLabel :: Text -> Either Text DomainLabel
domainLabel = fromParser A.domainLabel

port :: Text -> Either Text Word16
port = fromParser A.port

path :: Text -> Either Text Path
path = fromParser A.path

pathSegment :: Text -> Either Text PathSegment
pathSegment = fromParser A.pathSegment

query :: Text -> Either Text Query
query = fromParser A.query

fragment :: Text -> Either Text Fragment
fragment = fromParser A.fragment
