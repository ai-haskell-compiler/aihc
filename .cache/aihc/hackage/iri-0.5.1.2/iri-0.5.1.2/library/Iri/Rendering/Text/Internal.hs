module Iri.Rendering.Text.Internal where

import Iri.Data.Types
import Iri.Prelude
import qualified Iri.Rendering.TextBuilder.Internal as A
import qualified TextBuilder as B

-- | Render as a Unicode IRI text
iri :: Iri -> Text
iri = B.toText . A.iri

-- | Render as a Unicode IRI text
httpIri :: HttpIri -> Text
httpIri = B.toText . A.httpIri

scheme :: Scheme -> Text
scheme = B.toText . A.scheme

hierarchy :: Hierarchy -> Text
hierarchy = B.toText . A.hierarchy

authority :: Authority -> Text
authority = B.toText . A.authority

userInfo :: UserInfo -> Text
userInfo = B.toText . A.userInfo

host :: Host -> Text
host = B.toText . A.host

regName :: RegName -> Text
regName = B.toText . A.regName

domainLabel :: DomainLabel -> Text
domainLabel = B.toText . A.domainLabel

ipV4 :: IPv4 -> Text
ipV4 = B.toText . A.ipV4

ipV6 :: IPv6 -> Text
ipV6 = B.toText . A.ipV6

port :: Port -> Text
port = B.toText . A.port

path :: Path -> Text
path = B.toText . A.path

pathSegment :: PathSegment -> Text
pathSegment = B.toText . A.pathSegment

query :: Query -> Text
query = B.toText . A.query

fragment :: Fragment -> Text
fragment = B.toText . A.fragment
