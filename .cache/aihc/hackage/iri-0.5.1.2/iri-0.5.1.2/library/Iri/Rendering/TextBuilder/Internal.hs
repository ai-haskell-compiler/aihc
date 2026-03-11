module Iri.Rendering.TextBuilder.Internal
  ( iri,
    httpIri,
    scheme,
    hierarchy,
    authority,
    userInfo,
    host,
    regName,
    domainLabel,
    ipV4,
    ipV6,
    port,
    path,
    pathSegment,
    query,
    fragment,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Text as C
import qualified Data.Text.Encoding as A
import qualified Data.Text.Encoding.Error as A
import qualified Iri.CodePointPredicates.Core as CorePredicates
import qualified Iri.CodePointPredicates.Rfc3986 as Rfc3986Predicates
import qualified Iri.CodePointPredicates.Rfc3987 as Rfc3987Predicates
import Iri.Data.Types
import Iri.Prelude hiding (null)
import qualified Iri.Utf8CodePoint as K
import qualified Iri.Vector as F
import qualified Net.IPv4 as D
import qualified Net.IPv6 as E
import TextBuilder

iri :: Iri -> TextBuilder
iri (Iri schemeValue hierarchyValue queryValue fragmentValue) =
  scheme schemeValue
    <> char ':'
    <> hierarchy hierarchyValue
    <> ( prependIfNotNull
           (char '?')
           (query queryValue)
       )
    <> ( prependIfNotNull
           (char '#')
           (fragment fragmentValue)
       )

httpIri :: HttpIri -> TextBuilder
httpIri (HttpIri (Security secure) hostValue portValue pathValue queryValue fragmentValue) =
  (if secure then string "https://" else string "http://")
    <> host hostValue
    <> prependIfNotNull (char ':') (port portValue)
    <> prependIfNotNull (char '/') (path pathValue)
    <> prependIfNotNull (char '?') (query queryValue)
    <> prependIfNotNull (char '#') (fragment fragmentValue)

scheme :: Scheme -> TextBuilder
scheme (Scheme bytes) =
  text (A.decodeUtf8With A.lenientDecode bytes)

hierarchy :: Hierarchy -> TextBuilder
hierarchy =
  \case
    AuthorisedHierarchy authorityValue pathValue ->
      string "//" <> authority authorityValue <> prependIfNotNull (char '/') (path pathValue)
    AbsoluteHierarchy pathValue ->
      char '/' <> path pathValue
    RelativeHierarchy pathValue ->
      path pathValue

authority :: Authority -> TextBuilder
authority (Authority userInfoValue hostValue portValue) =
  appendIfNotNull (char '@') (userInfo userInfoValue)
    <> host hostValue
    <> prependIfNotNull (char ':') (port portValue)

userInfo :: UserInfo -> TextBuilder
userInfo =
  \case
    PresentUserInfo (User user) password -> case password of
      PresentPassword password -> userInfoComponent user <> char ':' <> userInfoComponent password
      MissingPassword -> userInfoComponent user
    MissingUserInfo -> mempty

userInfoComponent :: ByteString -> TextBuilder
userInfoComponent =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedUserInfoComponent Rfc3986Predicates.unencodedUserInfoComponent

host :: Host -> TextBuilder
host =
  \case
    NamedHost value -> regName value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

regName :: RegName -> TextBuilder
regName (RegName vector) =
  F.intercalate domainLabel (char '.') vector

domainLabel :: DomainLabel -> TextBuilder
domainLabel (DomainLabel x) =
  text x

ipV4 :: IPv4 -> TextBuilder
ipV4 =
  text . D.encode

ipV6 :: IPv6 -> TextBuilder
ipV6 =
  text . E.encode

port :: Port -> TextBuilder
port =
  \case
    PresentPort value -> decimal value
    MissingPort -> mempty

path :: Path -> TextBuilder
path (Path pathSegmentVector) =
  F.intercalate pathSegment (char '/') pathSegmentVector

pathSegment :: PathSegment -> TextBuilder
pathSegment (PathSegment value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedPathSegment Rfc3986Predicates.unencodedPathSegment value

query :: Query -> TextBuilder
query (Query value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedQuery Rfc3986Predicates.unencodedQuery value

fragment :: Fragment -> TextBuilder
fragment (Fragment value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedFragment Rfc3986Predicates.unencodedFragment value

urlEncodedBytesOrText :: CorePredicates.Predicate -> CorePredicates.Predicate -> ByteString -> TextBuilder
urlEncodedBytesOrText unencodedPredicate1 unencodedPredicate2 bytes =
  case A.decodeUtf8' bytes of
    Right text -> urlEncodedText unencodedPredicate1 text
    Left _ -> urlEncodedBytes unencodedPredicate2 bytes

-- | Apply URL-encoding to text
urlEncodedBytes :: CorePredicates.Predicate -> ByteString -> TextBuilder
urlEncodedBytes unencodedPredicate =
  ByteString.foldl'
    ( \builder ->
        mappend builder . \byte ->
          if unencodedPredicate (fromIntegral byte)
            then unicodeCodepoint (fromIntegral byte)
            else urlEncodedByte byte
    )
    mempty

-- | Apply URL-encoding to text
urlEncodedText :: CorePredicates.Predicate -> Text -> TextBuilder
urlEncodedText unencodedPredicate =
  C.foldl' (\builder -> mappend builder . urlEncodedUnicodeCodePoint unencodedPredicate . ord) mempty

urlEncodedUnicodeCodePoint :: CorePredicates.Predicate -> Int -> TextBuilder
urlEncodedUnicodeCodePoint unencodedPredicate codePoint =
  if unencodedPredicate codePoint
    then unicodeCodepoint codePoint
    else
      K.unicodeCodePoint
        codePoint
        (\b1 -> urlEncodedByte b1)
        (\b1 b2 -> urlEncodedByte b1 <> urlEncodedByte b2)
        (\b1 b2 b3 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3)
        (\b1 b2 b3 b4 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3 <> urlEncodedByte b4)

urlEncodedByte :: Word8 -> TextBuilder
urlEncodedByte x = char '%' <> hexadecimal x

prependIfNotNull :: TextBuilder -> TextBuilder -> TextBuilder
prependIfNotNull prepended it =
  if isEmpty it
    then mempty
    else prepended <> it

appendIfNotNull :: TextBuilder -> TextBuilder -> TextBuilder
appendIfNotNull appended it =
  if isEmpty it
    then mempty
    else it <> appended
