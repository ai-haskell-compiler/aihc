{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Iri.Parsing.Attoparsec.Text
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

import Data.Attoparsec.Text hiding (try)
import qualified Data.ByteString as K
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding.Error as L
import qualified Data.Vector as S
import qualified Iri.CodePointPredicates.Rfc3987 as C
import Iri.Data
import qualified Iri.MonadPlus as R
import Iri.Prelude
import qualified Net.IPv4 as M
import qualified Net.IPv6 as N
import qualified Ptr.ByteString as ByteString
import qualified Ptr.Poking as Poking
import qualified TextBuilder as J
import qualified VectorBuilder.MonadPlus as E

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label

-- |
-- Parser of a well-formed IRI conforming to the RFC3987 standard into 'Iri'.
-- Performs URL-decoding.
{-# INLINEABLE iri #-}
iri :: Parser Iri
iri =
  labeled "IRI" $ do
    parsedScheme <- scheme
    _ <- char ':'
    parsedHierarchy <- hierarchy
    parsedQuery <- query
    parsedFragment <- fragment
    return (Iri parsedScheme parsedHierarchy parsedQuery parsedFragment)

-- |
-- Same as 'iri', but optimized specifially for the case of HTTP IRIs.
{-# INLINEABLE httpIri #-}
httpIri :: Parser HttpIri
httpIri =
  labeled "HTTP IRI" $ do
    _ <- asciiCI "http"
    secure <- satisfy (\x -> x == 's' || x == 'S') $> True <|> pure False
    _ <- string "://"
    parsedHost <- host
    parsedPort <- PresentPort <$> (char ':' *> port) <|> pure MissingPort
    parsedPath <- ((char '/') *> path) <|> pure (Path mempty)
    parsedQuery <- query
    parsedFragment <- fragment
    return (HttpIri (Security secure) parsedHost parsedPort parsedPath parsedQuery parsedFragment)

{-# INLINE hierarchy #-}
hierarchy :: Parser Hierarchy
hierarchy =
  do
    slashPresent <- (char '/') $> True <|> pure False
    if slashPresent
      then do
        slashPresent <- (char '/') $> True <|> pure False
        if slashPresent
          then authorisedHierarchyBody AuthorisedHierarchy
          else AbsoluteHierarchy <$> path
      else RelativeHierarchy <$> path

{-# INLINE authorisedHierarchyBody #-}
authorisedHierarchyBody :: (Authority -> Path -> body) -> Parser body
authorisedHierarchyBody body =
  do
    parsedUserInfo <- (presentUserInfo PresentUserInfo <* char '@') <|> pure MissingUserInfo
    parsedHost <- host
    parsedPort <- PresentPort <$> (char ':' *> port) <|> pure MissingPort
    parsedPath <- ((char '/') *> path) <|> pure (Path mempty)
    return (body (Authority parsedUserInfo parsedHost parsedPort) parsedPath)

{-# INLINE scheme #-}
scheme :: Parser Scheme
scheme =
  labeled "Scheme"
    $ fmap (Scheme . B.encodeUtf8) (takeWhile1 (C.scheme . ord))

{-# INLINEABLE presentUserInfo #-}
presentUserInfo :: (User -> Password -> a) -> Parser a
presentUserInfo result =
  labeled "User info"
    $ do
      user <- User <$> urlEncodedComponent (C.unencodedUserInfoComponent . ord)
      passwordFollows <- True <$ char ':' <|> pure False
      if passwordFollows
        then do
          password <- PresentPassword <$> urlEncodedComponent (C.unencodedUserInfoComponent . ord)
          return (result user password)
        else return (result user MissingPassword)

{-# INLINE host #-}
host :: Parser Host
host =
  labeled "Host"
    $ IpV6Host
    <$> N.parser
    <|> IpV4Host
    <$> M.parser
    <|> NamedHost
    <$> regName

{-# INLINE regName #-}
regName :: Parser RegName
regName =
  fmap RegName (E.sepBy1 domainLabel (char '.'))

-- |
-- Domain label with Punycode decoding applied.
{-# INLINE domainLabel #-}
domainLabel :: Parser DomainLabel
domainLabel =
  labeled "Domain label"
    $ DomainLabel
    <$> takeWhile1 (C.unencodedRegName . ord)

{-# INLINE port #-}
port :: Parser Word16
port =
  decimal

{-# INLINE path #-}
path :: Parser Path
path =
  do
    segments <- E.sepBy pathSegment (char '/')
    if segmentsAreEmpty segments
      then return (Path mempty)
      else return (Path segments)
  where
    segmentsAreEmpty segments =
      S.length segments
        == 1
        && (case S.unsafeHead segments of PathSegment headSegment -> K.null headSegment)

{-# INLINE pathSegment #-}
pathSegment :: Parser PathSegment
pathSegment =
  fmap PathSegment (urlEncodedComponent (C.unencodedPathSegment . ord))

{-# INLINEABLE urlEncodedComponent #-}
urlEncodedComponent :: (Char -> Bool) -> Parser ByteString
urlEncodedComponent unencodedCharPredicate =
  labeled "URL-encoded component"
    $ fmap ByteString.poking
    $ R.fold
    $ (Poking.bytes . B.encodeUtf8 <$> takeWhile1 unencodedCharPredicate)
    <|> (Poking.word8 <$> urlEncodedByte)

{-# INLINEABLE urlEncodedComponentText #-}
urlEncodedComponentText :: (Char -> Bool) -> Parser Text
urlEncodedComponentText unencodedCharPredicate =
  labeled "URL-encoded component"
    $ fmap J.toText
    $ R.foldl mappend mempty
    $ (J.text <$> takeWhile1 unencodedCharPredicate)
    <|> urlEncodedSequenceTextBuilder

{-# INLINEABLE urlEncodedSequenceTextBuilder #-}
urlEncodedSequenceTextBuilder :: Parser J.TextBuilder
urlEncodedSequenceTextBuilder =
  labeled "URL-encoded sequence" $ do
    start <- progress (mempty, mempty, B.streamDecodeUtf8) =<< urlEncodedByte
    R.foldlM progress (start) urlEncodedByte >>= finish
  where
    progress (!builder, _ :: ByteString, decode) byte =
      case unsafeDupablePerformIO (try (evaluate (decode (K.singleton byte)))) of
        Right (B.Some decodedChunk undecodedBytes newDecode) ->
          return (builder <> J.text decodedChunk, undecodedBytes, newDecode)
        Left (L.DecodeError error _) ->
          fail (showString "UTF8 decoding: " error)
        Left _ ->
          fail "Unexpected decoding error"
    finish (builder, undecodedBytes, _) =
      if K.null undecodedBytes
        then return builder
        else fail (showString "UTF8 decoding: Bytes remaining: " (show undecodedBytes))

{-# INLINE urlEncodedByte #-}
urlEncodedByte :: Parser Word8
urlEncodedByte =
  do
    _ <- char '%'
    digit1 <- fromIntegral <$> hexadecimalDigit
    digit2 <- fromIntegral <$> hexadecimalDigit
    return (shiftL digit1 4 .|. digit2)

{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Parser Int
hexadecimalDigit =
  do
    c <- anyChar
    let x = ord c
    if x >= 48 && x < 58
      then return (x - 48)
      else
        if x >= 65 && x < 71
          then return (x - 55)
          else
            if x >= 97 && x < 103
              then return (x - 87)
              else fail ("Not a hexadecimal digit: " <> show c)

{-# INLINEABLE query #-}
query :: Parser Query
query =
  labeled "Query"
    $ (char '?' *> queryBody)
    <|> pure (Query mempty)

-- |
-- The stuff after the question mark.
{-# INLINEABLE queryBody #-}
queryBody :: Parser Query
queryBody =
  fmap Query (urlEncodedComponent (C.unencodedQuery . ord))

{-# INLINEABLE fragment #-}
fragment :: Parser Fragment
fragment =
  labeled "Fragment"
    $ (char '#' *> (Fragment <$> urlEncodedComponent (C.unencodedFragment . ord)))
    <|> pure (Fragment mempty)
