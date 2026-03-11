{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Iri.Parsing.Attoparsec.ByteString
  ( uri,
    httpUri,
    regName,
  )
where

import Data.Attoparsec.ByteString hiding (try)
import qualified Data.Attoparsec.ByteString.Char8 as F
import qualified Data.ByteString as K
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding.Error as L
import qualified Data.Text.Punycode as A
import qualified Data.Vector as S
import qualified Iri.CodePointPredicates.Rfc3986 as C
import Iri.Data
import qualified Iri.MonadPlus as R
import qualified Iri.PercentEncoding as I
import Iri.Prelude hiding (foldl, hash)
import qualified Net.IPv4 as M
import qualified Net.IPv6 as N
import qualified TextBuilder as J
import qualified VectorBuilder.MonadPlus as E

{-# INLINE percent #-}
percent :: Parser Word8
percent =
  word8 37

{-# INLINE plus #-}
plus :: Parser Word8
plus =
  word8 43

{-# INLINE colon #-}
colon :: Parser Word8
colon =
  word8 58

{-# INLINE at #-}
at :: Parser Word8
at =
  word8 64

{-# INLINE forwardSlash #-}
forwardSlash :: Parser Word8
forwardSlash =
  word8 47

{-# INLINE question #-}
question :: Parser Word8
question =
  word8 63

{-# INLINE hash #-}
hash :: Parser Word8
hash =
  word8 35

{-# INLINE equality #-}
equality :: Parser Word8
equality =
  word8 61

{-# INLINE ampersand #-}
ampersand :: Parser Word8
ampersand =
  word8 38

{-# INLINE semicolon #-}
semicolon :: Parser Word8
semicolon =
  word8 59

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label

-- |
-- Parser of a well-formed URI conforming to the RFC3986 or RFC3987 standards.
-- Performs URL- and Punycode-decoding.
{-# INLINEABLE uri #-}
uri :: Parser Iri
uri =
  labeled "URI" $ do
    parsedScheme <- scheme
    _ <- colon
    parsedHierarchy <- hierarchy
    parsedQuery <- query
    parsedFragment <- fragment
    return (Iri parsedScheme parsedHierarchy parsedQuery parsedFragment)

-- |
-- Same as 'uri', but optimized specifially for the case of HTTP URIs.
{-# INLINEABLE httpUri #-}
httpUri :: Parser HttpIri
httpUri =
  labeled "HTTP URI" $ do
    _ <- satisfy (\x -> x == 104 || x == 72)
    _ <- satisfy (\x -> x == 116 || x == 84)
    _ <- satisfy (\x -> x == 116 || x == 84)
    _ <- satisfy (\x -> x == 112 || x == 80)
    secure <- satisfy (\b -> b == 115 || b == 83) $> True <|> pure False
    _ <- string "://"
    parsedHost <- host
    parsedPort <- PresentPort <$> (colon *> port) <|> pure MissingPort
    parsedPath <- (forwardSlash *> path) <|> pure (Path mempty)
    parsedQuery <- query
    parsedFragment <- fragment
    return (HttpIri (Security secure) parsedHost parsedPort parsedPath parsedQuery parsedFragment)

{-# INLINE hierarchy #-}
hierarchy :: Parser Hierarchy
hierarchy =
  do
    slashPresent <- forwardSlash $> True <|> pure False
    if slashPresent
      then do
        slashPresent <- forwardSlash $> True <|> pure False
        if slashPresent
          then authorisedHierarchyBody AuthorisedHierarchy
          else AbsoluteHierarchy <$> path
      else RelativeHierarchy <$> path

{-# INLINE authorisedHierarchyBody #-}
authorisedHierarchyBody :: (Authority -> Path -> body) -> Parser body
authorisedHierarchyBody body =
  do
    parsedUserInfo <- (presentUserInfo PresentUserInfo <* at) <|> pure MissingUserInfo
    parsedHost <- host
    parsedPort <- PresentPort <$> (colon *> port) <|> pure MissingPort
    parsedPath <- (forwardSlash *> path) <|> pure (Path mempty)
    return (body (Authority parsedUserInfo parsedHost parsedPort) parsedPath)

{-# INLINE scheme #-}
scheme :: Parser Scheme
scheme =
  labeled "Scheme"
    $ fmap Scheme (takeWhile1 (C.scheme . fromIntegral))

{-# INLINEABLE presentUserInfo #-}
presentUserInfo :: (User -> Password -> a) -> Parser a
presentUserInfo result =
  labeled "User info"
    $ do
      user <- User <$> urlEncodedString (C.unencodedUserInfoComponent . fromIntegral)
      passwordFollows <- True <$ colon <|> pure False
      if passwordFollows
        then do
          password <- PresentPassword <$> urlEncodedString (C.unencodedUserInfoComponent . fromIntegral)
          return (result user password)
        else return (result user MissingPassword)

{-# INLINE host #-}
host :: Parser Host
host =
  labeled "Host"
    $ asum
      [ IpV6Host <$> ipV6,
        IpV4Host <$> M.parserUtf8,
        NamedHost <$> regName
      ]

{-# INLINEABLE ipV6 #-}
ipV6 :: Parser IPv6
ipV6 =
  do
    a <- F.hexadecimal
    _ <- colon
    b <- F.hexadecimal
    _ <- colon
    c <- F.hexadecimal
    _ <- colon
    d <- F.hexadecimal
    _ <- colon
    mplus
      ( do
          e <- F.hexadecimal
          _ <- colon
          f <- F.hexadecimal
          _ <- colon
          g <- F.hexadecimal
          _ <- colon
          h <- F.hexadecimal
          return (N.fromWord16s a b c d e f g h)
      )
      ( do
          _ <- colon
          return (N.fromWord16s a b c d 0 0 0 0)
      )

{-# INLINE regName #-}
regName :: Parser RegName
regName =
  fmap RegName (E.sepBy1 domainLabel (word8 46))

-- |
-- Domain label with Punycode decoding applied if need be.
{-# INLINE domainLabel #-}
domainLabel :: Parser DomainLabel
domainLabel =
  labeled "Domain label" $ do
    punycodeFollows <- True <$ string "xn--" <|> pure False
    ascii <- takeWhile1 (C.domainLabel . fromIntegral)
    if punycodeFollows
      then case A.decode ascii of
        Right text -> return (DomainLabel text)
        Left exception -> fail (showString "Punycode decoding exception: " (show exception))
      else return (DomainLabel (B.decodeUtf8 ascii))

{-# INLINE port #-}
port :: Parser Word16
port =
  F.decimal

{-# INLINE path #-}
path :: Parser Path
path =
  do
    segments <- E.sepBy pathSegment forwardSlash
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
  fmap PathSegment (urlEncodedString (C.unencodedPathSegment . fromIntegral))

{-# INLINEABLE utf8Chunks #-}
utf8Chunks :: Parser ByteString -> Parser Text
utf8Chunks chunk =
  labeled "UTF8 chunks"
    $ R.foldlM progress (mempty, mempty, B.streamDecodeUtf8) chunk
    >>= finish
  where
    progress (!builder, _, decode) bytes =
      case unsafeDupablePerformIO (try (evaluate (decode bytes))) of
        Right (B.Some decodedChunk undecodedBytes newDecode) ->
          return (builder <> J.text decodedChunk, undecodedBytes, newDecode)
        Left (L.DecodeError error _) ->
          fail (showString "UTF8 decoding: " error)
        Left _ ->
          fail "Unexpected decoding error"
    finish (builder, undecodedBytes, _) =
      if K.null undecodedBytes
        then return (J.toText builder)
        else fail (showString "UTF8 decoding: Bytes remaining: " (show undecodedBytes))

{-# INLINEABLE urlEncodedString #-}
urlEncodedString :: (Word8 -> Bool) -> Parser ByteString
urlEncodedString unencodedBytesPredicate =
  labeled "URL-encoded string"
    $ R.foldByteString
    $ takeWhile1 unencodedBytesPredicate
    <|> encoded
  where
    encoded =
      K.singleton <$> percentEncodedByte

{-# INLINE percentEncodedByte #-}
percentEncodedByte :: Parser Word8
percentEncodedByte =
  labeled "Percent-encoded byte" $ do
    _ <- percent
    byte1 <- anyWord8
    byte2 <- anyWord8
    I.matchPercentEncodedBytes (fail "Broken percent encoding") return byte1 byte2

{-# INLINE query #-}
query :: Parser Query
query =
  labeled "Query"
    $ (question *> (Query <$> queryOrFragmentBody))
    <|> pure (Query mempty)

{-# INLINE fragment #-}
fragment :: Parser Fragment
fragment =
  labeled "Fragment"
    $ (hash *> (Fragment <$> queryOrFragmentBody))
    <|> pure (Fragment mempty)

-- |
-- The stuff after the question or the hash mark.
{-# INLINEABLE queryOrFragmentBody #-}
queryOrFragmentBody :: Parser ByteString
queryOrFragmentBody =
  R.foldByteString
    $ takeWhile1 (C.unencodedQuery . fromIntegral)
    <|> " "
    <$ plus
    <|> K.singleton
    <$> percentEncodedByte
