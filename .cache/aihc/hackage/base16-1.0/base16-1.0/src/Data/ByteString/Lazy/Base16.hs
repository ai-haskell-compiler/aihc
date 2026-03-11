{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Lazy.Base16
-- Copyright    : (c) 2020-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base16
( encodeBase16
, encodeBase16'
, decodeBase16
, decodeBase16'
, decodeBase16Untyped
, decodeBase16Lenient
, isBase16
, isValidBase16
) where



import Prelude hiding (all, elem)

import Data.Base16.Types
import qualified Data.ByteString as B
import Data.ByteString.Lazy (all, elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.ByteString.Base16.Internal.Head as B16
import Data.ByteString.Base16.Internal.Utils (reChunk)
import Data.Either
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a lazy 'ByteString' value as Base16 'Text'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ByteString -> Base16 Text
encodeBase16 = fmap TL.decodeUtf8 . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a lazy 'ByteString' value as a Base16 'ByteString' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ByteString -> Base16 ByteString
encodeBase16' bs = assertBase16 $ case bs of
    Empty -> Empty
    Chunk b bs' -> Chunk (B16.encodeBase16_ b) (extractBase16 $ encodeBase16' bs')
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded lazy 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 ByteString -> ByteString
decodeBase16 bs = case extractBase16 bs of
    Empty -> Empty
    Chunk b bs' -> Chunk
      (B16.decodeBase16Typed_ (assertBase16 b))
      (decodeBase16 $ assertBase16 bs')
{-# INLINE decodeBase16 #-}

-- | Decode a Base16-encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16' $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16' :: Base16 Text -> ByteString
decodeBase16' = decodeBase16 . fmap TL.encodeUtf8
{-# INLINE decodeBase16' #-}

-- | Decode an untyped Base16-encoded lazy 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16Untyped "53756e"
-- Right "Sun"
--
-- >>> decodeBase16Untyped "6x"
-- Left "invalid character at offset: 1"
--
decodeBase16Untyped :: ByteString -> Either T.Text ByteString
decodeBase16Untyped Empty = Right Empty
decodeBase16Untyped (Chunk b bs) = Chunk <$> B16.decodeBase16_ b <*> decodeBase16Untyped bs
{-# INLINE decodeBase16Untyped #-}


-- | Decode an untyped Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant. It may give you garbage if you're not careful!
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16Lenient "6x6x"
-- "f"
--
decodeBase16Lenient :: ByteString -> ByteString
decodeBase16Lenient = fromChunks
  . fmap B16.decodeBase16Lenient_
  . reChunk
  . fmap (B.filter (flip elem "0123456789abcdefABCDEF"))
  . toChunks
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether an untyped lazy 'ByteString' value is base16 encoded.
--
-- === __Examples__:
--
-- >>> isBase16 "666f6"
-- False
--
-- >>> isBase16 "666f"
-- True
--
isBase16 :: ByteString -> Bool
isBase16 bs = isValidBase16 bs && isRight (decodeBase16Untyped bs)
{-# INLINE isBase16 #-}

-- | Tell whether an untyped lazy 'ByteString' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct alphabet. To check whether it is a true
-- Base16 encoded 'ByteString' value, use 'isBase16'.
--
-- === __Examples__:
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: ByteString -> Bool
isValidBase16 = all (`elem` "0123456789abcdefABCDEF")
{-# INLINE isValidBase16 #-}
