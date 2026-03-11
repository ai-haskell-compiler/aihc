-- |
-- Module       : Data.Text.Short.Encoding.Base16
-- Copyright    : (c) 2020-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.Text.Short.Encoding.Base16
( encodeBase16
, decodeBase16
, decodeBase16Untyped
, decodeBase16With
, decodeBase16Lenient
, isBase16
, isValidBase16
) where


import Data.Base16.Types
import Data.Bifunctor (first)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Short
import qualified Data.ByteString.Short.Base16 as BS16
import Data.Text (Text)
import Data.Text.Encoding.Base16.Error
import Data.Text.Short
import Data.Text.Short.Unsafe

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a 'ShortText' value in Base16 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ShortText -> Base16 ShortText
encodeBase16 = BS16.encodeBase16 . toShortByteString
{-# INLINE encodeBase16 #-}


-- | Decode a Base16-encoded 'ShortText' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 ShortText -> ShortText
decodeBase16 =  fromShortByteStringUnsafe
  . BS16.decodeBase16
  . fmap toShortByteString
{-# INLINE decodeBase16 #-}

-- | Decode an untyped Base16-encoded 'ShortText' value.
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
decodeBase16Untyped :: ShortText -> Either Text ShortText
decodeBase16Untyped =  fmap fromShortByteStringUnsafe
  . BS16.decodeBase16Untyped
  . toShortByteString
{-# INLINE decodeBase16Untyped #-}

-- | Attempt to decode an untyped 'ShortText' value as Base16, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Example__:
--
-- @
-- 'decodeBase16With' (fmap 'fromText' . 'Data.Text.Encoding.decodeUtf8'' . 'fromShort')
--   :: 'ShortByteString' -> 'Either' ('Base16Error' 'UnicodeException') 'ShortText'
-- @
--
-- @since 0.3.0.0
--
decodeBase16With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input to decode
    -> Either (Base16Error err) ShortText
decodeBase16With f t = case BS16.decodeBase16Untyped t of
    Left de -> Left $ DecodeError de
    Right a -> first ConversionError $ f a
{-# INLINE decodeBase16With #-}

-- | Decode an untyped Base16-encoded 'ShortText' value leniently, using a
-- strategy that never fails, catching unicode exceptions raised in the
-- process of converting to text values.
--
-- N.B.: this is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase16Lenient "53756e"
-- "Sun"
--
-- >>> decodeBase16Lenient "6x6x"
-- "f"
--
decodeBase16Lenient :: ShortText -> ShortText
decodeBase16Lenient = fromShortByteStringUnsafe
  . BS16.decodeBase16Lenient
  . toShortByteString
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether a 'ShortText' value is Base16-encoded.
--
-- === __Examples__:
--
-- >>> isBase16 "666f6"
-- False
--
-- >>> isBase16 "666f"
-- True
--
isBase16 :: ShortText -> Bool
isBase16 = B16.isBase16 . toByteString
{-# INLINE isBase16 #-}

-- | Tell whether a 'ShortText' value is a valid Base16 format.
--
-- This will not tell you whether or not this is a correct Base16 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base16 encoded 'ShortText' value, use 'isBase16'.
--
-- === __Examples__:
--
-- >>> isValidBase16 "666f+/6"
-- False
--
-- >>> isValidBase16 "666f6"
-- True
--
isValidBase16 :: ShortText -> Bool
isValidBase16 = B16.isValidBase16 . toByteString
{-# INLINE isValidBase16 #-}
