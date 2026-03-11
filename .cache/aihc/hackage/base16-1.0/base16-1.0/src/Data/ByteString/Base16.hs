{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base16
-- Copyright    : (c) 2020-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base16
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base16
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
import Data.ByteString (ByteString, all, elem)
import Data.ByteString.Base16.Internal.Head
import Data.Either
import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- $setup
--
-- >>> import Data.Base16.Types
-- >>> :set -XOverloadedStrings
--

-- | Encode a 'ByteString' value as Base16 'Text'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16 "Sun"
-- "53756e"
--
encodeBase16 :: ByteString -> Base16 Text
encodeBase16 = fmap T.decodeUtf8 . encodeBase16'
{-# INLINE encodeBase16 #-}

-- | Encode a 'ByteString' value as a Base16 'ByteString' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> encodeBase16' "Sun"
-- "53756e"
--
encodeBase16' :: ByteString -> Base16 ByteString
encodeBase16' = assertBase16 . encodeBase16_
{-# INLINE encodeBase16' #-}

-- | Decode a Base16-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16 $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16 :: Base16 ByteString -> ByteString
decodeBase16 = decodeBase16Typed_
{-# INLINE decodeBase16 #-}

-- | Decode Base16 'Text'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- >>> decodeBase16' $ assertBase16 "53756e"
-- "Sun"
--
decodeBase16' :: Base16 Text -> ByteString
decodeBase16' = decodeBase16Typed_ . fmap T.encodeUtf8
{-# INLINE decodeBase16' #-}

-- | Decode an untyped Base16-encoded 'ByteString' value with error-checking.
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
decodeBase16Untyped :: ByteString -> Either Text ByteString
decodeBase16Untyped = decodeBase16_
{-# INLINE decodeBase16Untyped #-}

-- | Decode a Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails
--
-- N.B.: this is not RFC 4648-compliant
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
decodeBase16Lenient = decodeBase16Lenient_
{-# INLINE decodeBase16Lenient #-}

-- | Tell whether an untyped 'ByteString' value is base16 encoded.
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

-- | Tell whether an untyped 'ByteString' value is a valid Base16 format.
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
