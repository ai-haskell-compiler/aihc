{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base32
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Short.Base32
( -- * Encoding
  encodeBase32
, encodeBase32'
, encodeBase32Unpadded
, encodeBase32Unpadded'
  -- * Decoding
, decodeBase32
, decodeBase32Unpadded
, decodeBase32Padded
--, decodeBase32Lenient
  -- * Validation
, isBase32
, isValidBase32
) where


import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)


-- | Encode a 'ShortByteString' value as a Base32 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32 :: ShortByteString -> ShortText
encodeBase32 = fromShortByteStringUnsafe . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ShortByteString' as a Base32 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "KN2W4==="
--
encodeBase32' :: ShortByteString -> ShortByteString
encodeBase32' = toShort . B32.encodeBase32' . fromShort

-- | Decode an arbitrarily padded Base32 encoded 'ShortByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base32-encoded values are optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32 "KN2W4==="
-- Right "Sun"
--
-- >>> decodeBase32 "KN2W4"
-- Right "Sun"
--
-- >>> decodeBase32 "KN2W==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32 :: ShortByteString -> Either Text ShortByteString
decodeBase32 = fmap toShort . B32.decodeBase32 . fromShort

{-# INLINE decodeBase32 #-}

-- | Encode a 'ShortByteString' value as Base32 'Text' without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "KN2W4"
--
encodeBase32Unpadded :: ShortByteString -> ShortText
encodeBase32Unpadded = fromShortByteStringUnsafe . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ShortByteString' value as Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "KN2W4"
--
encodeBase32Unpadded' :: ShortByteString -> ShortByteString
encodeBase32Unpadded' = toShort . B32.encodeBase32Unpadded' . fromShort

-- | Decode an unpadded Base32-encoded 'ShortByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base32 is explicitly required, it is
-- safer to call 'decodeBase32'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Unpadded "KN2W4"
-- Right "Sun"
--
-- >>> decodeBase32Unpadded "KN2W4==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Unpadded :: ShortByteString -> Either Text ShortByteString
decodeBase32Unpadded = fmap toShort . B32.decodeBase32Unpadded . fromShort
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32-encoded 'ShortByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base32 is explicitly required, it is
-- safer to call 'decodeBase32'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Padded "KN2W4==="
-- Right "Sun"
--
-- >>> decodeBase32Padded "KN2W4"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32Padded :: ShortByteString -> Either Text ShortByteString
decodeBase32Padded = fmap toShort . B32.decodeBase32Padded . fromShort
{-# INLINE decodeBase32Padded #-}

-- -- | Leniently decode an unpadded Base32-encoded 'ShortByteString'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- -- === __Examples__:
-- --
-- -- >>> decodeBase32Lenient "PDw_Pj4="
-- -- "<<?>>"
-- --
-- -- >>> decodeBase32Lenient "PDw_%%%$}Pj4"
-- -- "<<?>>"
-- --
-- decodeBase32Lenient :: ShortByteString -> ShortByteString
-- decodeBase32Lenient = toShort . B32.decodeBase32Lenient . fromShort
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ShortByteString' is Base32-encoded.
--
-- === __Examples__:
--
-- >>> isBase32 "KN2W4"
-- True
--
-- >>> isBase32 "KN2W4==="
-- True
--
-- >>> isBase32 "KN2W4=="
-- False
--
isBase32 :: ShortByteString -> Bool
isBase32 = B32.isBase32 . fromShort
{-# INLINE isBase32 #-}

-- | Tell whether a 'ShortByteString' is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ShortByteString' value, use 'isBase32'.
--
-- === __Examples__:
--
-- >>> isValidBase32 "KN2W4"
-- True
--
-- >>> isValidBase32 "KN2W4="
-- False
--
-- >>> isValidBase32 "KN2W4%"
-- False
--
isValidBase32 :: ShortByteString -> Bool
isValidBase32 = B32.isValidBase32 . fromShort
{-# INLINE isValidBase32 #-}
