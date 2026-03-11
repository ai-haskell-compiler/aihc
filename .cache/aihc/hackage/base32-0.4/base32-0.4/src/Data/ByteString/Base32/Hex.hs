{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base32.Hex
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes padded and unpadded decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base32.Hex
( -- * Encoding
  encodeBase32
, encodeBase32'
, encodeBase32Unpadded
, encodeBase32Unpadded'
  -- * Decoding
, decodeBase32
, decodeBase32Unpadded
, decodeBase32Padded
-- , decodeBase32Lenient
  -- * Validation
, isBase32Hex
, isValidBase32Hex
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal
import Data.ByteString.Base32.Internal.Tables
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import System.IO.Unsafe (unsafeDupablePerformIO)


-- | Encode a 'ByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
encodeBase32 :: ByteString -> Text
encodeBase32 = T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32hex 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "ADQMS==="
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = encodeBase32_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32' #-}

-- | Decode an arbitrarily padded Base32hex-encoded 'ByteString' value. If its length
-- is not a multiple of 8, then padding characters will be added to fill out the
-- input to a multiple of 8 for safe decoding, as Base32hex-encoded values are
-- optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32 "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32 "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32 "ADQM==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32 :: ByteString -> Either Text ByteString
decodeBase32 bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 2 bs $ decodeBase32_ hexDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 3 bs $ decodeBase32_ hexDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 5 bs $ decodeBase32_ hexDecodeTable (BS.append bs "=")
    | otherwise = Left "Base32-encoded bytestring has invalid size"
  where
    !r = l `rem` 8
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32hex 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "ADQMS"
--
encodeBase32Unpadded :: ByteString -> Text
encodeBase32Unpadded = T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as a Base32hex 'ByteString' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "ADQMS"
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = encodeBase32NoPad_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an unpadded Base32hex-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Unpadded "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32Unpadded "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Unpadded :: ByteString -> Either Text ByteString
decodeBase32Unpadded bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = validateLastNPads 1 bs $ decodeBase32_ hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 1 bs $ decodeBase32_ hexDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 1 bs $ decodeBase32_ hexDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 1 bs $ decodeBase32_ hexDecodeTable (BS.append bs "=")
    | otherwise = Left "Base32-encoded bytestring has invalid size"
  where
    !r = l `rem` 8
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32hex-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Padded "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32Padded "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32Padded :: ByteString -> Either Text ByteString
decodeBase32Padded bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 1 = Left "Base32-encoded bytestring has invalid size"
    | r == 3 = Left "Base32-encoded bytestring has invalid size"
    | r == 6 = Left "Base32-encoded bytestring has invalid size"
    | r /= 0 = Left "Base32-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable bs
  where
    !r = l `rem` 8
{-# INLINE decodeBase32Padded #-}

-- | Tell whether a 'ByteString' value is encoded in padded or unpadded Base32hex format
--
-- === __Examples__:
--
-- >>> isBase32Hex "ADQMS"
-- True
--
-- >>> isBase32Hex "ADQMS==="
-- True
--
-- >>> isBase32Hex "ADQMS=="
-- False
--
isBase32Hex :: ByteString -> Bool
isBase32Hex bs = isValidBase32Hex bs && isRight (decodeBase32 bs)
{-# INLINE isBase32Hex #-}

-- | Tell whether a 'ByteString' value is a valid Base32hex format.
--
-- This will not tell you whether or not this is a correct Base32hex representation,
-- only that it conforms to the correct shape (including padding/size etc.).
-- To check whether it is a true Base32hex encoded 'ByteString' value, use 'isBase32'.
--
-- === __Examples__:
--
-- >>> isValidBase32Hex "ADQMS"
-- True
--
-- >>> isValidBase32Hex "ADQMS="
-- False
--
-- >>> isValidBase32Hex "ADQMS%"
-- False
--
isValidBase32Hex :: ByteString -> Bool
isValidBase32Hex = validateBase32 "0123456789ABCDEFGHIJKLMNOPQRSTUV"
{-# INLINE isValidBase32Hex #-}
