{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base32.Hex
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes strictly padded/unpadded
-- decoding variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base32.Hex
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
, isBase32Hex
, isValidBase32Hex
) where


import Prelude hiding (all, elem)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32.Hex as B32H
import Data.ByteString.Base32.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a 'ByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
encodeBase32 :: ByteString -> TL.Text
encodeBase32 = TL.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32hex 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "ADQMS==="
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = fromChunks
    . fmap B32H.encodeBase32'
    . reChunkN 5
    . toChunks

-- | Decode an arbitrarily padded Base32hex encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base32hex-encoded values are optionally padded.
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
-- >>> decodeBase32 "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32 :: ByteString -> Either T.Text ByteString
decodeBase32 = fmap (fromChunks . (:[]))
    . B32H.decodeBase32
    . BS.concat
    . toChunks
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' value as Base32hex 'Text' without padding. Note that for Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "ADQMS"
--
encodeBase32Unpadded :: ByteString -> TL.Text
encodeBase32Unpadded = TL.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as Base32hex without padding. Note that for Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "ADQMS"
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = fromChunks
    . fmap B32H.encodeBase32Unpadded'
    . reChunkN 5
    . toChunks

-- | Decode an unpadded Base32hex-encoded 'ByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32'.
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
decodeBase32Unpadded :: ByteString -> Either T.Text ByteString
decodeBase32Unpadded = fmap (fromChunks . (:[]))
    . B32H.decodeBase32Unpadded
    . BS.concat
    . toChunks
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32hex-encoded 'ByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32'.
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
decodeBase32Padded :: ByteString -> Either T.Text ByteString
decodeBase32Padded = fmap (fromChunks . (:[]))
    . B32H.decodeBase32Padded
    . BS.concat
    . toChunks
{-# INLINE decodeBase32Padded #-}

-- -- | Leniently decode an unpadded Base32hex-encoded 'ByteString'. This function
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
-- decodeBase32Lenient :: ByteString -> ByteString
-- decodeBase32Lenient = fromChunks
--     . fmap B32H.decodeBase32Lenient
--     . reChunkN 8
--     . fmap (BS.filter (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567="))
--     . toChunks
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ByteString' is Base32hex-encoded.
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

-- | Tell whether a 'ByteString' is a valid Base32hex format.
--
-- This will not tell you whether or not this is a correct Base32hex representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32hex encoded 'ByteString' value, use 'isBase32Hex'.
--
-- === __Examples__:
--
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
isValidBase32Hex = go . toChunks
  where
    go [] = True
    go [c] = B32H.isValidBase32Hex c
    go (c:cs) = -- note the lack of padding char
      BS.all (flip elem "0123456789ABCDEFGHIJKLMNOPQRSTUV") c
      && go cs
{-# INLINE isValidBase32Hex #-}
