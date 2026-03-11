{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base32
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded
-- decoding variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base32
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


import Prelude hiding (all, elem)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Base32.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (elem, fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a 'ByteString' value as a Base32 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32 :: ByteString -> TL.Text
encodeBase32 = TL.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = fromChunks
  . fmap B32.encodeBase32'
  . reChunkN 5
  . toChunks

-- | Decode an arbitrarily padded Base32 encoded 'ByteString' value. If its length is not a multiple
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
decodeBase32 :: ByteString -> Either T.Text ByteString
decodeBase32 = fmap (fromChunks . (:[]))
  . B32.decodeBase32
  . BS.concat
  . toChunks
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' value as Base32 'Text' without padding. Note that for Base32,
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
encodeBase32Unpadded :: ByteString -> TL.Text
encodeBase32Unpadded = TL.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as Base32 without padding. Note that for Base32,
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
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = fromChunks
  . fmap B32.encodeBase32Unpadded'
  . reChunkN 5
  . toChunks

-- | Decode an unpadded Base32-encoded 'ByteString' value. Input strings are
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
decodeBase32Unpadded :: ByteString -> Either T.Text ByteString
decodeBase32Unpadded = fmap (fromChunks . (:[]))
  . B32.decodeBase32Unpadded
  . BS.concat
  . toChunks
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32-encoded 'ByteString' value. Input strings are
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
decodeBase32Padded :: ByteString -> Either T.Text ByteString
decodeBase32Padded = fmap (fromChunks . (:[]))
  . B32.decodeBase32Padded
  . BS.concat
  . toChunks
{-# INLINE decodeBase32Padded #-}

-- -- | Leniently decode an unpadded Base32-encoded 'ByteString'. This function
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
--     . fmap B32.decodeBase32Lenient
--     . reChunkN 8
--     . fmap (BS.filter (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567="))
--     . toChunks
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ByteString' is Base32-encoded.
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
isBase32 :: ByteString -> Bool
isBase32 bs = isValidBase32 bs && isRight (decodeBase32 bs)
{-# INLINE isBase32 #-}

-- | Tell whether a 'ByteString' is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ByteString' value, use 'isBase32'.
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
isValidBase32 :: ByteString -> Bool
isValidBase32 = go . toChunks
  where
    go [] = True
    go [c] = B32.isValidBase32 c
    go (c:cs) = -- note the lack of padding char
      BS.all (flip elem "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567") c
      && go cs
{-# INLINE isValidBase32 #-}
