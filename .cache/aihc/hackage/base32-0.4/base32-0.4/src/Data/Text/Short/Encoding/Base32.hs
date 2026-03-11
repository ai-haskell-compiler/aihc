{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.Text.Short.Encoding.Base32
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators
-- implementing the RFC 4648 specification for the Base32
-- encoding format. This includes strictly padded/unpadded decoding
-- variants, and external + internal validations for canonicity.
--
module Data.Text.Short.Encoding.Base32
( -- * Encoding
  encodeBase32
, encodeBase32Unpadded
  -- * Decoding
, decodeBase32
, decodeBase32With
, decodeBase32Unpadded
, decodeBase32UnpaddedWith
, decodeBase32Padded
, decodeBase32PaddedWith
--, decodeBase32Lenient
  -- * Validation
, isBase32
, isValidBase32
) where


import Data.Bifunctor (first)
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Base32 as BS32
import Data.Text (Text)
import qualified Data.Text.Encoding.Base32 as B32T
import Data.Text.Encoding.Base32.Error
import Data.Text.Short
import Data.Text.Short.Unsafe

-- | Encode a 'ShortText' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32 :: ShortText -> ShortText
encodeBase32 = fromByteStringUnsafe
  . B32.encodeBase32'
  . toByteString
{-# INLINE encodeBase32 #-}

-- | Decode an arbitrarily padded Base32-encoded 'ShortText' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base32 encodings are optionally padded.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'Data.Text.decodeUtf8'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
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
decodeBase32 :: ShortText -> Either Text ShortText
decodeBase32 = fmap fromText . B32T.decodeBase32 . toText
{-# INLINE decodeBase32 #-}

-- | Attempt to decode a 'ShortByteString' value as Base32, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' '(fmap fromText . T.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase32With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32With f t = case BS32.decodeBase32 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'ShortText' value in Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "KN2W4"
--
encodeBase32Unpadded :: ShortText -> ShortText
encodeBase32Unpadded = fromByteStringUnsafe
  . B32.encodeBase32Unpadded'
  . toByteString
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32 encoded 'ShortText' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeUtf8'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32UnpaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Unpadded "KN2W4"
-- Right "Sun"
--
-- >>> decodeBase32Unpadded "KN2W4==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Unpadded :: ShortText -> Either Text ShortText
decodeBase32Unpadded = fmap fromText . B32T.decodeBase32Unpadded . toText
{-# INLINE decodeBase32Unpadded #-}

-- | Attempt to decode an unpadded 'ShortByteString' value as Base32, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32UnpaddedWith' '(fmap fromText . T.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase32UnpaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32UnpaddedWith f t = case BS32.decodeBase32Unpadded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32 encoded 'ShortText' value
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeUtf8'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32PaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Padded "KN2W4==="
-- Right "Sun"
--
-- >>> decodeBase32Padded "KN2W4"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32Padded :: ShortText -> Either Text ShortText
decodeBase32Padded = fmap fromText . B32T.decodeBase32Padded . toText
{-# INLINE decodeBase32Padded #-}

-- | Attempt to decode a padded 'ShortByteString' value as Base32, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4328#section-6 RFC-4328 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' '(fmap fromText . T.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase32PaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32PaddedWith f t = case BS32.decodeBase32Padded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- -- | Leniently decode an unpadded Base32-encoded 'ShortText'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4328-compliant.
-- --
-- -- === __Examples__:
-- --
-- -- >>> decodeBase32Lenient "PDw_Pj4="
-- -- "<<?>>"
-- --
-- -- >>> decodeBase32Lenient "PDw_%%%$}Pj4"
-- -- "<<?>>"
-- --
-- decodeBase32Lenient :: ShortText -> ShortText
-- decodeBase32Lenient = fromText . B32T.decodeBase32Lenient . toText
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ShortText' value is Base32-encoded.
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
isBase32 :: ShortText -> Bool
isBase32 = B32.isBase32 . toByteString
{-# INLINE isBase32 #-}

-- | Tell whether a 'ShortText' value is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ShortText' value, use 'isBase32'.
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
isValidBase32 :: ShortText -> Bool
isValidBase32 = B32.isValidBase32 . toByteString
{-# INLINE isValidBase32 #-}
