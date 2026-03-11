-- |
-- Utilities for the UTF-8 encoding.
module Iri.Utf8CodePoint where

import Iri.Prelude

-- |
-- Church encoding of a UTF8-encoded character.
type Utf8CodePoint =
  forall a.
  (Word8 -> a) ->
  (Word8 -> Word8 -> a) ->
  (Word8 -> Word8 -> Word8 -> a) ->
  (Word8 -> Word8 -> Word8 -> Word8 -> a) ->
  a

{-# INLINE char #-}
char :: Char -> Utf8CodePoint
char a =
  unicodeCodePoint (ord a)

{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Utf8CodePoint
unicodeCodePoint x f1 f2 f3 f4 =
  if x <= 0x7F
    then f1 (fromIntegral x)
    else
      if x <= 0x07FF
        then
          f2
            (fromIntegral ((x `shiftR` 6) + 0xC0))
            (fromIntegral ((x .&. 0x3F) + 0x80))
        else
          if x <= 0xFFFF
            then
              f3
                (fromIntegral (x `shiftR` 12) + 0xE0)
                (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80)
                (fromIntegral (x .&. 0x3F) + 0x80)
            else
              f4
                (fromIntegral (x `shiftR` 18) + 0xF0)
                (fromIntegral ((x `shiftR` 12) .&. 0x3F) + 0x80)
                (fromIntegral ((x `shiftR` 6) .&. 0x3F) + 0x80)
                (fromIntegral (x .&. 0x3F) + 0x80)
