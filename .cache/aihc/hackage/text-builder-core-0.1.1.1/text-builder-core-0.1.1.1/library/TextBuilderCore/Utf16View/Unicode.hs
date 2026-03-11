-- |
-- Utilities for construction of Unicode codepoints.
module TextBuilderCore.Utf16View.Unicode where

import TextBuilderCore.Prelude

{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Int
utf8CodeUnits3 byte1 byte2 byte3 =
  shiftL (fromIntegral byte1 - 0xE0) 12
    + shiftL (fromIntegral byte2 - 0x80) 6
    + fromIntegral byte3
    - 0x80

{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Int
utf8CodeUnits4 byte1 byte2 byte3 byte4 =
  shiftL (fromIntegral byte1 - 0xF0) 18
    + shiftL (fromIntegral byte2 - 0x80) 12
    + shiftL (fromIntegral byte3 - 0x80) 6
    + fromIntegral byte4
    - 0x80
