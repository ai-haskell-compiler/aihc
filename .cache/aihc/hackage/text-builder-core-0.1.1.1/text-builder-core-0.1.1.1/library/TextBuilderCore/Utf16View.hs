module TextBuilderCore.Utf16View where

import TextBuilderCore.Prelude
import qualified TextBuilderCore.Utf16View.Unicode as Unicode

-- |
-- A matching function, which chooses the continuation to run.
type Utf16View =
  forall x. (Word16 -> x) -> (Word16 -> Word16 -> x) -> x

{-# INLINE char #-}
char :: Char -> Utf16View
char x =
  unicodeCodepoint (ord x)

{-# INLINE unicodeCodepoint #-}
unicodeCodepoint :: Int -> Utf16View
unicodeCodepoint x case1 case2 =
  if x < 0x10000
    then case1 (fromIntegral x)
    else case2 case2Unit1 case2Unit2
  where
    m =
      x - 0x10000
    case2Unit1 =
      fromIntegral (shiftR m 10 + 0xD800)
    case2Unit2 =
      fromIntegral ((m .&. 0x3FF) + 0xDC00)

{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Utf16View
utf8CodeUnits1 x case1 _ =
  case1 (fromIntegral x)

{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Utf16View
utf8CodeUnits2 byte1 byte2 case1 _ =
  case1 (shiftL (fromIntegral byte1 - 0xC0) 6 + fromIntegral byte2 - 0x80)

{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Utf16View
utf8CodeUnits3 byte1 byte2 byte3 =
  unicodeCodepoint (Unicode.utf8CodeUnits3 byte1 byte2 byte3)

{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Utf16View
utf8CodeUnits4 byte1 byte2 byte3 byte4 =
  unicodeCodepoint (Unicode.utf8CodeUnits4 byte1 byte2 byte3 byte4)
