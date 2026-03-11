module TextBuilderCore.Utf8View where

import TextBuilderCore.Prelude

-- |
-- A matching function, which chooses the continuation to run.
type Utf8View =
  forall x.
  (Word8 -> x) ->
  (Word8 -> Word8 -> x) ->
  (Word8 -> Word8 -> Word8 -> x) ->
  (Word8 -> Word8 -> Word8 -> Word8 -> x) ->
  x

{-# INLINE unicodeCodepoint #-}
unicodeCodepoint :: Int -> Utf8View
unicodeCodepoint x case1 case2 case3 case4
  | x < 0x80 = case1 (fromIntegral x)
  | x < 0x800 =
      case2
        (fromIntegral $ x `shiftR` 6 .|. 0xC0)
        (fromIntegral $ (x .&. 0x3F) .|. 0x80)
  | x < 0x10000 =
      case3
        (fromIntegral $ x `shiftR` 12 .|. 0xE0)
        (fromIntegral $ (x `shiftR` 6) .&. 0x3F .|. 0x80)
        (fromIntegral $ (x .&. 0x3F) .|. 0x80)
  | otherwise =
      case4
        (fromIntegral $ x `shiftR` 18 .|. 0xF0)
        (fromIntegral $ (x `shiftR` 12) .&. 0x3F .|. 0x80)
        (fromIntegral $ (x `shiftR` 6) .&. 0x3F .|. 0x80)
        (fromIntegral $ (x .&. 0x3F) .|. 0x80)
