{-# LANGUAGE TypeApplications #-}

{- | Zigzag encoding maps signed integers to unsigned integers so that numbers
with a small absolute value (for instance, -1) have a small varint encoded
value too. It does this in a way that "zig-zags" back and forth through the
positive and negative integers, so that -1 is encoded as 1, 1 is encoded as
2, -2 is encoded as 3, and so on.

> zigzag(n) = { 2 * n       if 0 <= n
>             { -2 * n - 1  if n < 0

This description was adapted from
https://developers.google.com/protocol-buffers/docs/encoding#signed-ints
which is released under https://creativecommons.org/licenses/by/4.0/
-}
module Data.Word.Zigzag
  ( toZigzag
  , fromZigzag
  , toZigzagNative
  , fromZigzagNative
  , toZigzag32
  , fromZigzag32
  , toZigzag64
  , fromZigzag64
  ) where

import Data.Bits (complement, unsafeShiftL, unsafeShiftR, xor, (.&.))
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)

{- | Encode a big integer with zigzag.

If you know the size of the data, it is likely more efficient to use one of
'toZigzagNative', 'toZigzag32', or 'toZigzag64'.
-}
toZigzag :: Integer -> Natural
toZigzag n
  | 0 <= n = fromIntegral $ 2 * n
  | otherwise = fromIntegral $ (-2) * n - 1

{- | Decode a zigzag-encoded big ingeter.

If you know the size of the data, it is likely more efficient to use one of
'fromZigzagNative', 'fromZigzag32', or 'fromZigzag64'.
-}
fromZigzag :: Natural -> Integer
fromZigzag n
  | even n = fromIntegral $ n `div` 2
  | otherwise = negate . fromIntegral $ (n + 1) `div` 2

{- | Encode a native-size integer with zigzag.

In C, this is:

> (n << 1) ^ (n >> (CHAR_BIT * sizeof(int) - 1))
-}
toZigzagNative :: Int -> Word
toZigzagNative = fromIntegral . toZigzag64 . fromIntegral

{- | Decode a native-size zigzag-encoded integer.

In C, this is:

> (n >> 1) ^ (~(n & 1) + 1)
-}
fromZigzagNative :: Word -> Int
fromZigzagNative = fromIntegral . fromZigzag64 . fromIntegral

{- | Encode a 32-bit integer with zigzag.

In C, this is:

> (n << 1) ^ (n >> 31)
-}
toZigzag32 :: Int32 -> Word32
toZigzag32 = fromIntegral . toZigzag64 . fromIntegral

{- | Decode a 32-bit zigzag-encoded integer.

In C, this is:

> (n >> 1) ^ (~(n & 1) + 1)
-}
fromZigzag32 :: Word32 -> Int32
fromZigzag32 = fromIntegral . fromZigzag64 . fromIntegral

{- | Encode a 64-bit integer with zigzag.

In C, this is:

> (n << 1) ^ (n >> 63)
-}
toZigzag64 :: Int64 -> Word64
toZigzag64 i =
  fromIntegral @Int64 @Word64 $
    (i `unsafeShiftL` 1) `xor` (i `unsafeShiftR` 63)

{- | Decode a 64-bit zigzag-encoded integer.

In C, this is:

> (n >> 1) ^ (~(n & 1) + 1)
-}
fromZigzag64 :: Word64 -> Int64
fromZigzag64 n =
  fromIntegral $
    (n `unsafeShiftR` 1) `xor` (complement (n .&. 1) + 1)
