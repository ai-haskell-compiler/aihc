module Data.Word
  ( Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    byteSwap16,
    byteSwap32,
    byteSwap64,
    bitReverse8,
    bitReverse16,
    bitReverse32,
    bitReverse64,
  )
where

import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..), bitReverse16, bitReverse32, bitReverse64, bitReverse8, byteSwap16, byteSwap32, byteSwap64)
