{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Word where

import Data.Word
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise

class WordConcat a where
  type DoubleWords a
  -- | Concatenate two words in little-endian order of the same size into a
  -- word of twice the size.
  leConcat :: a -> a -> DoubleWords a

class WordSplit a where
  type HalfWords a
  -- | Split a word equally into two smaller words in little-endian order.
  leSplit :: a -> (HalfWords a, HalfWords a)

instance WordConcat Word8 where
  type DoubleWords Word8 = Word16
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance WordConcat Word16 where
  type DoubleWords Word16 = Word32
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance WordConcat Word32 where
  type DoubleWords Word32 = Word64
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance WordSplit Word64 where
  type HalfWords Word64 = Word32
  leSplit a = (fromIntegral a, fromIntegral (a .>. 32))

instance WordSplit Word32 where
  type HalfWords Word32 = Word16
  leSplit a = (fromIntegral a, fromIntegral (a .>. 16))

instance WordSplit Word16 where
  type HalfWords Word16 = Word8
  leSplit a = (fromIntegral a, fromIntegral (a .>. 8))
