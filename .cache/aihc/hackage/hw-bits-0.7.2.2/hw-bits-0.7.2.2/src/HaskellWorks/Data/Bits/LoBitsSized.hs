module HaskellWorks.Data.Bits.LoBitsSized
  ( LoBitsSized(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

class LoBitsSized a where
  -- | Value with the n least significant bits set to 1.
  loBitsSized :: Count -> a

instance LoBitsSized Word64 where
  loBitsSized n = let o = fromIntegral (64 - n) in 0xFFFFFFFFFFFFFFFF .<. o .>. o

instance LoBitsSized Word32 where
  loBitsSized n = let o = fromIntegral (32 - n) in 0xFFFFFFFF .<. o .>. o

instance LoBitsSized Word16 where
  loBitsSized n = let o = fromIntegral (16 - n) in 0xFFFF .<. o .>. o

instance LoBitsSized Word8 where
  loBitsSized n = let o = fromIntegral (8 - n) in 0xFF .<. o .>. o
