{-|
Module      : Data.Bits.Pext.Prim
Description : Parallel extract operations (emulated)
Copyright   : (c) John Ky, 2018-2019
License     : BSD-3-Clause
Maintainer  : newhoggy@gmail.com
Stability   : stable
-}
module Data.Bits.Pext.Slow
  ( SlowPext(..)
  ) where

import Data.Bits
import GHC.Word

slowPext64
  :: Word64 -- ^ the bitmap from which bits will be extracted
  -> Word64 -- ^ the bitmap selecting the bits that are to be extracted
  -> Word64 -- ^ the bitmap containing the extract bits with higher-order bits cleared
slowPext64 = slowPext64' 0 0 0

slowPext64' :: Word64 -> Int -> Int -> Word64 -> Word64 -> Word64
slowPext64' result offset index src mask = if index /= 64
  then if maskBit /= 0
          then slowPext64' nextResult (offset + 1) (index + 1) src mask
          else slowPext64' result      offset      (index + 1) src mask
  else result
  where srcBit      = (src  `shiftR` index) .&. 1
        maskBit     = (mask `shiftR` index) .&. 1
        nextResult  = result .|. (srcBit `shiftL` offset)

-- | Bitwise parallel extract (emulated).  Extract bits from the source at
-- the locations described by the mask.
class SlowPext a where
  slowPext
    :: a -- ^ the bitmap from which bits will be extracted
    -> a -- ^ the bitmap selecting the bits that are to be extracted
    -> a -- ^ the bitmap containing the extract bits with higher-order bits cleared

instance SlowPext Word where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word8 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word16 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word32 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word64 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))
