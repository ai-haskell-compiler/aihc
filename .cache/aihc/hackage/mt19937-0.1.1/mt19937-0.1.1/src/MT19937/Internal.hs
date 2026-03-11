module MT19937.Internal where

import Data.Bits
import Data.Word ( Word32 )
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | MT19937 tempering function.
temper :: (Num a, Bits a) => a -> a
temper x = z
  where
    y1 = x  `xor` ((x  `shiftR` u) .&. d)
    y2 = y1 `xor` ((y1 `shiftL` s) .&. b)
    y3 = y2 `xor` ((y2 `shiftL` t) .&. c)
    z  = y3 `xor`  (y3 `shiftR` l)
    u = 11
    d = 0xFFFFFFFF
    s = 7
    b = 0x9D2C5680
    t = 15
    c = 0xEFC60000
    l = 18

-- | Twist an MT19937 state vector.
twist :: VUM.PrimMonad m => VUM.MVector (VUM.PrimState m) Word32 -> m ()
twist mt = go 0
  where
    m = 397
    a = 0x9908B0DF
    go = \case
      624 -> pure ()
      i   -> do
        mti  <- VUM.unsafeRead mt i
        mti1 <- VUM.unsafeRead mt ((i+1) `mod` 624)
        mtim <- VUM.unsafeRead mt ((i+m) `mod` 624)
        let x    = (mti .&. 0x80000000) + (mti1 .&. 0x7FFFFFFF)
            mti' = mtim `xor` (x `shiftR` 1)
        if   x .&. 1 == 0
        then VUM.unsafeWrite mt i mti'
        else VUM.unsafeWrite mt i (mti' `xor` a)
        go (i+1)
