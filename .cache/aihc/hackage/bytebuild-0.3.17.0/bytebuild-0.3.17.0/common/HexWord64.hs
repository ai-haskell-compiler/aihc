{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module HexWord64
  ( word64PaddedUpperHex
  ) where

-- We have to jump through some hoops to manually do worker-wrapper
-- since CPR doesn't work on nested products. Sadly, even with all
-- the hoop jumping, the explicit loop used here is still outperformed
-- by just inlining the loop.

import Data.Bits
import Data.Bytes.Builder.Bounded.Unsafe (Builder, construct)
import Data.Primitive
import Data.Word
import GHC.Exts
import GHC.ST (ST (ST))

import qualified Control.Monad.Primitive as PM

type ST# s (a :: TYPE (r :: RuntimeRep)) = State# s -> (# State# s, a #)

word64PaddedUpperHex :: Word64 -> Builder 16
word64PaddedUpperHex w = construct $ \a b ->
  ST
    ( \s0 -> case word64PaddedUpperHexLoop w 60 a b s0 of
        (# s1, i #) -> (# s1, I# i #)
    )

word64PaddedUpperHexLoop :: forall s. Word64 -> Int -> MutableByteArray s -> Int -> ST# s Int#
word64PaddedUpperHexLoop !w !shiftAmount !arr !i@(I# i#) s0 =
  if shiftAmount >= 0
    then case PM.internal @(ST s) (writeByteArray arr i (toHexUpper (unsafeShiftR w shiftAmount))) s0 of
      (# s1, (_ :: ()) #) -> word64PaddedUpperHexLoop w (shiftAmount - 4) arr (i + 1) s1
    else (# s0, i# #)

toHexUpper :: Word64 -> Word8
toHexUpper w' =
  fromIntegral $
    (complement theMask .&. loSolved)
      .|. (theMask .&. hiSolved)
 where
  w = w' .&. 0xF
  -- This is all ones if the value was >= 10
  theMask = (1 .&. unsafeShiftR (w - 10) 63) - 1
  loSolved = w + 48
  hiSolved = w + 55
