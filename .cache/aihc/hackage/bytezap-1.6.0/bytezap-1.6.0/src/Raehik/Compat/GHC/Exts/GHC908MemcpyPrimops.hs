{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

-- On supported GHCs, we simply re-export. This means unused import warnings.
-- Sorry idk how to fix that.

module Raehik.Compat.GHC.Exts.GHC908MemcpyPrimops
  ( copyAddrToAddrNonOverlapping#
  , setAddrRange#
  ) where

import GHC.Exts

#if MIN_VERSION_base(4,19,0)

-- These should all be imported from GHC.Exts, so above is simply re-exporting.

#else

import GHC.IO ( unIO )
import Foreign.Marshal.Utils ( copyBytes, fillBytes )

copyAddrToAddrNonOverlapping#
    :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld
copyAddrToAddrNonOverlapping# src# dest# len# s# =
    case unIO (copyBytes (Ptr dest#) (Ptr src#) (fromIntegral (I# len#))) s# of
      (# s'#, () #) -> s'#

setAddrRange#
    :: Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
setAddrRange# dest# w# len# s0 =
    case unIO (fillBytes (Ptr dest#) (fromIntegral (I# w#)) (I# len#)) s0 of
      (# s1, () #) -> s1

#endif
