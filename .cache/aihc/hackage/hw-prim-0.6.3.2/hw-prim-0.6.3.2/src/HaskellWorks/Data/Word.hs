{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module HaskellWorks.Data.Word
  ( ltWord64
  ) where

import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Word  hiding (ltWord64)

ltWord64 :: Word64 -> Word64 -> Word64
#if MIN_VERSION_base(4,17,0)
ltWord64 (W64# a#) (W64# b#) = fromIntegral (I# (ltWord64# a# b#))
#else
ltWord64 (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
#endif
{-# INLINE ltWord64 #-}
