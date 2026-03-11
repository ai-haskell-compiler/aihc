module HaskellWorks.Data.Excess.PartialMinExcess0
  ( PartialMinExcess0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.MinExcess

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMinExcess0 a where
  partialMinExcess0 :: Int -> a -> MinExcess

instance PartialMinExcess0 Word8 where
  partialMinExcess0 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where partialMinExcessMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Lo (n * 256 + fromIntegral w)
          partialMinExcessAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Ex (n * 256 + fromIntegral w)
  {-# INLINE partialMinExcess0 #-}

instance PartialMinExcess0 Word16 where
  partialMinExcess0 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess0 8       (fromIntegral  w        :: Word8)
          MinExcess bMin bAll = partialMinExcess0 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess0 #-}

instance PartialMinExcess0 Word32 where
  partialMinExcess0 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess0 16       (fromIntegral  w         :: Word16)
          MinExcess bMin bAll = partialMinExcess0 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess0 #-}

instance PartialMinExcess0 Word64 where
  partialMinExcess0 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess0 32       (fromIntegral  w         :: Word32)
          MinExcess bMin bAll = partialMinExcess0 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess0 #-}
