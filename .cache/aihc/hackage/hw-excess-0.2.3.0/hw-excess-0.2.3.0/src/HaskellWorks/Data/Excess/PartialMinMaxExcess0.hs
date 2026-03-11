module HaskellWorks.Data.Excess.PartialMinMaxExcess0
  ( PartialMinMaxExcess0(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMinMaxExcess0 a where
  partialMinMaxExcess0 :: Int -> a -> Triplet

instance PartialMinMaxExcess0 Word8 where
  partialMinMaxExcess0 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where partialMinMaxExcessMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Lo (n * 256 + fromIntegral w)
          partialMinMaxExcessAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Ex (n * 256 + fromIntegral w)
          partialMinMaxExcessMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess0Hi (n * 256 + fromIntegral w)
  {-# INLINE partialMinMaxExcess0 #-}

instance PartialMinMaxExcess0 Word16 where
  partialMinMaxExcess0 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess0 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = partialMinMaxExcess0 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess0 #-}

instance PartialMinMaxExcess0 Word32 where
  partialMinMaxExcess0 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess0 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = partialMinMaxExcess0 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess0 #-}

instance PartialMinMaxExcess0 Word64 where
  partialMinMaxExcess0 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess0 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = partialMinMaxExcess0 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess0 #-}
