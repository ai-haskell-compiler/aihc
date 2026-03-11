module HaskellWorks.Data.Excess.PartialMinMaxExcess1
  ( PartialMinMaxExcess1(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.Triplet

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMinMaxExcess1 a where
  partialMinMaxExcess1 :: Int -> a -> Triplet

instance PartialMinMaxExcess1 Word8 where
  partialMinMaxExcess1 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where partialMinMaxExcessMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Lo (n * 256 + fromIntegral w)
          partialMinMaxExcessAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Ex (n * 256 + fromIntegral w)
          partialMinMaxExcessMax = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Hi (n * 256 + fromIntegral w)
  {-# INLINE partialMinMaxExcess1 #-}

instance PartialMinMaxExcess1 Word16 where
  partialMinMaxExcess1 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess1 8       (fromIntegral  w        :: Word8)
          Triplet bMin bAll bMax = partialMinMaxExcess1 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess1 #-}

instance PartialMinMaxExcess1 Word32 where
  partialMinMaxExcess1 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess1 16       (fromIntegral  w         :: Word16)
          Triplet bMin bAll bMax = partialMinMaxExcess1 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess1 #-}

instance PartialMinMaxExcess1 Word64 where
  partialMinMaxExcess1 n w = Triplet partialMinMaxExcessMin partialMinMaxExcessAll partialMinMaxExcessMax
    where Triplet aMin aAll aMax = partialMinMaxExcess1 32       (fromIntegral  w         :: Word32)
          Triplet bMin bAll bMax = partialMinMaxExcess1 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMinMaxExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinMaxExcessAll = if n <= 8 then aAll else aAll + bAll
          partialMinMaxExcessMax = if n <= 8 then aMax else BL.maxInt aMax (aAll + bMax)
  {-# INLINE partialMinMaxExcess1 #-}
