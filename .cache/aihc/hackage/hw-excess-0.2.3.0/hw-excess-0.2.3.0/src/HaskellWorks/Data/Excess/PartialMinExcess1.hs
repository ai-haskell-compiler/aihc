module HaskellWorks.Data.Excess.PartialMinExcess1
  ( PartialMinExcess1(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Excess.MinExcess

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Excess.Internal.Branchless    as BL
import qualified HaskellWorks.Data.Excess.Internal.Partial.Table as T

class PartialMinExcess1 a where
  partialMinExcess1 :: Int -> a -> MinExcess

instance PartialMinExcess1 Word8 where
  partialMinExcess1 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where partialMinExcessMin = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Lo (n * 256 + fromIntegral w)
          partialMinExcessAll = fromIntegral $ DVS.unsafeIndex T.tableWord8Excess1Ex (n * 256 + fromIntegral w)
  {-# INLINE partialMinExcess1 #-}

instance PartialMinExcess1 Word16 where
  partialMinExcess1 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess1 8       (fromIntegral  w        :: Word8)
          MinExcess bMin bAll = partialMinExcess1 (n - 8) (fromIntegral (w .>. 8) :: Word8)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess1 #-}

instance PartialMinExcess1 Word32 where
  partialMinExcess1 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess1 16       (fromIntegral  w         :: Word16)
          MinExcess bMin bAll = partialMinExcess1 (n - 16) (fromIntegral (w .>. 16) :: Word16)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess1 #-}

instance PartialMinExcess1 Word64 where
  partialMinExcess1 n w = MinExcess partialMinExcessMin partialMinExcessAll
    where MinExcess aMin aAll = partialMinExcess1 32       (fromIntegral  w         :: Word32)
          MinExcess bMin bAll = partialMinExcess1 (n - 32) (fromIntegral (w .>. 32) :: Word32)
          partialMinExcessMin = if n <= 8 then aMin else BL.minInt aMin (aAll + bMin)
          partialMinExcessAll = if n <= 8 then aAll else aAll + bAll
  {-# INLINE partialMinExcess1 #-}
