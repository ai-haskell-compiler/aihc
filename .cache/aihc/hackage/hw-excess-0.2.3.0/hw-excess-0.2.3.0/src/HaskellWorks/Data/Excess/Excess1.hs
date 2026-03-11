{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Excess.Excess1
    ( Excess1(..)
    ) where

import Data.Word
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1

import qualified Data.Vector.Storable as DVS

class Excess1 v where
  excess1 :: v -> Count -> Int

instance Excess1 Word8 where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 Word16 where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 Word32 where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 Word64 where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 (DVS.Vector Word8) where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 (DVS.Vector Word16) where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 (DVS.Vector Word32) where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}

instance Excess1 (DVS.Vector Word64) where
  excess1 v c = fromIntegral (rank1 v c) - fromIntegral (rank0 v c)
  {-# INLINE excess1 #-}
