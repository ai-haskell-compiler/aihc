{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Vector.StorableVectorLike
  ( StorableVectorLike(..)
  ) where

import Data.Word
import Foreign.Storable

import qualified Data.Vector.Storable as DVS

-- | Class of values that support storable vector like operations
class StorableVectorLike v e where
  sImap :: (Storable a, Storable b) => (Int -> a -> b) -> v a -> v b
  sMap :: (Storable a, Storable b) => (a -> b) -> v a -> v b
  sUnfoldr :: (Storable a) => (b -> Maybe (a, b)) -> b -> v a
  sUnfoldrN :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> v a

instance StorableVectorLike DVS.Vector Word8 where
  sImap = DVS.imap
  sMap = DVS.map
  sUnfoldr = DVS.unfoldr
  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sImap     #-}
  {-# INLINABLE sMap      #-}
  {-# INLINABLE sUnfoldr  #-}
  {-# INLINABLE sUnfoldrN #-}

instance StorableVectorLike DVS.Vector Word16 where
  sImap = DVS.imap
  sMap = DVS.map
  sUnfoldr = DVS.unfoldr
  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sImap     #-}
  {-# INLINABLE sMap      #-}
  {-# INLINABLE sUnfoldr  #-}
  {-# INLINABLE sUnfoldrN #-}

instance StorableVectorLike DVS.Vector Word32 where
  sImap = DVS.imap
  sMap = DVS.map
  sUnfoldr = DVS.unfoldr
  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sImap     #-}
  {-# INLINABLE sMap      #-}
  {-# INLINABLE sUnfoldr  #-}
  {-# INLINABLE sUnfoldrN #-}

instance StorableVectorLike DVS.Vector Word64 where
  sImap = DVS.imap
  sMap = DVS.map
  sUnfoldr = DVS.unfoldr
  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sImap     #-}
  {-# INLINABLE sMap      #-}
  {-# INLINABLE sUnfoldr  #-}
  {-# INLINABLE sUnfoldrN #-}
