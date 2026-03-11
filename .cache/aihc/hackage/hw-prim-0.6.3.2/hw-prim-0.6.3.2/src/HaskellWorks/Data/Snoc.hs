{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Snoc
    ( Snoc(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Snoc v where
  snoc :: v -> Elem v -> v

instance Snoc [a] where
  snoc v a = v ++ [a]
  {-# INLINE snoc #-}

instance Snoc BS.ByteString where
  snoc = BS.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Word8) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Word16) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Word32) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Word64) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Word8) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Word16) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Word32) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Word64) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Int8) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Int16) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Int32) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DV.Vector Int64) where
  snoc = DV.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Int8) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Int16) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Int32) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Int64) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}

instance Snoc (DVS.Vector Int) where
  snoc = DVS.snoc
  {-# INLINE snoc #-}
