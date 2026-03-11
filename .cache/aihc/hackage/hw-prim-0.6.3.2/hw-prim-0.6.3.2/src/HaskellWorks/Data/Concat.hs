{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Concat
  ( Concat(..)
  ) where

import Data.Int
import Data.Word

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

-- | Class of values that support vector like operations
class Concat v where
  concat :: [v] -> v

instance Concat [a] where
  concat = L.concat
  {-# INLINE concat #-}

instance Concat BS.ByteString where
  concat = BS.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Word8) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Word16) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Word32) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Word64) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Word8) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Word16) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Word32) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Word64) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Int8) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Int16) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Int32) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DV.Vector Int64) where
  concat = DV.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Int8) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Int16) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Int32) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Int64) where
  concat = DVS.concat
  {-# INLINE concat #-}

instance Concat (DVS.Vector Int) where
  concat = DVS.concat
  {-# INLINE concat #-}
