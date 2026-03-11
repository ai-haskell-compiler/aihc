{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Generate
  ( Container(..)
  , Generate(..)
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

-- | Class of values that support vector like operations
class Container v => Generate v where
  generate :: Int -> (Int -> Elem v) -> v

instance Generate String where
  generate n f = f `fmap` [0 .. (n - 1)]
  {-# INLINE generate #-}

instance Generate BS.ByteString where
  generate n f = fst (BS.unfoldrN n go 0)
    where go i = if i /= n then Just (f i, i + 1) else Nothing
  {-# INLINE generate #-}

instance Generate (DV.Vector Word8) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Word16) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Word32) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Word64) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Word8) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Word16) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Word32) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Word64) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Int8) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Int16) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Int32) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DV.Vector Int64) where
  generate = DV.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Int8) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Int16) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Int32) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Int64) where
  generate = DVS.generate
  {-# INLINE generate #-}

instance Generate (DVS.Vector Int) where
  generate = DVS.generate
  {-# INLINE generate #-}
