{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Empty
    ( Empty(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container a => Empty a where
  empty :: a

instance Empty [a] where
  empty = []
  {-# INLINE empty #-}

instance Empty BS.ByteString where
  empty = BS.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Word8) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Word16) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Word32) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Word64) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Word8) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Word16) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Word32) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Word64) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Int8) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Int16) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Int32) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DV.Vector Int64) where
  empty = DV.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Int8) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Int16) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Int32) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Int64) where
  empty = DVS.empty
  {-# INLINE empty #-}

instance Empty (DVS.Vector Int) where
  empty = DVS.empty
  {-# INLINE empty #-}
