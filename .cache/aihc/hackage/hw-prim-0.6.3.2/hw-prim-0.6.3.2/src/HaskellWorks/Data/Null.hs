{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Null
    ( Null(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container a => Null a where
  null :: a -> Bool

instance Null [a] where
  null = L.null
  {-# INLINE null #-}

instance Null BS.ByteString where
  null = BS.null
  {-# INLINE null #-}

instance Null (DV.Vector Word8) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Word16) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Word32) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Word64) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DVS.Vector Word8) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Word16) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Word32) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Word64) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DV.Vector Int8) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Int16) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Int32) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DV.Vector Int64) where
  null = DV.null
  {-# INLINE null #-}

instance Null (DVS.Vector Int8) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Int16) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Int32) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Int64) where
  null = DVS.null
  {-# INLINE null #-}

instance Null (DVS.Vector Int) where
  null = DVS.null
  {-# INLINE null #-}
