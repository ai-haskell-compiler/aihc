{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Cons
    ( Cons(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Cons v where
  cons :: Elem v -> v -> v

instance Cons [a] where
  cons = (:)
  {-# INLINE cons #-}

instance Cons BS.ByteString where
  cons = BS.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Word8) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Word16) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Word32) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Word64) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Word8) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Word16) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Word32) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Word64) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Int8) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Int16) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Int32) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DV.Vector Int64) where
  cons = DV.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Int8) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Int16) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Int32) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Int64) where
  cons = DVS.cons
  {-# INLINE cons #-}

instance Cons (DVS.Vector Int) where
  cons = DVS.cons
  {-# INLINE cons #-}
