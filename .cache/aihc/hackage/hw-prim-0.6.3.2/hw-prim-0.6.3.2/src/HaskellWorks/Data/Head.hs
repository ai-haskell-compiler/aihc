{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Head
    ( Container(..)
    , Head(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container
import Prelude                     hiding (head)

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Head v where
  head :: v -> Elem v

instance Head [a] where
  head = L.head
  {-# INLINE head #-}

instance Head BS.ByteString where
  head = BS.head
  {-# INLINE head #-}

instance Head (DV.Vector Word8) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Word16) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Word32) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Word64) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DVS.Vector Word8) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Word16) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Word32) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Word64) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DV.Vector Int8) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Int16) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Int32) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DV.Vector Int64) where
  head = DV.head
  {-# INLINE head #-}

instance Head (DVS.Vector Int8) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Int16) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Int32) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Int64) where
  head = DVS.head
  {-# INLINE head #-}

instance Head (DVS.Vector Int) where
  head = DVS.head
  {-# INLINE head #-}
