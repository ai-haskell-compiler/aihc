{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Take
    ( Container(..)
    , Take(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Positioning

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Take v where
  take :: Count -> v -> v

instance Take [a] where
  take = L.take . fromIntegral
  {-# INLINE take #-}

instance Take BS.ByteString where
  take = BS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Word8) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Word16) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Word32) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Word64) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Word8) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Word16) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Word32) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Word64) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Int8) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Int16) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Int32) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DV.Vector Int64) where
  take = DV.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Int8) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Int16) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Int32) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Int64) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}

instance Take (DVS.Vector Int) where
  take = DVS.take . fromIntegral
  {-# INLINE take #-}
