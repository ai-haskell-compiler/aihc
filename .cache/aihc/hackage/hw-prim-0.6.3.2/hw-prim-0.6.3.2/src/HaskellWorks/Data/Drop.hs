{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Drop
    ( Container(..)
    , Drop(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Positioning
import Prelude                       hiding (drop)

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Drop v where
  drop :: Count -> v -> v

instance Drop [a] where
  drop = L.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop BS.ByteString where
  drop = BS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Word8) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Word16) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Word32) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Word64) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Word8) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Word16) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Word32) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Word64) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Int8) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Int16) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Int32) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DV.Vector Int64) where
  drop = DV.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Int8) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Int16) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Int32) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Int64) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}

instance Drop (DVS.Vector Int) where
  drop = DVS.drop . fromIntegral
  {-# INLINE drop #-}
