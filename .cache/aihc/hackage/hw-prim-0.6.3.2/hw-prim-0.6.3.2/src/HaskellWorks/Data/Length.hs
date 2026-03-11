{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.Length
    ( Container(..)
    , Length(..)
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Positioning
import Prelude                       hiding (length)

import qualified Data.ByteString      as BS
import qualified Data.List            as L
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container v => Length v where
  length :: v -> Count
  end :: v -> Position
  end = fromIntegral . length
  {-# INLINE end #-}

instance Length [a] where
  length = fromIntegral . L.length
  {-# INLINE length #-}

instance Length BS.ByteString where
  length = fromIntegral . BS.length
  {-# INLINE length #-}

instance Length (DV.Vector Word8) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Word16) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Word32) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Word64) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DVS.Vector Word8) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Word16) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Word32) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Word64) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DV.Vector Int8) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Int16) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Int32) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DV.Vector Int64) where
  length = fromIntegral . DV.length
  {-# INLINE length #-}

instance Length (DVS.Vector Int8) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Int16) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Int32) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Int64) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}

instance Length (DVS.Vector Int) where
  length = fromIntegral . DVS.length
  {-# INLINE length #-}
