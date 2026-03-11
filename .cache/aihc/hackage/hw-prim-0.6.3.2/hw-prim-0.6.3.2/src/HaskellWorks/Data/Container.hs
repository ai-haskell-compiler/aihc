{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Container
    ( Container(..)
    ) where

import Data.Int
import Data.Word

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Container a where
  type Elem a

instance Container [a] where
  type Elem [a] = a

instance Container BS.ByteString where
  type Elem BS.ByteString = Word8

instance Container (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8

instance Container (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16

instance Container (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32

instance Container (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64

instance Container (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8

instance Container (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16

instance Container (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32

instance Container (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64

instance Container (DV.Vector Int8) where
  type Elem (DV.Vector Int8) = Int8

instance Container (DV.Vector Int16) where
  type Elem (DV.Vector Int16) = Int16

instance Container (DV.Vector Int32) where
  type Elem (DV.Vector Int32) = Int32

instance Container (DV.Vector Int64) where
  type Elem (DV.Vector Int64) = Int64

instance Container (DVS.Vector Int8) where
  type Elem (DVS.Vector Int8) = Int8

instance Container (DVS.Vector Int16) where
  type Elem (DVS.Vector Int16) = Int16

instance Container (DVS.Vector Int32) where
  type Elem (DVS.Vector Int32) = Int32

instance Container (DVS.Vector Int64) where
  type Elem (DVS.Vector Int64) = Int64

instance Container (DVS.Vector Int) where
  type Elem (DVS.Vector Int) = Int
