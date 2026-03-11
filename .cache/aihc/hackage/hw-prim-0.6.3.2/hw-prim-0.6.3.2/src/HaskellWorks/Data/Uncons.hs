{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Uncons
    ( Container(..)
    , Uncons(..)
    ) where

import Data.Int
import Data.Maybe
import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Drop
import Prelude                     hiding (drop)

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Drop v => Uncons v where
  uncons :: v -> Maybe (Elem v, v)

instance Uncons String where
  uncons s = case s of
    (x:xs) -> Just (x, xs)
    _      -> Nothing
  {-# INLINE uncons   #-}

instance Uncons BS.ByteString where
  uncons = BS.uncons
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Word8) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Word16) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Word32) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Word64) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Word8) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Word16) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Word32) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Word64) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Int8) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Int16) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Int32) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DV.Vector Int64) where
  uncons s = if DV.length s == 0 then Nothing else Just (DV.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Int8) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Int16) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Int32) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Int64) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}

instance Uncons (DVS.Vector Int) where
  uncons s = if DVS.length s == 0 then Nothing else Just (DVS.head s, drop 1 s)
  {-# INLINE uncons   #-}
