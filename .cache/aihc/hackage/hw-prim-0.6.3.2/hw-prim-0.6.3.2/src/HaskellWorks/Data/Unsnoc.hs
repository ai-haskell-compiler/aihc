{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Unsnoc
    ( Container(..)
    , Unsnoc(..)
    ) where

import Data.Int
import Data.Maybe
import Data.Word
import HaskellWorks.Data.Container
import Prelude                     hiding (drop)

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Unsnoc v where
  unsnoc :: v -> Maybe (Elem v, v)

instance Unsnoc String where
  unsnoc s = case reverse s of
    (x:xs) -> Just (x, reverse xs)
    _      -> Nothing
  {-# INLINE unsnoc   #-}

instance Unsnoc BS.ByteString where
  unsnoc s = if BS.length s == 0 then Nothing else Just (BS.last s, BS.take (BS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Word8) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Word16) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Word32) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Word64) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Word8) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Word16) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Word32) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Word64) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Int8) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Int16) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Int32) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DV.Vector Int64) where
  unsnoc s = if DV.length s == 0 then Nothing else Just (DV.last s, DV.take (DV.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Int8) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Int16) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Int32) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Int64) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}

instance Unsnoc (DVS.Vector Int) where
  unsnoc s = if DVS.length s == 0 then Nothing else Just (DVS.last s, DVS.take (DVS.length s - 1) s)
  {-# INLINE unsnoc   #-}
