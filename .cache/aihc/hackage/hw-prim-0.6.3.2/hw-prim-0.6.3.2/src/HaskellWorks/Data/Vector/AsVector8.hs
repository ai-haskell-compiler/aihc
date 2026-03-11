{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Vector.AsVector8
  ( AsVector8(..)
  ) where

import Data.Word

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

class AsVector8 a where
  asVector8 :: a -> DVS.Vector Word8

instance AsVector8 (DVS.Vector Word8) where
  asVector8 = id
  {-# INLINE asVector8 #-}

instance AsVector8 (DVS.Vector Word16) where
  asVector8 = DVS.unsafeCast
  {-# INLINE asVector8 #-}

instance AsVector8 (DVS.Vector Word32) where
  asVector8 = DVS.unsafeCast
  {-# INLINE asVector8 #-}

instance AsVector8 (DVS.Vector Word64) where
  asVector8 = DVS.unsafeCast
  {-# INLINE asVector8 #-}

instance AsVector8 BS.ByteString where
  asVector8 bs = case BSI.toForeignPtr bs of
    (fptr, start, offset) -> DVS.unsafeFromForeignPtr fptr start offset
  {-# INLINE asVector8 #-}
