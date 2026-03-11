{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Vector.AsVector64
  ( AsVector64(..)
  ) where

import Data.Word

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

class AsVector64 a where
  asVector64 :: a -> DVS.Vector Word64

instance AsVector64 (DVS.Vector Word64) where
  asVector64 = id
  {-# INLINE asVector64 #-}

instance AsVector64 BS.ByteString where
  asVector64 bs = if vLen * 8 == BS.length bs
    then case BSI.toForeignPtr bs of
      (fptr, start, offset) -> DVS.unsafeCast (DVS.unsafeFromForeignPtr fptr start offset)
    else case BSI.toForeignPtr (bs <> BS.replicate (vLen * 8 - BS.length bs) 0) of
      (fptr, start, offset) -> DVS.unsafeCast (DVS.unsafeFromForeignPtr fptr start offset)
    where vLen = (BS.length bs + 7) `div` 8
  {-# INLINE asVector64 #-}
