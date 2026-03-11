module Data.StorableVector.Memory where

import Foreign.Storable (Storable)
import qualified Foreign.ForeignPtr as F


{-# INLINE mallocForeignPtrArray #-}
mallocForeignPtrArray :: Storable a => Int -> IO (F.ForeignPtr a)
mallocForeignPtrArray = F.mallocForeignPtrArray
