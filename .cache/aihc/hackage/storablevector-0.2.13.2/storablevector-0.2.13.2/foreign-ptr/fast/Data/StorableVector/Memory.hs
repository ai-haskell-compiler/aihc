module Data.StorableVector.Memory where

import Foreign.Storable (Storable)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)

{-# INLINE mallocForeignPtrArray #-}
mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray = mallocPlainForeignPtrArray
