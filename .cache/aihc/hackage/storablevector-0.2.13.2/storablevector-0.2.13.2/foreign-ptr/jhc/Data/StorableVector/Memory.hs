module Data.StorableVector.Memory where

import Foreign.Storable (Storable, sizeOf, )
import qualified Foreign.ForeignPtr as F


{-# INLINE mallocForeignPtrArray #-}
mallocForeignPtrArray :: Storable a => Int -> IO (F.ForeignPtr a)
mallocForeignPtrArray =
   let withSize :: Storable a => a -> (Int -> IO (F.ForeignPtr a)) -> (Int -> IO (F.ForeignPtr a))
       withSize dummy f n = f (n*sizeOf dummy)
   in  withSize undefined F.mallocForeignPtrBytes
