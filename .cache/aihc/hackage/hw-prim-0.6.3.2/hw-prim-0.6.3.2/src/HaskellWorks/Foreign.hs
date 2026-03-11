module HaskellWorks.Foreign
  ( mallocForeignPtrBytesWithAlignedPtr
  , mallocForeignPtrBytesWithAlignedCastPtr
  ) where

import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr        (Ptr, alignPtr)
import Foreign.Storable   (Storable)

import qualified Foreign.ForeignPtr        as F
import qualified Foreign.ForeignPtr.Unsafe as F
import qualified Foreign.Ptr               as F

mallocForeignPtrBytesWithAlignedPtr :: Storable a => Int -> Int -> IO (ForeignPtr a, Ptr a)
mallocForeignPtrBytesWithAlignedPtr alignment n = do
  fptr <- F.mallocForeignPtrBytes (n + alignment)
  let alignedPtr = alignPtr (F.unsafeForeignPtrToPtr fptr) alignment
  return (fptr, alignedPtr)

mallocForeignPtrBytesWithAlignedCastPtr :: Storable a => Int -> Int -> IO (ForeignPtr a, Ptr b)
mallocForeignPtrBytesWithAlignedCastPtr alignment n = do
  fptr <- F.mallocForeignPtrBytes (n + alignment)
  let alignedPtr = alignPtr (F.unsafeForeignPtrToPtr fptr) alignment
  return (fptr, F.castPtr alignedPtr)
