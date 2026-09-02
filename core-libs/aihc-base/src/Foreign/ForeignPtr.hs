module Foreign.ForeignPtr
  ( ForeignPtr,
    FinalizerPtr,
    FinalizerEnvPtr,
    newForeignPtr,
    newForeignPtr_,
    addForeignPtrFinalizer,
    withForeignPtr,
    finalizeForeignPtr,
    touchForeignPtr,
    castForeignPtr,
    plusForeignPtr,
    mallocForeignPtr,
    mallocForeignPtrBytes,
    mallocForeignPtrArray,
    mallocForeignPtrArray0,
  )
where

import Foreign.Storable (Storable (..))
import GHC.ForeignPtr
  ( FinalizerEnvPtr,
    FinalizerPtr,
    ForeignPtr,
    addForeignPtrFinalizer,
    castForeignPtr,
    finalizeForeignPtr,
    mallocForeignPtr,
    mallocForeignPtrBytes,
    newForeignPtr_,
    plusForeignPtr,
    touchForeignPtr,
    withForeignPtr,
  )
import GHC.Ptr (Ptr)
import Prelude (IO, Int, Num (..), return, undefined)

newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
newForeignPtr finalizer pointer = do
  foreignPointer <- newForeignPtr_ pointer
  addForeignPtrFinalizer finalizer foreignPointer
  return foreignPointer

mallocForeignPtrArray :: (Storable a) => Int -> IO (ForeignPtr a)
mallocForeignPtrArray = mallocForeignPtrArrayOf undefined

mallocForeignPtrArrayOf :: (Storable a) => a -> Int -> IO (ForeignPtr a)
mallocForeignPtrArrayOf placeholder count = mallocForeignPtrBytes (count * sizeOf placeholder)

mallocForeignPtrArray0 :: (Storable a) => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 count = mallocForeignPtrArray (count + 1)
