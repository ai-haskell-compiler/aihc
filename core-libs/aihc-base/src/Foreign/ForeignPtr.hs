module Foreign.ForeignPtr
  ( ForeignPtr,
    FinalizerPtr,
    FinalizerEnvPtr,
    newForeignPtr,
    newForeignPtr_,
    newForeignPtrEnv,
    addForeignPtrFinalizer,
    addForeignPtrFinalizerEnv,
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
    addForeignPtrFinalizerEnv,
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

-- | Make a foreign pointer with a finalizer that also gets the environment
-- pointer.
newForeignPtrEnv :: FinalizerEnvPtr env a -> Ptr env -> Ptr a -> IO (ForeignPtr a)
newForeignPtrEnv finalizer environment pointer = do
  foreignPointer <- newForeignPtr_ pointer
  addForeignPtrFinalizerEnv finalizer environment foreignPointer
  return foreignPointer

mallocForeignPtrArray :: (Storable a) => Int -> IO (ForeignPtr a)
mallocForeignPtrArray = mallocForeignPtrArrayOf undefined

mallocForeignPtrArrayOf :: (Storable a) => a -> Int -> IO (ForeignPtr a)
mallocForeignPtrArrayOf placeholder count = mallocForeignPtrBytes (count * sizeOf placeholder)

mallocForeignPtrArray0 :: (Storable a) => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 count = mallocForeignPtrArray (count + 1)
