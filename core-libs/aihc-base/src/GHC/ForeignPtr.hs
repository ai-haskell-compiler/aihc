{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.ForeignPtr
  ( ForeignPtr (..),
    ForeignPtrContents (..),
    Finalizers (..),
    FinalizerPtr,
    FinalizerEnvPtr,
    newForeignPtr_,
    newConcForeignPtr,
    mallocForeignPtr,
    mallocPlainForeignPtr,
    mallocForeignPtrBytes,
    mallocPlainForeignPtrBytes,
    mallocForeignPtrAlignedBytes,
    mallocPlainForeignPtrAlignedBytes,
    addForeignPtrFinalizer,
    addForeignPtrConcFinalizer,
    finalizeForeignPtr,
    touchForeignPtr,
    unsafeForeignPtrToPtr,
    withForeignPtr,
    unsafeWithForeignPtr,
    castForeignPtr,
    plusForeignPtr,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Ptr (FunPtr, Ptr (..))
import Foreign.Storable (Storable (..))
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newAlignedPinnedByteArray#, newPinnedByteArray#, plusAddr#, touch#)
import Prelude (Eq (..), Maybe (..), Num (..), Ord (..), error, return, sequence_, undefined, (>>=))

data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents

data Finalizers
  = NoFinalizers
  | HaskellFinalizers [IO ()]

data ForeignPtrContents
  = PlainForeignPtr (IORef Finalizers)
  | FinalPtr
  | MallocPtr (MutableByteArray# RealWorld) (IORef Finalizers)
  | PlainPtr (MutableByteArray# RealWorld)

type FinalizerPtr a = FunPtr (Ptr a -> IO ())

type FinalizerEnvPtr env a = FunPtr (Ptr env -> Ptr a -> IO ())

-- | Foreign pointers compare by the address they wrap, ignoring the
-- finalizer bookkeeping that rides along with it.
instance Eq (ForeignPtr a) where
  left == right = unsafeForeignPtrToPtr left == unsafeForeignPtrToPtr right

instance Ord (ForeignPtr a) where
  compare left right = compare (unsafeForeignPtrToPtr left) (unsafeForeignPtrToPtr right)

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (ForeignPtr address _) = Ptr address

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (ForeignPtr address contents) = ForeignPtr address contents

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr address contents) (I# offset) = ForeignPtr (plusAddr# address offset) contents

-- | Keep the backing allocation alive until this point.
touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr (ForeignPtr _ contents) =
  IO
    ( \state ->
        case touch# contents state of
          nextState -> (# nextState, () #)
    )

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr pointer action = do
  result <- action (unsafeForeignPtrToPtr pointer)
  touchForeignPtr pointer
  return result

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ (Ptr address) = do
  finalizers <- newIORef NoFinalizers
  return (ForeignPtr address (PlainForeignPtr finalizers))

newConcForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newConcForeignPtr pointer finalizer = do
  foreignPointer <- newForeignPtr_ pointer
  addForeignPtrConcFinalizer foreignPointer finalizer
  return foreignPointer

mallocForeignPtr :: (Storable a) => IO (ForeignPtr a)
mallocForeignPtr = mallocForeignPtrOf undefined

mallocForeignPtrOf :: (Storable a) => a -> IO (ForeignPtr a)
mallocForeignPtrOf placeholder = mallocForeignPtrBytes (sizeOf placeholder)

mallocPlainForeignPtr :: (Storable a) => IO (ForeignPtr a)
mallocPlainForeignPtr = mallocPlainForeignPtrOf undefined

mallocPlainForeignPtrOf :: (Storable a) => a -> IO (ForeignPtr a)
mallocPlainForeignPtrOf placeholder = mallocPlainForeignPtrBytes (sizeOf placeholder)

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes (I# size) = do
  finalizers <- newIORef NoFinalizers
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, ForeignPtr (mutableByteArrayContents# buffer) (MallocPtr buffer finalizers) #)
    )

mallocPlainForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytes (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, ForeignPtr (mutableByteArrayContents# buffer) (PlainPtr buffer) #)
    )

mallocForeignPtrAlignedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocForeignPtrAlignedBytes (I# size) (I# align) = do
  finalizers <- newIORef NoFinalizers
  IO
    ( \state ->
        case newAlignedPinnedByteArray# size align state of
          (# allocatedState, buffer #) ->
            (# allocatedState, ForeignPtr (mutableByteArrayContents# buffer) (MallocPtr buffer finalizers) #)
    )

mallocPlainForeignPtrAlignedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocPlainForeignPtrAlignedBytes (I# size) (I# align) =
  IO
    ( \state ->
        case newAlignedPinnedByteArray# size align state of
          (# allocatedState, buffer #) ->
            (# allocatedState, ForeignPtr (mutableByteArrayContents# buffer) (PlainPtr buffer) #)
    )

-- | Foreign finalizer functions cannot be called by this runtime.
addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
addForeignPtrFinalizer _ _ = error "GHC.ForeignPtr.addForeignPtrFinalizer: foreign finalizers are not available"

addForeignPtrConcFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrConcFinalizer (ForeignPtr _ contents) finalizer =
  case finalizerReference contents of
    Nothing -> error "GHC.ForeignPtr.addForeignPtrConcFinalizer: plain pointers cannot have finalizers"
    Just reference ->
      readIORef reference >>= \finalizers ->
        writeIORef reference (HaskellFinalizers (finalizer : finalizerList finalizers))

finalizerReference :: ForeignPtrContents -> Maybe (IORef Finalizers)
finalizerReference (PlainForeignPtr reference) = Just reference
finalizerReference (MallocPtr _ reference) = Just reference
finalizerReference FinalPtr = Nothing
finalizerReference (PlainPtr _) = Nothing

finalizerList :: Finalizers -> [IO ()]
finalizerList NoFinalizers = []
finalizerList (HaskellFinalizers finalizers) = finalizers

-- | Run the registered finalizers now and remove them.
finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr (ForeignPtr _ contents) =
  case finalizerReference contents of
    Nothing -> return ()
    Just reference -> do
      finalizers <- readIORef reference
      writeIORef reference NoFinalizers
      sequence_ (finalizerList finalizers)
