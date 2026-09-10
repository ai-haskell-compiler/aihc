{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Foreign.Marshal.Alloc
  ( alloca,
    allocaBytes,
    allocaBytesAligned,
    malloc,
    mallocBytes,
    calloc,
    callocBytes,
    realloc,
    reallocBytes,
    free,
    finalizerFree,
  )
where

import Foreign.Storable (Storable (..))
import GHC.Base (Monad (..))
import GHC.ForeignPtr (FinalizerPtr)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (MutableByteArray#, RealWorld, mutableByteArrayContents#, newAlignedPinnedByteArray#, newPinnedByteArray#, touch#)
import GHC.Ptr (Ptr (..))
import Prelude (error, undefined)

-- | A pinned scratch buffer. Boxing the raw array lets the scope of the
-- allocation be extended past the action with 'touch#'.
data AllocaBuffer = AllocaBuffer (MutableByteArray# RealWorld)

alloca :: (Storable a) => (Ptr a -> IO b) -> IO b
alloca = allocaOf undefined

allocaOf :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
allocaOf placeholder = allocaBytesAligned (sizeOf placeholder) (alignment placeholder)

-- | Run the action with a pointer to a freshly allocated, uninitialised
-- block of the given size. The block is only guaranteed to live for the
-- duration of the action.
allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes size action = do
  buffer <- newAllocaBuffer size
  withAllocaBuffer buffer action

allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAligned size align action = do
  buffer <- newAlignedAllocaBuffer size align
  withAllocaBuffer buffer action

newAllocaBuffer :: Int -> IO AllocaBuffer
newAllocaBuffer (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, AllocaBuffer buffer #)
    )

newAlignedAllocaBuffer :: Int -> Int -> IO AllocaBuffer
newAlignedAllocaBuffer (I# size) (I# align) =
  IO
    ( \state ->
        case newAlignedPinnedByteArray# size align state of
          (# allocatedState, buffer #) ->
            (# allocatedState, AllocaBuffer buffer #)
    )

withAllocaBuffer :: AllocaBuffer -> (Ptr a -> IO b) -> IO b
withAllocaBuffer buffer action =
  case buffer of
    AllocaBuffer raw -> do
      result <- action (Ptr (mutableByteArrayContents# raw))
      touchAllocaBuffer buffer
      return result

-- | Keep the backing allocation alive until this point.
touchAllocaBuffer :: AllocaBuffer -> IO ()
touchAllocaBuffer buffer =
  IO
    ( \state ->
        case touch# buffer state of
          nextState -> (# nextState, () #)
    )

malloc :: (Storable a) => IO (Ptr a)
malloc = mallocOf undefined

mallocOf :: (Storable a) => a -> IO (Ptr a)
mallocOf placeholder = mallocBytes (sizeOf placeholder)

mallocBytes :: Int -> IO (Ptr a)
mallocBytes _ = error "Foreign.Marshal.Alloc.mallocBytes: the C allocator is not available"

calloc :: (Storable a) => IO (Ptr a)
calloc = callocOf undefined

callocOf :: (Storable a) => a -> IO (Ptr a)
callocOf placeholder = callocBytes (sizeOf placeholder)

callocBytes :: Int -> IO (Ptr a)
callocBytes _ = error "Foreign.Marshal.Alloc.callocBytes: the C allocator is not available"

realloc :: (Storable b) => Ptr a -> IO (Ptr b)
realloc = reallocOf undefined

reallocOf :: (Storable b) => b -> Ptr a -> IO (Ptr b)
reallocOf placeholder pointer = reallocBytes pointer (sizeOf placeholder)

reallocBytes :: Ptr a -> Int -> IO (Ptr b)
reallocBytes _ _ = error "Foreign.Marshal.Alloc.reallocBytes: the C allocator is not available"

free :: Ptr a -> IO ()
free _ = error "Foreign.Marshal.Alloc.free: the C allocator is not available"

finalizerFree :: FinalizerPtr a
finalizerFree = error "Foreign.Marshal.Alloc.finalizerFree: the C allocator is not available"
