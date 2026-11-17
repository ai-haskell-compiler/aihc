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

import Foreign.C.Types (CSize (..))
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import GHC.ForeignPtr (FinalizerPtr)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Internal.Base (Monad (..))
import GHC.Prim (MutableByteArray#, RealWorld, mutableByteArrayContents#, newAlignedPinnedByteArray#, newPinnedByteArray#, touch#)
import GHC.Ptr (Ptr (..))
import Prelude (Eq (..), Ord (..), String, error, fromIntegral, otherwise, undefined, (&&), (++))

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
mallocBytes size = c_malloc (fromIntegral size) >>= checkAllocation "mallocBytes" size

calloc :: (Storable a) => IO (Ptr a)
calloc = callocOf undefined

callocOf :: (Storable a) => a -> IO (Ptr a)
callocOf placeholder = callocBytes (sizeOf placeholder)

callocBytes :: Int -> IO (Ptr a)
callocBytes size = c_calloc 1 (fromIntegral size) >>= checkAllocation "callocBytes" size

realloc :: (Storable b) => Ptr a -> IO (Ptr b)
realloc = reallocOf undefined

reallocOf :: (Storable b) => b -> Ptr a -> IO (Ptr b)
reallocOf placeholder pointer = reallocBytes pointer (sizeOf placeholder)

reallocBytes :: Ptr a -> Int -> IO (Ptr b)
reallocBytes pointer size = c_realloc pointer (fromIntegral size) >>= checkAllocation "reallocBytes" size

free :: Ptr a -> IO ()
free = c_free

-- | Stop with an error when the C allocator returns a null pointer.
-- A request for zero bytes can return a null pointer, which is not a failure.
checkAllocation :: String -> Int -> Ptr a -> IO (Ptr a)
checkAllocation name size pointer
  | pointer == nullPtr && size > 0 = error ("Foreign.Marshal.Alloc." ++ name ++ ": out of memory")
  | otherwise = return pointer

foreign import ccall unsafe "stdlib.h malloc" c_malloc :: CSize -> IO (Ptr a)

foreign import ccall unsafe "stdlib.h calloc" c_calloc :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "stdlib.h realloc" c_realloc :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "stdlib.h free" c_free :: Ptr a -> IO ()

foreign import ccall unsafe "stdlib.h &free" finalizerFree :: FinalizerPtr a
