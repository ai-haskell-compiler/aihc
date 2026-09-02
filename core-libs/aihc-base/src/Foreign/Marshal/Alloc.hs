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
import GHC.ForeignPtr (FinalizerPtr)
import GHC.Ptr (Ptr)
import Prelude (IO, Int, error, undefined)

alloca :: (Storable a) => (Ptr a -> IO b) -> IO b
alloca = allocaOf undefined

allocaOf :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
allocaOf placeholder = allocaBytes (sizeOf placeholder)

-- | Temporary memory needs the C allocator, which is not available.
allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes _ _ = error "Foreign.Marshal.Alloc.allocaBytes: temporary memory is not available"

allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b
allocaBytesAligned size _ = allocaBytes size

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
