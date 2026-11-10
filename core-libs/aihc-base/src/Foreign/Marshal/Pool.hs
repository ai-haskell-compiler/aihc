-- | Memory pools. A pool keeps each allocation that it makes, and
-- 'freePool' releases all of them together.
module Foreign.Marshal.Pool
  ( Pool,
    newPool,
    freePool,
    withPool,
    pooledMalloc,
    pooledMallocBytes,
    pooledRealloc,
    pooledReallocBytes,
    pooledMallocArray,
    pooledMallocArray0,
    pooledReallocArray,
    pooledReallocArray0,
    pooledNew,
    pooledNewArray,
    pooledNewArray0,
  )
where

import Control.Exception.Base (mask, onException)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (delete)
import Foreign.Marshal.Alloc (free, mallocBytes, reallocBytes)
import Foreign.Marshal.Array (pokeArray, pokeArray0)
import Foreign.Marshal.Error (throwIf)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import Prelude (IO, Int, Num (..), const, length, mapM_, notElem, return, undefined, ($), (.), (<$>), (>>=))

-- | A pool of memory. The pool keeps the pointer of each allocation.
newtype Pool = Pool (IORef [Ptr ()])

-- | Make an empty pool.
newPool :: IO Pool
newPool = Pool <$> newIORef []

-- | Release all memory that the pool holds.
freePool :: Pool -> IO ()
freePool (Pool pool) = readIORef pool >>= mapM_ free

-- | Run an action with a new pool, and release the pool after the action.
-- The pool is also released when the action throws an exception.
withPool :: (Pool -> IO b) -> IO b
withPool action =
  mask $ \restore -> do
    pool <- newPool
    value <- restore (action pool) `onException` freePool pool
    freePool pool
    return value

-- | Allocate space for one value in the pool.
pooledMalloc :: (Storable a) => Pool -> IO (Ptr a)
pooledMalloc pool = pooledMallocArray pool 1

-- | Allocate a number of bytes in the pool.
pooledMallocBytes :: Pool -> Int -> IO (Ptr a)
pooledMallocBytes (Pool pool) size = do
  pointer <- mallocBytes size
  modifyIORef pool (pointer :)
  return (castPtr pointer)

-- | Change the size of an allocation of the pool to one value.
pooledRealloc :: (Storable a) => Pool -> Ptr a -> IO (Ptr a)
pooledRealloc pool pointer = pooledReallocArray pool pointer 1

-- | Change the size of an allocation of the pool. The pointer must come
-- from this pool.
pooledReallocBytes :: Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocBytes (Pool pool) pointer size = do
  let untyped = castPtr pointer
  _ <- throwIf (notElem untyped) (const "pointer not in pool") (readIORef pool)
  newPointer <- reallocBytes untyped size
  modifyIORef pool ((newPointer :) . delete untyped)
  return (castPtr newPointer)

-- | Allocate space for a number of values in the pool.
pooledMallocArray :: (Storable a) => Pool -> Int -> IO (Ptr a)
pooledMallocArray pool count = allocation
  where
    allocation = pooledMallocBytes pool (count * elementSize (allocatedPointer allocation))

-- | Allocate space for a number of values and one end marker in the pool.
pooledMallocArray0 :: (Storable a) => Pool -> Int -> IO (Ptr a)
pooledMallocArray0 pool count = pooledMallocArray pool (count + 1)

-- | Change the size of an allocation of the pool to a number of values.
pooledReallocArray :: (Storable a) => Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocArray pool pointer count = pooledReallocBytes pool pointer (count * elementSize pointer)

-- | Change the size of an allocation of the pool to a number of values
-- and one end marker.
pooledReallocArray0 :: (Storable a) => Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocArray0 pool pointer count = pooledReallocArray pool pointer (count + 1)

-- | Allocate a value in the pool and write it.
pooledNew :: (Storable a) => Pool -> a -> IO (Ptr a)
pooledNew pool value = do
  pointer <- pooledMalloc pool
  poke pointer value
  return pointer

-- | Allocate a list of values in the pool and write them.
pooledNewArray :: (Storable a) => Pool -> [a] -> IO (Ptr a)
pooledNewArray pool values = do
  pointer <- pooledMallocArray pool (length values)
  pokeArray pointer values
  return pointer

-- | Allocate a list of values and an end marker in the pool and write them.
pooledNewArray0 :: (Storable a) => Pool -> a -> [a] -> IO (Ptr a)
pooledNewArray0 pool marker values = do
  pointer <- pooledMallocArray0 pool (length values)
  pokeArray0 marker pointer values
  return pointer

-- | The pointer type of an allocation. Only its type is used.
allocatedPointer :: IO (Ptr a) -> Ptr a
allocatedPointer _ = undefined

-- | The size of the value type of a pointer.
elementSize :: (Storable a) => Ptr a -> Int
elementSize pointer = sizeOf (pointee pointer)
  where
    pointee :: Ptr b -> b
    pointee _ = undefined
