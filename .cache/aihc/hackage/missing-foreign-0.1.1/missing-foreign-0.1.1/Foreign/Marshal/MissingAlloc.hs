{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------
-- |
-- Module       : Foreign.Marshal.MissingAlloc
-- Copyright    : (c) Jason Dagit 2011
-- License      : BSD-style (see the file LICENSE)
-- 
-- Maintainer   : dagitj@gmail.com
--
-- The module "Foreign.Marshal.MissingAlloc" provides access to
-- 'calloc' (e.g., allocated 0-initialized chunks of memory
-- outside of the Haskell storage manager).
--
-- If any of these allocation functions fails, an exception is
-- raised.
--
-- The storage allocated is alligned to store any basic
-- foreign types.
--
----------------------------------------------------------------
module Foreign.Marshal.MissingAlloc (
  -- * Memory allocation
  -- ** Initialized dynamic allocation
  calloc,     -- :: Storable a => IO (Ptr a)
  callocBytes -- :: Int -> IO (Ptr a)
) where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Error

foreign import ccall unsafe "stdlib.h calloc"
  _calloc :: CSize -> CSize -> IO (Ptr a)

-- | Allocate a block of memory that is sufficient to hold values of type
-- @a@.  The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
-- The memory is initalized to 0.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
{-# INLINE calloc #-}
calloc :: Storable a => IO (Ptr a)
calloc = doCalloc undefined
  where
  doCalloc       :: Storable b => b -> IO (Ptr b)
  doCalloc dummy = callocBytes (sizeOf dummy)

-- | Allocate a block of memory of the given number of bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign tyes that fit into a memory block of the allocated size.
-- The memory is initialized to 0.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
callocBytes :: Int -> IO (Ptr a)
callocBytes size = throwIfNull "calloc" (_calloc (fromIntegral size) 1)
