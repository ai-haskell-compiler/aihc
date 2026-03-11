-- | Provides shared memory for IPC (Inter Process Communication).
module SharedMemory
  ( openSharedMemory
  ) where

import           Foreign.C.Types (CSize)
import qualified Foreign.Concurrent as FC
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.Posix.Files
import           System.Posix.SharedMem
import           System.Posix.Types

import           MMAP


-- | @openSharedMemory shmemPath size openFlags openFileMode@:
-- Creates a shared memory file using @shm_open@ at @shmemPath@ of @size@ bytes,
-- returning the created `Fd` and `ForeignPtr` pointing to the @mmap@'ed memory.
--
-- Note that there are portability considerations, that put constraints on what
-- you can pass to this function. For example, @shmemPath@ is recommended to
-- start with @/@, and BSD as of writing requires that. Consult @man shm_open@.
--
-- The `Fd` can be used to resize the shared memory region.
--
-- When the returned `ForeignPtr` is garbage collected, the memory is @munmap@'ed,
-- but the `Fd` remains open until it is closed or garbage collected.
--
-- Closing the `Fd` will not invalidate the returned `ForeignPtr`.
openSharedMemory :: String -> CSize -> ShmOpenFlags -> FileMode -> IO (ForeignPtr (), Fd)
openSharedMemory shmemPath size openFlags openFileMode = do
  fd <- shmOpen shmemPath openFlags openFileMode
  setFdSize fd (fromIntegral size)
  ptr <- mmap nullPtr
              size
              (protRead <> protWrite)
              (mkMmapFlags mapShared mempty)
              fd
              0
  fptr <- FC.newForeignPtr ptr (munmap ptr size)
  return (fptr, fd)
