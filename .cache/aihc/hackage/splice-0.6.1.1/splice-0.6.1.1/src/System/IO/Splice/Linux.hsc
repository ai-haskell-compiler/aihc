{- | Exposes the GNU\/Linux system call @splice()@: <http://kerneltrap.org/node/6505>.

     /This module is only available (compiled & exposed) on GNU/\//Linux./
-}
--  
-- Module      : System.IO.Splice.Linux
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : stable
-- Portability : GNU\/Linux-only


#include <fcntl.h>
{-# LANGUAGE CPP, ForeignFunctionInterface #-}


module System.IO.Splice.Linux (

    c_splice
  , ChunkSize
  , sPLICE_F_MOVE
  , sPLICE_F_MORE
  , sPLICE_F_NONBLOCK

  ) where


import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import System.Posix.Types


-- | The numeric type used by 'c_splice' for chunk size recommendations when
--   moving data.
type ChunkSize = (#type size_t)

-- | Moves data between two file descriptors without copying between kernel
--   address space and user address space. It transfers up to @len@ bytes of
--   data from the file descriptor @fd_in@ to the file descriptor @fd_out@,
--   where one of the descriptors must refer to a pipe.
--
--   'c_splice' is /NOT/ a loop and needs to be called repeatedly.
--
--   For an example, see 'Network.Socket.Splice.Internal.splice'.
foreign import ccall "splice"
  c_splice
  :: Fd                  -- ^ @fd_in@.
  -> Ptr (#type loff_t)  -- ^ @off_in@.
  -> Fd                  -- ^ @fd_out@.
  -> Ptr (#type loff_t)  -- ^ @off_out@.
  -> ChunkSize           -- ^ @len@.
  -> Word                -- ^ @flags@.
  -> IO (#type ssize_t)  -- ^ number of bytes moved if successful; otherwise -1.


-- | Attempt to move pages instead of copying. This is only a hint to the
--   kernel: pages may stil be copied (/in kernel address space/) if the kernel
--   cannot move the pages from the pipe, or if the pipe buffers don't refer to
--   full pages.
sPLICE_F_MOVE :: Word
sPLICE_F_MOVE = (#const "SPLICE_F_MOVE")


-- | More data will be coming in a subsequent 'c_splice'. This is a helpful hint
--   when @fd_out@ refers to a socket.
sPLICE_F_MORE :: Word
sPLICE_F_MORE = (#const "SPLICE_F_MORE")


-- | Do not block on I\/O. This makes the 'c_splice' pipe operations
--   nonblocking, but 'c_splice' may nevertheless block because the file
--   descriptors that are 'c_splice'd to\/from may block (unless they have the
--   @O_NONBLOCK@ flag set).
sPLICE_F_NONBLOCK :: Word
sPLICE_F_NONBLOCK = (#const "SPLICE_F_NONBLOCK")