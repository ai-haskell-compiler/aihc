{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy #-}
-- | Linux open file descriptor locking.
-- 
-- <https://www.gnu.org/software/libc/manual/html_node/Open-File-Description-Locks.html>
--
-- We prefer this over BSD locking (e.g. flock) since the latter appears to
-- break in some NFS configurations. Note that we intentionally do not try to
-- use ordinary POSIX file locking due to its peculiar semantics under
-- multi-threaded environments.
--
module Lukko.OFD (
    -- * Types
    FileLockingNotSupported(..),
    fileLockingSupported,
    FileLockingSupported,
    FileLockingMethod (..),
    fileLockingMethod,
    LockMode(..),
    -- * File descriptors
    FD,
    fdOpen,
    fdClose,
    fdLock,
    fdTryLock,
    fdUnlock,
    -- * Handles
    hLock,
    hTryLock,
    hUnlock,
    ) where

#define _GNU_SOURCE
#include <unistd.h>

-- Support for file description locks (F_OFD_SETLKW and F_OFD_SETLK) was added
-- to glibc in version 2.20. If we have an older version then we must get the
-- functionality directly from the linux headers.
--
-- See glibc 2.20 release notes: <https://sourceware.org/legacy-ml/libc-alpha/2014-09/msg00088.html>
#if defined(__GLIBC__) && defined(__GLIBC_PREREQ)
#if __GLIBC_PREREQ(2, 20)
#include <fcntl.h>
#else
#include <linux/fcntl.h>
#endif
#else
#include <fcntl.h>
#endif

import Control.Monad (void)
import System.IO (Handle)

import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.IO.Exception
import GHC.Ptr
import System.Posix.Types (COff, CPid)

import Lukko.Internal.FD
import Lukko.Internal.FillBytes
import Lukko.Internal.Types

-------------------------------------------------------------------------------
-- Support constants
-------------------------------------------------------------------------------

-- | A constants specifying whether file locking is supported.
fileLockingSupported :: Bool
fileLockingSupported = True

-- | A type level 'fileLockingSupported'.
type FileLockingSupported = True

-- | A constant specifying this method
fileLockingMethod :: FileLockingMethod
fileLockingMethod = MethodOFD

-------------------------------------------------------------------------------
-- FD
-------------------------------------------------------------------------------

-- | Lock using OFD locks.
fdLock :: FD -> LockMode -> IO ()
fdLock fd mode = void (lockImpl Nothing fd "fdLock" mode True)

-- | Try to lock using OFD locks.
fdTryLock :: FD -> LockMode -> IO Bool
fdTryLock fd mode = lockImpl Nothing fd "fdTryLock" mode False

-- | Unlock using OFD locks.
fdUnlock :: FD -> IO ()
fdUnlock = unlockImpl

-------------------------------------------------------------------------------
-- Handle
-------------------------------------------------------------------------------

-- | Lock using OFD locks.
hLock :: Handle -> LockMode -> IO ()
hLock h mode = do
    fd <- handleToFd h
    void (lockImpl (Just h) fd "hLock" mode True)

-- | Try to lock using OFD locks.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = do
    fd <- handleToFd h
    lockImpl (Just h) fd "hTryLock" mode False

-- | Unlock using OFD locks.
hUnlock :: Handle -> IO ()
hUnlock h = do
    fd <- handleToFd h
    unlockImpl fd

-------------------------------------------------------------------------------
-- Compat
-------------------------------------------------------------------------------

-- there is no alignment in old hsc2hs
#if defined(MIN_TOOL_VERSION_hsc2hs)
#if !MIN_TOOL_VERSION_hsc2hs(0,68,7)
#define HAS_NO_ALIGNMENT 1
#endif
#else
-- if run directly (cabal sets MIN_TOOL_VERSION_hsc2hs macro)
-- assume the worst.
#define HAS_NO_ALIGNMENT 1
#endif

#ifdef HAS_NO_ALIGNMENT
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

foreign import capi interruptible "fcntl.h fcntl"
  c_fcntl :: CInt -> CInt -> Ptr FLock -> IO CInt

data FLock  = FLock { l_type   :: CShort
                    , l_whence :: CShort
                    , l_start  :: COff
                    , l_len    :: COff
                    , l_pid    :: CPid
                    }

instance Storable FLock where
    sizeOf _ = #{size struct flock}
    alignment _ = #{alignment struct flock}
    poke ptr x = do
        fillBytes ptr 0 (sizeOf x)
        #{poke struct flock, l_type}   ptr (l_type x)
        #{poke struct flock, l_whence} ptr (l_whence x)
        #{poke struct flock, l_start}  ptr (l_start x)
        #{poke struct flock, l_len}    ptr (l_len x)
        #{poke struct flock, l_pid}    ptr (l_pid x)
    peek ptr = do
        x1 <- #{peek struct flock, l_type}   ptr
        x2 <- #{peek struct flock, l_whence} ptr
        x3 <- #{peek struct flock, l_start}  ptr
        x4 <- #{peek struct flock, l_len}    ptr
        x5 <- #{peek struct flock, l_pid}    ptr
        return (FLock x1 x2 x3 x4 x5)

lockImpl :: Maybe Handle -> FD -> String -> LockMode -> Bool -> IO Bool
lockImpl mh (FD fd) ctx mode block = do
  with flock $ \flock_ptr -> fix $ \retry -> do
      ret <- c_fcntl fd mode' flock_ptr
      case ret of
        0 -> return True
        _ -> getErrno >>= \errno -> case () of
          _ | not block && errno == eWOULDBLOCK -> return False
            | errno == eINTR -> retry
            | otherwise -> ioException $ errnoToIOError ctx errno mh Nothing
  where
    flock = FLock { l_type = case mode of
                               SharedLock -> #{const F_RDLCK}
                               ExclusiveLock -> #{const F_WRLCK}
                  , l_whence = #{const SEEK_SET}
                  , l_start = 0
                  , l_len = 0
                  , l_pid = 0
                  }
    mode'
      | block     = #{const F_OFD_SETLKW}
      | otherwise = #{const F_OFD_SETLK}

unlockImpl :: FD -> IO ()
unlockImpl (FD fd) = do
  let flock = FLock { l_type = #{const F_UNLCK}
                    , l_whence = #{const SEEK_SET}
                    , l_start = 0
                    , l_len = 0
                    , l_pid = 0
                    }
  throwErrnoIfMinus1_ "hUnlock"
      $ with flock $ c_fcntl fd #{const F_OFD_SETLK}
