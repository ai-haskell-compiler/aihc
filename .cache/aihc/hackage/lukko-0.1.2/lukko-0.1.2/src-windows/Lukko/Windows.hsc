{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy #-}

-- | File locking for Windows.
module Lukko.Windows (
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

#include <windows.h>

import Control.Monad (void)
import System.IO     (Handle)

import Data.Bits
import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr (Ptr)
import GHC.Windows

import Lukko.Internal.FD
import Lukko.Internal.FillBytes
import Lukko.Internal.Types

#if defined(i386_HOST_ARCH)
##define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
##define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

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
fileLockingMethod = MethodWindows

-------------------------------------------------------------------------------
-- FD
-------------------------------------------------------------------------------

-- | Lock using Win32 locks.
fdLock :: FD -> LockMode -> IO ()
fdLock fd mode = void (lockImpl Nothing fd "fdLock" mode True)

-- | Try to lock using Win32 locks.
fdTryLock :: FD -> LockMode -> IO Bool
fdTryLock fd mode = lockImpl Nothing fd "fdTryLock" mode False

-- | Unlock using Win32 locks.
fdUnlock :: FD -> IO ()
fdUnlock = unlockImpl

-------------------------------------------------------------------------------
-- Handle
-------------------------------------------------------------------------------

-- | Lock using Win32 locks.
hLock :: Handle -> LockMode -> IO ()
hLock h mode = do
    fd <- handleToFd h
    void (lockImpl (Just h) fd "hLock" mode True)

-- | Try to lock using Win32 locks.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = do
    fd <- handleToFd h
    lockImpl (Just h) fd "hTryLock" mode False

-- | Unlock using Win32 locks.
hUnlock :: Handle -> IO ()
hUnlock h = do
    fd <- handleToFd h
    unlockImpl fd

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

lockImpl :: Maybe Handle -> FD -> String -> LockMode -> Bool -> IO Bool
lockImpl _ (FD wh) ctx mode block = do
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    let flags = cmode .|. (if block then 0 else #{const LOCKFILE_FAIL_IMMEDIATELY})
    -- We want to lock the whole file without looking up its size to be
    -- consistent with what flock does. According to documentation of LockFileEx
    -- "locking a region that goes beyond the current end-of-file position is
    -- not an error", hence we pass maximum value as the number of bytes to
    -- lock.
    fix $ \retry -> c_LockFileEx wh flags 0 #{const INFINITE} #{const INFINITE} ovrlpd >>= \res -> case res of
      True  -> return True
      False -> getLastError >>= \err -> case () of
        _ | not block && err == #{const ERROR_LOCK_VIOLATION} -> return False
          | err == #{const ERROR_OPERATION_ABORTED} -> retry
          | otherwise -> failWith ctx err
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

    cmode = case mode of
      SharedLock    -> 0
      ExclusiveLock -> #{const LOCKFILE_EXCLUSIVE_LOCK}

unlockImpl :: FD -> IO ()
unlockImpl (FD wh) = do
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    c_UnlockFileEx wh 0 #{const INFINITE} #{const INFINITE} ovrlpd >>= \res -> case res of
      True  -> return ()
      False -> getLastError >>= failWith "fdUnlock"
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

-- https://docs.microsoft.com/en-gb/windows/win32/api/fileapi/nf-fileapi-lockfileex
foreign import WINDOWS_CCONV interruptible "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL

-- https://docs.microsoft.com/en-gb/windows/win32/api/fileapi/nf-fileapi-unlockfileex
foreign import WINDOWS_CCONV interruptible "UnlockFileEx"
  c_UnlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> Ptr () -> IO BOOL
