{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy #-}
-- | File locking via BSD-style @flock(2)@.
module Lukko.FLock (
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

#include <sys/file.h>

import Control.Monad (void)
import System.IO (Handle)

import Data.Bits
import Data.Function
import Foreign.C.Error
import Foreign.C.Types
import GHC.Base
import GHC.IO.Exception

import Lukko.Internal.FD
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
fileLockingMethod = MethodFLock

-------------------------------------------------------------------------------
-- FD
-------------------------------------------------------------------------------

-- | Lock using BSD-style locks.
fdLock :: FD -> LockMode -> IO ()
fdLock fd mode = void (lockImpl Nothing fd "fdLock" mode True)

-- | Try to lock using BSD-style locks.
fdTryLock :: FD -> LockMode -> IO Bool
fdTryLock fd mode = lockImpl Nothing fd "fdTryLock" mode False

-- | Unlock using BSD-style locks.
fdUnlock :: FD -> IO ()
fdUnlock = unlockImpl

-------------------------------------------------------------------------------
-- Handle
-------------------------------------------------------------------------------

-- | Lock using BSD-style locks.
hLock :: Handle -> LockMode -> IO ()
hLock h mode = do
    fd <- handleToFd h
    void (lockImpl (Just h) fd "hLock" mode True)

-- | Try to lock using BSD-style locks.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock h mode = do
    fd <- handleToFd h
    lockImpl (Just h) fd "hTryLock" mode False

-- | Unlock using BSD-style locks.
hUnlock :: Handle -> IO ()
hUnlock h = do
    fd <- handleToFd h
    unlockImpl fd

-------------------------------------------------------------------------------
-- Compat stuff
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

lockImpl :: Maybe Handle -> FD -> String -> LockMode -> Bool -> IO Bool
lockImpl mh (FD fd)  ctx mode block = do
  let flags = cmode .|. (if block then 0 else #{const LOCK_NB})
  fix $ \retry -> c_flock fd flags >>= \res -> case res of
    0 -> return True
    _ -> getErrno >>= \errno -> case () of
      _ | not block
        , errno == eAGAIN || errno == eACCES -> return False
        | errno == eINTR -> retry
        | otherwise -> ioException $ errnoToIOError ctx errno mh Nothing
  where
    cmode = case mode of
      SharedLock    -> #{const LOCK_SH}
      ExclusiveLock -> #{const LOCK_EX}

unlockImpl :: FD -> IO ()
unlockImpl (FD fd) = do
  throwErrnoIfMinus1_ "flock" $ c_flock fd #{const LOCK_UN}

foreign import ccall interruptible "flock"
  c_flock :: CInt -> CInt -> IO CInt
