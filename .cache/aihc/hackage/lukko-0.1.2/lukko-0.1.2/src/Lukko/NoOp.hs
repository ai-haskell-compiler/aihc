{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}
-- | Non-operating locks.
--
-- All functions throw 'FileLockingNotImplemented'.
module Lukko.NoOp (
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

import Control.Exception (throwIO)
import System.IO         (Handle)

import Lukko.Internal.FD
import Lukko.Internal.Types

-- | A constants specifying whether file locking is supported.
fileLockingSupported :: Bool
fileLockingSupported = False

-- | A type level 'fileLockingSupported'.
type FileLockingSupported = 'False

-- | A constant specifying this method
fileLockingMethod :: FileLockingMethod
fileLockingMethod = MethodNoOp

-- | No-op implementation.
hLock :: Handle -> LockMode -> IO ()
hLock _ _ = throwIO FileLockingNotSupported

-- | No-op implementation
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock _ _ = throwIO FileLockingNotSupported

-- | No-op implementation.
hUnlock :: Handle -> IO ()
hUnlock _ = throwIO FileLockingNotSupported

-- | No-op implementation.
fdLock :: FD -> LockMode -> IO ()
fdLock _ _ = throwIO FileLockingNotSupported

-- | No-op implementation
fdTryLock :: FD -> LockMode -> IO Bool
fdTryLock _ _ = throwIO FileLockingNotSupported

-- | No-op implementation.
fdUnlock :: FD -> IO ()
fdUnlock _ = throwIO FileLockingNotSupported
