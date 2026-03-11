{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Safe               #-}
module Lukko.Internal.Types where

import Control.Exception (Exception)
import Data.Typeable     (Typeable)

-- | Exception thrown by 'hLock' on non-Windows platforms that don't support
-- @flock@.
data FileLockingNotSupported = FileLockingNotSupported
  deriving (Typeable, Show)

instance Exception FileLockingNotSupported

-- | Indicates a mode in which a file should be locked.
data LockMode = SharedLock | ExclusiveLock
  deriving (Typeable)

-- | Potentially availble lock methods.
data FileLockingMethod
    = MethodOFD      -- ^ open file descriptor locking
    | MethodFLock    -- ^ BSD @flock@
    | MethodWindows  -- ^ Windows locking
    | MethodNoOp     -- ^ No-Op (throws 'FileLockingNotSupported')
  deriving (Typeable, Eq, Ord, Enum, Bounded, Show)
