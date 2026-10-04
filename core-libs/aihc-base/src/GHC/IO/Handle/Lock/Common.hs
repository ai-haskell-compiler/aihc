-- | The types that every implementation of "GHC.IO.Handle.Lock" shares.
module GHC.IO.Handle.Lock.Common
  ( FileLockingNotSupported (..),
    LockMode (..),
  )
where

import GHC.Exception.Type (Exception (..))
import Prelude

-- | The platform has no file locks.
data FileLockingNotSupported = FileLockingNotSupported
  deriving (Show)

instance Exception FileLockingNotSupported

-- | A shared lock lets other processes hold a shared lock at the same time.
-- An exclusive lock lets no other process hold a lock.
data LockMode = SharedLock | ExclusiveLock
