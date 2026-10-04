-- | WASI has no file locks. Each call reports 'FileLockingNotSupported',
-- as GHC does on a platform without locks.
module GHC.IO.Handle.Lock.Descriptor
  ( lockDescriptor,
    unlockDescriptor,
  )
where

import GHC.IO (throwIO)
import GHC.IO.FD (FD)
import GHC.IO.Handle.Lock.Common (FileLockingNotSupported (..), LockMode)
import Prelude

lockDescriptor :: String -> FD -> LockMode -> Bool -> IO Bool
lockDescriptor _ _ _ _ = throwIO FileLockingNotSupported

unlockDescriptor :: String -> FD -> IO ()
unlockDescriptor _ _ = throwIO FileLockingNotSupported
