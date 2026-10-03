{-# LANGUAGE NamedFieldPuns #-}

-- | Advisory locks on the file of a handle. A lock is between processes:
-- the operating system releases it when the file closes.
module GHC.IO.Handle.Lock
  ( FileLockingNotSupported (..),
    LockMode (..),
    hLock,
    hTryLock,
    hUnlock,
  )
where

import Data.Typeable (cast)
import GHC.IO.FD (FD (..))
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Lock.Common (FileLockingNotSupported (..), LockMode (..))
import GHC.IO.Handle.Lock.Descriptor (lockDescriptor, unlockDescriptor)
import GHC.Internal.IO.Types (Handle, Handle__ (..), IOErrorType (..), IOException (..), ioException)
import Prelude

-- | Lock the file of the handle. Wait while another process holds a lock
-- that this lock cannot share.
hLock :: Handle -> LockMode -> IO ()
hLock handle mode = do
  _ <- withDescriptor "hLock" handle (\descriptor -> lockDescriptor "hLock" descriptor mode True)
  return ()

-- | Lock the file of the handle if no wait is necessary. The result tells
-- whether the lock is now held.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock handle mode =
  withDescriptor "hTryLock" handle (\descriptor -> lockDescriptor "hTryLock" descriptor mode False)

-- | Release the lock of the file of the handle.
hUnlock :: Handle -> IO ()
hUnlock handle = withDescriptor "hUnlock" handle (unlockDescriptor "hUnlock")

-- | Run an action on the operating system descriptor of a handle. A handle
-- of another device has no descriptor to lock.
withDescriptor :: String -> Handle -> (FD -> IO a) -> IO a
withDescriptor location handle action =
  withHandle_ location handle $ \Handle__ {haDevice} ->
    case cast haDevice of
      Just descriptor -> action descriptor
      Nothing ->
        ioException
          IOError
            { ioe_handle = Just handle,
              ioe_type = InappropriateType,
              ioe_location = location,
              ioe_description = "handle is not a file descriptor",
              ioe_errno = Nothing,
              ioe_filename = Nothing
            }
