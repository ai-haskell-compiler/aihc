{-# LANGUAGE CApiFFI #-}

-- | File locks with @flock@.
module GHC.IO.Handle.Lock.Descriptor
  ( lockDescriptor,
    unlockDescriptor,
  )
where

import Data.Bits ((.|.))
import Foreign.C.Error (eINTR, eWOULDBLOCK, getErrno, throwErrno, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import GHC.IO.FD (FD (..))
import GHC.IO.Handle.Lock.Common (LockMode (..))
import Prelude

-- | Lock the file of a descriptor. When the call must not wait and another
-- process holds a lock that conflicts, the result is 'False'.
lockDescriptor :: String -> FD -> LockMode -> Bool -> IO Bool
lockDescriptor location descriptor mode wait = attempt
  where
    operation =
      ( case mode of
          SharedLock -> lockShared
          ExclusiveLock -> lockExclusive
      )
        .|. (if wait then 0 else lockNonBlocking)
    attempt = do
      result <- c_flock (fdFD descriptor) operation
      if result == 0
        then return True
        else do
          errno <- getErrno
          if errno == eINTR
            then attempt
            else
              if errno == eWOULDBLOCK && not wait
                then return False
                else throwErrno location

-- | Release the lock of the file of a descriptor.
unlockDescriptor :: String -> FD -> IO ()
unlockDescriptor location descriptor =
  throwErrnoIfMinus1_ location (c_flock (fdFD descriptor) lockUnlock)

foreign import capi unsafe "sys/file.h flock"
  c_flock :: CInt -> CInt -> IO CInt

foreign import capi unsafe "sys/file.h value LOCK_SH"
  lockShared :: CInt

foreign import capi unsafe "sys/file.h value LOCK_EX"
  lockExclusive :: CInt

foreign import capi unsafe "sys/file.h value LOCK_NB"
  lockNonBlocking :: CInt

foreign import capi unsafe "sys/file.h value LOCK_UN"
  lockUnlock :: CInt
