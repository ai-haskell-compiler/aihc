{-# LANGUAGE CPP #-}

{-|
This module intends to make the operations of @System.Posix.Process@ available
on all platforms.
-}
module System.PosixCompat.Process (
      getProcessID
    ) where

#if defined(mingw32_HOST_OS)

import System.Posix.Types (ProcessID)
import System.Win32.Process (getCurrentProcessId)

getProcessID :: IO ProcessID
getProcessID = fromIntegral <$> getCurrentProcessId

#elif defined(wasm32_HOST_ARCH)

import System.Posix.Types (ProcessID)

getProcessID :: IO ProcessID
getProcessID = pure 1

#else

import System.Posix.Process

#endif
