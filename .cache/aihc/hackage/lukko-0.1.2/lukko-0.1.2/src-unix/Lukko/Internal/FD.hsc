{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE Trustworthy #-}
module Lukko.Internal.FD (
    FD (..),
    fdOpen,
    fdClose,
    handleToFd,
    ) where

#include <unistd.h>
#include <fcntl.h>

import Data.Bits          ((.|.))
import Foreign.C.Error    (throwErrnoIfMinus1Retry)
import Foreign.C.Types
import Foreign.C.String   (CString, withCString)
import System.IO          (Handle)
import System.Posix.Types (CMode (..))

import qualified GHC.IO.FD        as GHC (FD (..))

import Lukko.Internal.HandleToFD (ghcHandleToFd)

-- | Opaque /file descriptor/
--
-- This is a wrapper over 'CInt'
newtype FD = FD CInt

foreign import capi interruptible "fcntl.h open"
   c_open :: CString -> CInt -> CMode -> IO CInt

foreign import ccall interruptible "close"
   c_close :: CInt -> IO CInt

-- | Open file to be used for locking.
--
-- @
-- open(path, O_RDWR | O_CREAT);
-- @
fdOpen :: FilePath -> IO FD
fdOpen fp = withCString fp $ \cfp -> do
    fd <- throwErrnoIfMinus1Retry "open" $ c_open cfp flags mode
    return (FD fd)
  where
    flags = #{const O_RDWR} .|. #{const O_CREAT}
    mode  = CMode 0o666

-- | Close lock file.
--
-- @
-- close(fd);
-- @
fdClose :: FD -> IO ()
fdClose (FD fd) =  do
    ret <- throwErrnoIfMinus1Retry "close" $ c_close fd
    return ()

-- | Convert GHC 'Handle' to lukko 'FD'.
handleToFd :: Handle -> IO FD
handleToFd h = do
    GHC.FD {GHC.fdFD = fd} <- ghcHandleToFd h
    return (FD fd)
