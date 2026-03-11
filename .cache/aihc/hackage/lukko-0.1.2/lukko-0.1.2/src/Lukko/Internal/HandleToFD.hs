{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
module Lukko.Internal.HandleToFD (ghcHandleToFd) where

import qualified GHC.IO.FD as GHC (FD (..))
import           System.IO (Handle)

import qualified GHC.IO.Handle.FD as GHC (handleToFd)

ghcHandleToFd :: Handle -> IO GHC.FD
ghcHandleToFd = GHC.handleToFd
