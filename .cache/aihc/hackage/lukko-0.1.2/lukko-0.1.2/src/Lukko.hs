{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
-- | Open 'Handle' based locking
module Lukko (
    FileLockingNotSupported(..),
    Impl.fileLockingSupported,
    Impl.FileLockingSupported,
    FileLockingMethod (..),
    Impl.fileLockingMethod,
    LockMode(..),
    -- * File descriptors
    FD,
    fdOpen,
    fdClose,
    fdLock,
    fdTryLock,
    fdUnlock,
    -- * Handles
    handleToFd,
    hLock,
    hTryLock,
    hUnlock,
    ) where

{- Parts of these software is derived from GHC sources
   distributed under BSD-3-Clause license:

The Glasgow Haskell Compiler License

Copyright 2004, The University Court of the University of Glasgow.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

-}

import Control.Monad (void)
import System.IO     (Handle)

import Lukko.Internal.Types

import qualified Lukko.Internal.FD as Impl

#if defined(USE_OFD_LOCKING)
import qualified Lukko.OFD as Impl
#elif defined(USE_FLOCK)
import qualified Lukko.FLock as Impl
#elif defined(USE_WINDOWS_LOCK)
import qualified Lukko.Windows as Impl
#else
import qualified Lukko.NoOp as Impl
#endif

-------------------------------------------------------------------------------
-- Handles
-------------------------------------------------------------------------------

-- | If a 'Handle' references a file descriptor, attempt to lock contents of the
-- underlying file in appropriate mode. If the file is already locked in
-- incompatible mode, this function blocks until the lock is established. The
-- lock is automatically released upon closing a 'Handle'.
--
-- Things to be aware of:
--
-- 1) This function may block inside a C call. If it does, in order to be able
-- to interrupt it with asynchronous exceptions and/or for other threads to
-- continue working, you MUST use threaded version of the runtime system.
--
-- 2) The implementation uses 'LockFileEx' on Windows,
-- /open file descriptor/ locks on Linux, and 'flock' otherwise,
-- hence all of their caveats also apply here.
--
-- 3) On non-Windows plaftorms that don't support 'flock' (e.g. Solaris) this
-- function throws 'FileLockingNotImplemented'. We deliberately choose to not
-- provide @fcntl@ based locking instead because of its broken semantics.
--
hLock :: Handle -> LockMode -> IO ()
hLock = Impl.hLock

-- | Non-blocking version of 'hLock'.
hTryLock :: Handle -> LockMode -> IO Bool
hTryLock = Impl.hTryLock

-- | Release a lock taken with 'hLock' or 'hTryLock'.
hUnlock :: Handle -> IO ()
hUnlock = Impl.hUnlock

-------------------------------------------------------------------------------
-- File descriptors
-------------------------------------------------------------------------------

-- | Opaque /file descriptor/
--
-- An @int@ / 'CInt' on unix systems,
-- and 'HANDLE' on windows.
type FD = Impl.FD

-- | Open file to be used for locking.
fdOpen :: FilePath -> IO FD
fdOpen = Impl.fdOpen

-- | Close lock file.
fdClose :: FD -> IO ()
fdClose = Impl.fdClose

-- | Convert GHC 'Handle' to lukko 'FD'.
handleToFd :: Handle -> IO FD
handleToFd = Impl.handleToFd

-- | Like 'hLock', but work on "raw" file descriptor,
-- as handled by 'fdOpen' and 'fdClose'.
fdLock :: Impl.FD -> LockMode -> IO ()
fdLock = Impl.fdLock

-- | Non-blocking version of 'fdLock'.
fdTryLock :: Impl.FD -> LockMode -> IO Bool
fdTryLock = Impl.fdTryLock

-- | Release a lock taken with 'fdLock' or 'fdTryLock'.
fdUnlock :: Impl.FD -> IO ()
fdUnlock = Impl.fdUnlock
