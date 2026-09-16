-- | The size of a @sigset_t@ on WASI.
--
-- WASI has no signals, so its C library has no @sigset_t@ and there is no
-- size to state. Nothing on this platform can allocate one either: the calls
-- that would take a signal mask are the unsupported ones of
-- "System.Posix.Internals.Syscalls", so a caller that reaches this number
-- has already been told the operation is unsupported.
module System.Posix.Internals.Repr (sizeofSigsetT) where

import GHC.Int (Int)

-- | @sizeof(sigset_t)@, which WASI does not have.
sizeofSigsetT :: Int
sizeofSigsetT = 0
