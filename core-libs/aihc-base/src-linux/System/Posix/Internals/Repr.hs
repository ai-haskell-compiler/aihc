-- | The size of a @sigset_t@ on Linux.
--
-- A Haskell module cannot ask the C headers how big a structure is, so the
-- number is written by hand, once per platform, and the spec suite holds it
-- against the platform's own headers rather than against another number
-- written here.
module System.Posix.Internals.Repr (sizeofSigsetT) where

import GHC.Int (Int)

-- | @sizeof(sigset_t)@: glibc and musl both reserve 1024 bits for a signal mask.
sizeofSigsetT :: Int
sizeofSigsetT = 128
