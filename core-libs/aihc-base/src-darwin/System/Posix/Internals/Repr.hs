-- | The size of a @sigset_t@ on Apple platforms.
--
-- A Haskell module cannot ask the C headers how big a structure is, so the
-- number is written by hand, once per platform, and the spec suite holds it
-- against the platform's own headers rather than against another number
-- written here.
module System.Posix.Internals.Repr (sizeofSigsetT) where

import GHC.Int (Int)

-- | @sizeof(sigset_t)@: Apple holds a signal mask in a single 32-bit word.
sizeofSigsetT :: Int
sizeofSigsetT = 4
