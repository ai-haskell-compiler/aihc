{-# LANGUAGE CPP #-}

module PtrPoker.Compat.ForeignPtr where

import PtrPoker.Prelude

#if MIN_VERSION_base(4,15,0)
import qualified GHC.ForeignPtr
#else
import qualified Foreign.ForeignPtr
#endif

-- | 'unsafeWithForeignPtr' compatibility wrapper.
--
-- GHC 9.0 made 'withForeignPtr' sound at the cost of performance. If you want to
-- use the faster unsafe implementation, it's now at 'unsafeWithForeignPtr', and
-- GHC asks you to promise that your continuation does not diverge. All we do here
-- is @memcpy@ bytestrings, so we gladly pinky swear. For more detail, see Ben
-- Gamari's post:
-- <https://www.haskell.org/ghc/blog/20210607-the-keepAlive-story.html>
--
-- Note that fumieval's mason uses 'unsafeWithForeignPtr' in the same way also to
-- copy bytestrings.
{-# INLINE unsafeWithForeignPtr #-}
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
#if MIN_VERSION_base(4,15,0)
unsafeWithForeignPtr =
    GHC.ForeignPtr.unsafeWithForeignPtr
#else
unsafeWithForeignPtr =
    -- same implementation as new @unsafeWithForeignPtr@ (it was always unsafe)
    Foreign.ForeignPtr.withForeignPtr
#endif
