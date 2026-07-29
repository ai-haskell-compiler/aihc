{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Primitive pinned-buffer allocation shared by the internal IO layers.
module GHC.IO.Buffer.Internal (withPinnedByteArray) where

import GHC.IO (IO (..))
import GHC.Prim (MutableByteArray#, RealWorld, newPinnedByteArray#)

-- | Allocate zero-filled pinned storage for the duration of an action. The
-- proof-of-concept runtime does not reclaim the allocation yet.
withPinnedByteArray :: Int# -> (MutableByteArray# RealWorld -> IO a) -> IO a
withPinnedByteArray size action =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            case action buffer of
              IO run -> run allocatedState
    )
