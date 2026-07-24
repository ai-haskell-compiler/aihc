{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Operation-independent integration with the green-thread scheduler.
-- Platform backends remain behind the native runtime request ABI.
module GHC.Event (awaitIO) where

import GHC.IO (IO (..))
import GHC.Prim (awaitIO#)
import GHC.Ptr (Ptr (..))

-- | Suspend the current green thread until an opaque runtime request is ready.
awaitIO :: Ptr request -> IO ()
awaitIO (Ptr request) =
  IO
    ( \state ->
        case awaitIO# request state of
          nextState -> (# nextState, () #)
    )
