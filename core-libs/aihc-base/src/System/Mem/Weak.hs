module System.Mem.Weak
  ( Weak,
    mkWeak,
    deRefWeak,
    finalize,
    mkWeakPtr,
    addFinalizer,
    mkWeakPair,
  )
where

import GHC.Weak (Weak, deRefWeak, finalize, mkWeak)
import Prelude

mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)
mkWeakPtr key = mkWeak key key

mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k, v))
mkWeakPair key value = mkWeak key (key, value)

-- | Attach a finalizer to a key. The runtime does not run a finalizer
-- when the key becomes unreachable.
addFinalizer :: key -> IO () -> IO ()
addFinalizer key finalizer = do
  _ <- mkWeakPtr key (Just finalizer)
  return ()
