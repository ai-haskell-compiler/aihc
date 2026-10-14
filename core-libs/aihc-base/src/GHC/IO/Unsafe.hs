{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeDupableInterleaveIO,
  )
where

import GHC.Prim (noDuplicate#, realWorld#)
import GHC.Prim.IO (IO (..))

-- | An action that must not run twice claims the evaluations of its thread
-- first. Another thread that evaluates the same thunk then waits for this
-- one, as in GHC.
unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) =
  unsafeDupablePerformIO (IO (\state -> action (noDuplicate# state)))

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO (IO action) =
  case action realWorld# of
    (# _, result #) -> result

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO action) =
  unsafeDupableInterleaveIO (IO (\state -> action (noDuplicate# state)))

{- HLINT ignore unsafeDupableInterleaveIO "Use tuple-section" -}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO action =
  IO (\state -> (# state, unsafePerformIO action #))
