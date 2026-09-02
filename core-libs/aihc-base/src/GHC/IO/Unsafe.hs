{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeDupableInterleaveIO,
  )
where

import GHC.Prim (realWorld#)
import GHC.Prim.IO (IO (..))

unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) =
  case action realWorld# of
    (# _, result #) -> result

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = unsafeDupableInterleaveIO

{- HLINT ignore unsafeDupableInterleaveIO "Use tuple-section" -}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO action =
  IO (\state -> (# state, unsafePerformIO action #))
