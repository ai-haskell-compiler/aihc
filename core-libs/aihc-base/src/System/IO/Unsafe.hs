{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.IO.Unsafe (unsafePerformIO) where

import GHC.IO (IO (..))
import GHC.Prim (realWorld#)

unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) =
  case action realWorld# of
    (# _, result #) -> result
