{-# LANGUAGE MagicHash #-}

module PrimitiveChecks (primitiveChecks) where

import GHC.Exts (Proxy#, isTrue#, proxy#, reallyUnsafePtrEquality#)
import GHC.Magic (lazy, runRW#)

proxy :: a -> Proxy# ()
proxy _ = proxy#

consume :: Proxy# () -> Int
consume _ = 42

loop :: Int
loop = loop

-- | GHC exports 'runRW#' and 'lazy' from GHC.Magic. bytestring imports them
-- from there.
magic :: Int
magic = runRW# (\_ -> lazy 7)

primitiveChecks :: Bool
primitiveChecks =
  consume (proxy True) == 42
    && magic == 7
    && isTrue# (reallyUnsafePtrEquality# loop loop)
    && not (isTrue# (reallyUnsafePtrEquality# (Just True) (Just False)))
