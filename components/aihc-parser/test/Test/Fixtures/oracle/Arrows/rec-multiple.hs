{- ORACLE_TEST xfail arrow rec with multiple bindings -}
{-# LANGUAGE Arrows #-}
module ArrowRecMultiple where

import Control.Arrow

f g h = proc x -> do
  rec y <- g -< x + 1
      z <- h -< y + 2
  returnA -< (y, z)
