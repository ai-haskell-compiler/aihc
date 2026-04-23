{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
module A where

import GHC.Exts

-- Multiple negative MagicHash patterns in function equations.
f :: Int# -> Int
f  0#   = 0
f -1#   = 1
f -100# = 2
f  _    = 3
