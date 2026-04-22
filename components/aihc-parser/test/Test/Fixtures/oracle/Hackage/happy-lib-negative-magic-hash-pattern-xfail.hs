{- ORACLE_TEST xfail negative MagicHash literal in function equation pattern misparses as infix minus -}
{-# LANGUAGE MagicHash #-}
module A where

import GHC.Exts

f :: Int# -> Int
f  0# = 0
f -1# = 1
f  _  = 2
