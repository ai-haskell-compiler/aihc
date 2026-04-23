{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
module A where

import GHC.Exts

f :: Int# -> Int
f  0# = 0
f -1# = 1
f  _  = 2
