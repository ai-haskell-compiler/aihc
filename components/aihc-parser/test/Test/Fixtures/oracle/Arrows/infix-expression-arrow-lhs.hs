{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module M where

f = proc x -> [] `a` [] -< let y = (# #) in 846#
