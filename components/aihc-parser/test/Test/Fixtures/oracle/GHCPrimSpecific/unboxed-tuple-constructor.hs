{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module UnboxedTupleConstructor where

data Unit# = (# #)

data Tuple2# a b = (# a, b #)

data Tuple3# a b c = (# a, b, c #)
