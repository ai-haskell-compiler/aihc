{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
module UnboxedSumConstructor where

data Sum2# a b
  = (# a | #)
  | (# | b #)

data Sum3# a b c
  = (# a | | #)
  | (# | b | #)
  | (# | | c #)
