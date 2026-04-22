{- ORACLE_TEST xfail case expression not accepted as unboxed tuple element -}
{-# LANGUAGE UnboxedTuples #-}
module CaseInUnboxedTuple where

f x = (# case x of y -> y #)
