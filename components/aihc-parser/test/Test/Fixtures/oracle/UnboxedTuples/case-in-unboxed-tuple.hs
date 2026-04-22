{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
module CaseInUnboxedTuple where

f x = (# case x of y -> y #)
