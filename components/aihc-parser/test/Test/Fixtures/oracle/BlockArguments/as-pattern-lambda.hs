{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021, BlockArguments #-}
module AsPatternLambda where

f = id \x@(Just y) -> y
