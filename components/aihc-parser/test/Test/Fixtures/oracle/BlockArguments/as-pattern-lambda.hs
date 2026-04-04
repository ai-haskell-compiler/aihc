{- ORACLE_TEST xfail block argument lambda with as-pattern binder -}
{-# LANGUAGE GHC2021, BlockArguments #-}
module AsPatternLambda where

f = id \x@(Just y) -> y
