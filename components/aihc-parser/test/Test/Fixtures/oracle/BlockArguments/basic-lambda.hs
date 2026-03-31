{- ORACLE_TEST xfail parser accepts but pretty-printer roundtrip formatting differs -}
{-# LANGUAGE BlockArguments #-}
module BasicLambda where

f = id \x -> x