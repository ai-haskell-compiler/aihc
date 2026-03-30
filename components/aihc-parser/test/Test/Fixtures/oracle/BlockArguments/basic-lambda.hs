{- ORACLE_TEST xfail basic lambda block argument -}
{-# LANGUAGE BlockArguments #-}
module BasicLambda where

f = id \x -> x