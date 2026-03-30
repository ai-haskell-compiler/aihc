{- ORACLE_TEST xfail basic if block argument -}
{-# LANGUAGE BlockArguments #-}
module BasicIf where

f x y z = id if x then y else z