{- ORACLE_TEST xfail parser accepts but pretty-printer roundtrip formatting differs -}
{-# LANGUAGE BlockArguments #-}
module BasicIf where

f x y z = id if x then y else z