{- ORACLE_TEST xfail basic type data declarations -}
{-# LANGUAGE TypeData #-}
module Basic where

type data Nat = Zero | Succ Nat