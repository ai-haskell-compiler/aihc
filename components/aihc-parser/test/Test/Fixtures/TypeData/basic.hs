{- ORACLE_TEST
id: basic
category: declarations
expected: xfail
reason: basic type data declarations
-}
{-# LANGUAGE TypeData #-}
module Basic where

type data Nat = Zero | Succ Nat
