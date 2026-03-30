{- ORACLE_TEST
id: type
category: types
expected: xfail
reason: implicit parameter in type
-}
{-# LANGUAGE ImplicitParams #-}
module Type where

f :: (?x :: Int) => Int
f = ?x
