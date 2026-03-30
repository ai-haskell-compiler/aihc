{- ORACLE_TEST xfail implicit parameter in type -}
{-# LANGUAGE ImplicitParams #-}
module Type where

f :: (?x :: Int) => Int
f = ?x