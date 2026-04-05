{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module Type where

f :: (?x :: Int) => Int
f = ?x
