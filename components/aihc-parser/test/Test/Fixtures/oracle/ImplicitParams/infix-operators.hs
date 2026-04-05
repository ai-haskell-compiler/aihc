{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module InfixOperators where

f t = let { ?x = t; ?y = ?x + 1 } in ?x + ?y
