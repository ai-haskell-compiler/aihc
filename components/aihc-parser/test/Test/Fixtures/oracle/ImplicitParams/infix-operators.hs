{- ORACLE_TEST xfail infix operators with implicit params -}
{-# LANGUAGE ImplicitParams #-}
module InfixOperators where

f t = let { ?x = t; ?y = ?x + 1 } in ?x + ?y
