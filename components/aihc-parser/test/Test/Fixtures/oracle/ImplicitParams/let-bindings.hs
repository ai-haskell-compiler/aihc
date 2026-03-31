{- ORACLE_TEST xfail let-bindings with implicit params -}
{-# LANGUAGE ImplicitParams #-}
module LetBindings where

f t = let { ?x = t; ?y = ?x + (1::Int) } in ?x + ?y
