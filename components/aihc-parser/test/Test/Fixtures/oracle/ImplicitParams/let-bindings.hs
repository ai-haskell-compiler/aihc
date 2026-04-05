{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module LetBindings where

f t = let { ?x = t; ?y = ?x + (1::Int) } in ?x + ?y
