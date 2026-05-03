{- ORACLE_TEST pass -}
module InfixFunlhsLocalPattern where

data Expr = Var | Expr :$ Expr

f = x
  where
    a :$ b `g` c
      | True = x
