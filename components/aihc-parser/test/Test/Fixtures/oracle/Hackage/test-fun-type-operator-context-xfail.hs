{- ORACLE_TEST xfail parser rejects infix constructor syntax in data declarations -}
{-# LANGUAGE TypeOperators #-}
module TestFunTypeOperatorContextXfail where

data Var = Var

data Expr = Expr

data Ctx = (Var, Expr) :. Ctx
