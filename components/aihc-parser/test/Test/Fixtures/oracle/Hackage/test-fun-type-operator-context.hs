{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
module TestFunTypeOperatorContextXfail where

data Var = Var

data Expr = Expr

data Ctx = (Var, Expr) :. Ctx
