{- ORACLE_TEST pass -}

module OperatorPattern where

data Expr a = Plus a a

foldExpr (+) = fold
    where
        fold (Plus x y) = fold x + fold y
