{- ORACLE_TEST xfail operator pattern in function definition -}

module OperatorPattern where

data Expr a = Plus a a

foldExpr (+) = fold
    where
        fold (Plus x y) = fold x + fold y
