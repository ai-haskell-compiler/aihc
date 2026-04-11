{- ORACLE_TEST pass -}
module MultipleOperatorPatterns where

data Expr a = Const a | Plus a a | Minus a a | Times a a | Divide a a

foldExpr c (+) (-) (*) (/) = fold
  where
    fold x = undefined
