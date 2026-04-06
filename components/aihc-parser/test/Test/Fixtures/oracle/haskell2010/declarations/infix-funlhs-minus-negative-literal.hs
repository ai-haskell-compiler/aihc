{- ORACLE_TEST pass -}
module InfixFunlhsMinusNegativeLiteral where

-- Infix function head with minus operator
x - y = x + (-1)

-- Negative literal patterns
f (-5) = "negative five"
f x = "other"
