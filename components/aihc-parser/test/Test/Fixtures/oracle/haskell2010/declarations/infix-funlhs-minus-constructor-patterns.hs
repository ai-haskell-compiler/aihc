{- ORACLE_TEST pass -}
module InfixFunlhsMinusConstructorPatterns where

data P = Z | S P

-- Infix function head with minus operator and constructor patterns
S m - S n = m - n
Z - x = x
x - Z = x
