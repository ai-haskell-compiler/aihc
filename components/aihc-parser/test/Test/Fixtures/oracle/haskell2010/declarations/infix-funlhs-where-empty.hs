{- ORACLE_TEST pass -}
module InfixFunlhsWhereEmpty where
-- An infix function definition with an explicit empty where clause
x `myOp` y = x where {}
