{- ORACLE_TEST pass -}
module InfixFunlhsGuarded where
x <=> y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ