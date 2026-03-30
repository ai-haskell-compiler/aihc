{- ORACLE_TEST pass -}
module InfixFunlhsLocal where
f = g
  where
    x <+> y = x + y
    g = 1 <+> 2