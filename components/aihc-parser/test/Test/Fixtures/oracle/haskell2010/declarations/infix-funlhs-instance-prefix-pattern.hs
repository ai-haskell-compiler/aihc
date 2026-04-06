{- ORACLE_TEST pass -}
module InfixFunlhsInstancePrefixPattern where

data P = Z | S P

instance Num P where
  S m - S n = m - n
