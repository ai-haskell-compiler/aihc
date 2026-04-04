{- ORACLE_TEST xfail infix instance method with prefix constructor patterns -}
module InfixFunlhsInstancePrefixPattern where

data P = Z | S P

instance Num P where
  S m - S n = m - n
