{- ORACLE_TEST xfail pattern synonym where clause with parenthesized infix sub-pattern on LHS -}
{-# LANGUAGE PatternSynonyms #-}
module A where

data T a = a :<| a

pattern x :> y <- x :<| y
  where
    (a :<| b) :> c = a :<| (b :<| c)
