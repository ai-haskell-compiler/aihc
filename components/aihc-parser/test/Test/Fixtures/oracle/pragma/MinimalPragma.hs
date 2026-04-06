{- ORACLE_TEST pass -}
module MinimalPragma where

class PartialEq a where
  peq :: a -> a -> Bool
  pne :: a -> a -> Bool
  peq x y = not (pne x y)
  pne x y = not (peq x y)
  {-# MINIMAL peq | pne #-}
