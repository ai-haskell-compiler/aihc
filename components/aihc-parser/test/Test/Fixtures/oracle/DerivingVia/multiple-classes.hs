{- ORACLE_TEST xfail deriving multiple classes via -}
{-# LANGUAGE DerivingVia #-}
module MultipleClasses where

newtype MyInt = MyInt Int
  deriving (Eq, Ord) via Int