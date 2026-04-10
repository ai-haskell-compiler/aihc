{- ORACLE_TEST pass -}
module SpecializeInstancePragma where

data Foo a = Foo a

data Bar = Bar deriving (Eq)

instance Eq a => Eq (Foo a) where
  {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
  Foo x == Foo y = x == y
