{- ORACLE_TEST xfail SPECIALIZE instance pragma not preserved in pretty-printer roundtrip -}
module SpecializeInstancePragma where

data Foo a = Foo a

data Bar = Bar deriving (Eq)

instance Eq a => Eq (Foo a) where
  {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
  Foo x == Foo y = x == y
