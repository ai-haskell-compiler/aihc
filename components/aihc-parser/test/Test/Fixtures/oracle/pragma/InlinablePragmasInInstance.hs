{- ORACLE_TEST pass -}
module InlinablePragmasInInstance where

class C a where
  foo :: a -> Int
  bar :: a -> Int

data A = A

instance C A where
  {-# INLINABLE foo #-}
  foo _ = 0
  {-# INLINABLE bar #-}
  bar _ = 1
