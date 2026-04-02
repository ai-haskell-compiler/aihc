{- ORACLE_TEST pass -}

module InstanceOverlappablePragma where

class Select a where
  select :: a -> String

instance {-# OVERLAPPABLE #-} Select [a] where
  select _ = "list"

instance Select [Int] where
  select _ = "ints"
