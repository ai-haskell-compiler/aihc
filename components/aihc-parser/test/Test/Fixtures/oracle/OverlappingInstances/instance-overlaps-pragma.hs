{- ORACLE_TEST pass -}

module InstanceOverlapsPragma where

class Select a where
  select :: a -> String

instance {-# OVERLAPS #-} Select [Int] where
  select _ = "ints"

instance Select [a] where
  select _ = "list"
