{- ORACLE_TEST pass -}

module InstanceOverlappingPragma where

class Select a where
  select :: a -> String

instance {-# OVERLAPPING #-} Select [Int] where
  select _ = "ints"

instance Select [a] where
  select _ = "list"
