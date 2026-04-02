{- ORACLE_TEST pass -}

module InstanceIncoherentPragma where

class Select a where
  select :: a -> String

instance {-# INCOHERENT #-} Select [a] where
  select _ = "list"

instance Select [Int] where
  select _ = "ints"
