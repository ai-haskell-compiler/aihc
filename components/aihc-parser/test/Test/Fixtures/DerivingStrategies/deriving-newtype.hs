{- ORACLE_TEST
id: deriving-newtype
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesNewtype where

newtype Age = Age Int
  deriving newtype (Eq, Ord, Show)
