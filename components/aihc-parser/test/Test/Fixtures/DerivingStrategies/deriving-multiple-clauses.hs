{- ORACLE_TEST
id: deriving-multiple-clauses
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesMultipleClauses where

data Box a = Box a
  deriving stock (Eq)
  deriving stock (Show)
