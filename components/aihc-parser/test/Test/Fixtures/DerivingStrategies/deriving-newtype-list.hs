{- ORACLE_TEST
id: deriving-newtype-list
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesNewtypeList where

newtype Total = Total Int
  deriving newtype Eq
