{- ORACLE_TEST
id: deriving-stock-multi
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesStockMulti where

data Pair a = Pair a a
  deriving stock (Eq, Show)
