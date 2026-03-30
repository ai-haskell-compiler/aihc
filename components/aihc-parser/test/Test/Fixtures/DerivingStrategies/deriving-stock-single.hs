{- ORACLE_TEST
id: deriving-stock-single
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesStockSingle where

data Tag = Tag
  deriving stock (Eq)
