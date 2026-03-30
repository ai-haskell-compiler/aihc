{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesStockSingle where

data Tag = Tag
  deriving stock (Eq)