{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesNewtypeList where

newtype Total = Total Int
  deriving newtype Eq