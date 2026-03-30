{- ORACLE_TEST pass -}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingStrategyAnyclass where

class Default a where
  def :: a

data Unit = Unit

deriving anyclass instance Default Unit