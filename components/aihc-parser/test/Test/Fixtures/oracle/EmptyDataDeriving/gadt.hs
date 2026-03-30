{- ORACLE_TEST xfail gadt empty data deriving -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}
module GADT where

data Empty where
  deriving Show