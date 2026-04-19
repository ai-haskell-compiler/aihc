{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}
module ParenthesizedSingleVia where

newtype MyInt = MyInt Int
  deriving (Show) via Int
