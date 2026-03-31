{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}
module ComplexVia where

newtype T a = T a
  deriving Eq via (Maybe a)