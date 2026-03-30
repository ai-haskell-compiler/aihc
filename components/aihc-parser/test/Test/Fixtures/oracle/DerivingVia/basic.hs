{- ORACLE_TEST xfail basic deriving via -}
{-# LANGUAGE DerivingVia #-}
module Basic where

newtype MyInt = MyInt Int
  deriving Show via Int