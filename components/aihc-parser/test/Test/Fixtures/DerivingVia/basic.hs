{- ORACLE_TEST
id: basic
category: declarations
expected: xfail
reason: basic deriving via
-}
{-# LANGUAGE DerivingVia #-}
module Basic where

newtype MyInt = MyInt Int
  deriving Show via Int
