{- ORACLE_TEST
id: complex-via
category: declarations
expected: xfail
reason: complex via type
-}
{-# LANGUAGE DerivingVia #-}
module ComplexVia where

newtype T a = T a
  deriving Eq via (Maybe a)
