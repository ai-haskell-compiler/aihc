{- ORACLE_TEST
id: multiple-classes
category: declarations
expected: xfail
reason: deriving multiple classes via
-}
{-# LANGUAGE DerivingVia #-}
module MultipleClasses where

newtype MyInt = MyInt Int
  deriving (Eq, Ord) via Int
