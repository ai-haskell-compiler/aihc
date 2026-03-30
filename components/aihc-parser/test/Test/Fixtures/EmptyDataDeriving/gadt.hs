{- ORACLE_TEST
id: gadt
category: declarations
expected: xfail
reason: gadt empty data deriving
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDeriving #-}
module GADT where

data Empty where
  deriving Show
