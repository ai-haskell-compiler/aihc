{- ORACLE_TEST
id: gadt-context
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}

module GadtContext where

data Set a where
  MkSet :: Eq a => [a] -> Set a

data NumInst a where
  MkNumInst :: Num a => NumInst a
