{- ORACLE_TEST
id: gadt-newtype
category: declarations
expected: pass
-}
{-# LANGUAGE GADTSyntax #-}

module GadtNewtype where

newtype Down a where
  Down :: a -> Down a
