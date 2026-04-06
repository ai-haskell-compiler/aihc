{- ORACLE_TEST pass -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module GadtConstructorKindBinder where

data T where
  C :: (x :: *) -> T
