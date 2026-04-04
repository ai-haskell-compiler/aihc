{- ORACLE_TEST xfail poly-arity gadt constructor kind binder -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module GadtConstructorKindBinder where

data T where
  C :: (x :: *) -> T
