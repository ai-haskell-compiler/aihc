{- ORACLE_TEST
id: forall-class-method
category: types
expected: pass
-}
{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllClassMethod where

class Poly f where
  poly :: forall a. f a -> f a
