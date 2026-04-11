{- ORACLE_TEST xfail reason="Operator section in pattern position not handled" -}
{-# LANGUAGE Haskell2010 #-}

module OperatorSectionInPattern where

class Eq1 f where
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

data Free f a = Pure a | Impure (f (Free f a))

instance Eq1 f => Eq1 (Free f) where
  liftEq (==) (Pure a) (Pure b) = a == b
  liftEq (==) (Impure a) (Impure b) = undefined
  liftEq _ _ _ = False
