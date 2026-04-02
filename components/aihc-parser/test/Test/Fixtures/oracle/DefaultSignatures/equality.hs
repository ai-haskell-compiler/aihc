{- ORACLE_TEST pass -}
{-# LANGUAGE DefaultSignatures #-}
module Equality where

class Inj a where
  inj :: a -> a
  default inj :: (p ~ a) => p -> a
  inj = \x -> x
