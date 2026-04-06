{- ORACLE_TEST pass -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExplicitForAllInstance where

class C a where
  c :: a -> ()

instance forall a. C [a] where
  c _ = ()
