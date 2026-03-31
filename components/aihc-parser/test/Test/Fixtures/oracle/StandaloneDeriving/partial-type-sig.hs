{- ORACLE_TEST xfail wildcard in deriving instance -}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module PartialTypeSigDeriving where

data Foo a = Foo a

deriving instance _ => Eq (Foo a)
