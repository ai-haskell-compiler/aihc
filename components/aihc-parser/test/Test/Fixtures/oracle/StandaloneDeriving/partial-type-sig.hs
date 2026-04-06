{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module PartialTypeSigDeriving where

data Foo a = Foo a

deriving instance _ => Eq (Foo a)
