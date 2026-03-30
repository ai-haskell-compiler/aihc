{- ORACLE_TEST
id: kindsig-class-head
category: declarations
expected: pass
-}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesClassHead where

import Data.Kind (Type)

class Runs (f :: Type -> Type) where
  runF :: f a -> f a
