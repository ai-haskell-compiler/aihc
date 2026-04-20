{- ORACLE_TEST xfail Constraint implication -}
{-# LANGUAGE TypeOperators, DataKinds, ConstraintKinds, PolyKinds, UndecidableInstances #-}
module ConstraintImplication where

import Data.Kind (Constraint, Type)

class (ks :: [(Type -> Type -> Type) -> Constraint]) |- (k :: (Type -> Type -> Type) -> Constraint) where
  implies :: (Satisfies p ks) => (k p => p a b) -> p a b

class Satisfies (p :: (Type -> Type -> Type) -> Constraint) (ks :: [(Type -> Type -> Type) -> Constraint])
