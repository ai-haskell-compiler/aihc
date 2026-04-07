{- ORACLE_TEST pass -}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesMultipleConstraints where

import Data.Kind (Constraint, Type)

-- Multiple constraints with kind annotations
class (c1 :: Type -> Constraint, c2 :: Type -> Constraint) => MultiConstraint c1 c2 where
  multiMethod :: (c1 a, c2 a) => a -> a

-- Mixed: regular constraint + kind-annotated constraint
class (Show a, c :: Type -> Constraint) => MixedConstraint c a where
  mixedMethod :: c a => a -> String

-- Nested parentheses with kind annotation
class ((c :: Type -> Constraint)) => NestedParens c where
  nestedMethod :: c a => a -> a
