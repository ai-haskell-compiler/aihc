{- ORACLE_TEST pass -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ForallMultilineNestedConstraint where

import Data.Kind (Constraint, Type)

class Compat a where
  type CompatConstraint a :: Type -> Constraint
  type CompatF a :: Type -> Type

getCompatible
    :: forall a.
       ( Compat a
       , (CompatConstraint a) a
       )
    => (forall c. (Compat c, (CompatConstraint a) c) => (CompatF a) c)
    -> (CompatF a) a
getCompatible = undefined
