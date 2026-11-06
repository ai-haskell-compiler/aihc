{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Overloaded record fields.
module GHC.Records
  ( HasField (..),
  )
where

import GHC.Types (Constraint, Type)

-- | A record type @r@ that has a field @x@ of type @a@.
type HasField :: forall {k}. k -> Type -> Type -> Constraint
class HasField x r a | x r -> a where
  -- | Select the field from the record.
  getField :: r -> a
