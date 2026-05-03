{- ORACLE_TEST pass -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module ConstraintKindSignatureRhsRoundtrip where

import Data.Coerce (Coercible)
import Data.Kind (Constraint)

type Coercible2 f = (forall a b c d. (Coercible a b, Coercible c d) => Coercible (f a c) (f b d) :: Constraint)
