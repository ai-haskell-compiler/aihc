{- ORACLE_TEST xfail parser support pending -}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesConstraint where

import Data.Kind (Constraint, Type)

class (c :: Type -> Constraint) => UsesConstraint c where
  useConstraint :: c a => a -> a