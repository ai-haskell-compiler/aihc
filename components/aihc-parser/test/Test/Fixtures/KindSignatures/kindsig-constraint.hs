{- ORACLE_TEST
id: kindsig-constraint
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesConstraint where

import Data.Kind (Constraint, Type)

class (c :: Type -> Constraint) => UsesConstraint c where
  useConstraint :: c a => a -> a
