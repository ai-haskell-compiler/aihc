{- ORACLE_TEST
id: type-family-arity
category: declarations
expected: xfail
reason: type family with arity
-}
{-# LANGUAGE TypeFamilies #-}
module TypeFamilyArity where

import Data.Kind (Type)

type family F a b :: Type -> Type
