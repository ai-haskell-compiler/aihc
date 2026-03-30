{- ORACLE_TEST xfail type family with arity -}
{-# LANGUAGE TypeFamilies #-}
module TypeFamilyArity where

import Data.Kind (Type)

type family F a b :: Type -> Type