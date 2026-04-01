{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module TypeFamilyArity where

import Data.Kind (Type)

type family F a b :: Type -> Type