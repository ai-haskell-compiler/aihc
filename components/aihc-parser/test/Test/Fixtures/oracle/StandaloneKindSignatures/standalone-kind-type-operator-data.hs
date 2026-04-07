{- ORACLE_TEST pass -}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module StandaloneKindTypeOperatorData where

import Data.Kind (Type)

type (:+:) :: Type -> Type -> Type
data a :+: b = L
