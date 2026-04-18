{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module TypeSynonymAbstraction where

import Data.Kind (Type)

data Proxy (a :: k) = Proxy

type Witnessed :: forall k. (k -> Type) -> k -> Type
type Witnessed @k (f :: k -> Type) (a :: k) = Proxy a
