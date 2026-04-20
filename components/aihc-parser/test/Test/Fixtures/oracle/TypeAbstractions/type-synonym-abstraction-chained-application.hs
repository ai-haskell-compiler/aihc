{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module TypeSynonymAbstractionChainedApplication where

import Data.Kind (Type)

type PairType :: forall k. (k -> Type) -> (k -> Type) -> k -> Type
data PairType f g a

type IOWitness :: forall k. k -> Type
data IOWitness a

type Witnessed :: forall k. k -> Type
type Witnessed @k = PairType @k (IOWitness @k) (IOWitness @k)
