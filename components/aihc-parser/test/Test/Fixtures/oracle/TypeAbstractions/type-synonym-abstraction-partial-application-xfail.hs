{- ORACLE_TEST xfail reason="type synonym equations reject visible kind application in the rhs" -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}

module TypeSynonymAbstractionPartialApplicationXFail where

import Data.Kind (Type)

type Witnessed :: forall k. k -> Type
type Witnessed @k = PairType @k IOWitness
