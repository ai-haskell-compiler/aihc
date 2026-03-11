{-# LANGUAGE UndecidableInstances #-}

{- | Primitive predicate simplifier.

The simplifier may not be expectected to consistently implement any
transformations whatsoever. The only guarantees are

* the output has the same or fewer operations
* the output meaning is identical to the input

See 'Rerefined.Simplify.Core' (internal module) for more details.
-}

module Rerefined.Simplify
  ( Simplify
  , TrySimplify
  , AssertSimplified
  ) where

import Rerefined.Simplify.Core ( SimplifyStep )

import Data.Kind ( type Constraint )
import GHC.TypeError ( type TypeError, type ErrorMessage(..) )
import Rerefined.Predicate ( type PredicateName )

-- | Simplify the given predicate.
--
-- Returns the input predicate if we were unable to simplify.
type Simplify p = Simplify' p

-- | Helper definition for reducing duplication.
type Simplify' p = SimplifyLoop p (SimplifyStep p)

-- | Simplification loop.
type family SimplifyLoop p mp where
    -- simplification step succeeded: try again on the simplified predicate
    SimplifyLoop p (Just p') = Simplify' p'

    -- failed to simplify: give up, return the latest predicate
    SimplifyLoop p Nothing   = p

-- | Try to simplify the given predicate.
--
-- Returns 'Nothing' if we were unable to simplify.
type TrySimplify p = TrySimplifyLoop p (SimplifyStep p)

-- | Simplification loop which returns 'Nothing' for 0 simplifications.
type family TrySimplifyLoop p mp where
    -- got a simplification: continue with the regular simplifier
    TrySimplifyLoop p (Just p') = Just (Simplify' p')

    -- couldn't simplify
    TrySimplifyLoop p Nothing   = Nothing

-- | Assert that a predicate may not be trivially simplified.
--
-- Returns the empty constraint on success, else emits a pretty type error.
--
-- Useful e.g. if you'd like to make sure a user isn't writing silly predicates.
type AssertSimplified p = AssertSimplified' p (TrySimplify p)

type family AssertSimplified' p mp' :: Constraint where
    AssertSimplified' p Nothing   = ()
    AssertSimplified' p (Just p') = TypeError
        (    Text "Predicate is trivially simplifiable"
        :$$: Text "   " :<>: Text (PredicateName 0 p)
        :$$: Text "-> " :<>: Text (PredicateName 0 p') )
