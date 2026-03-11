{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Or where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | Logical disjunction. Also OR logic gate.
data Or l r

-- | Precedence of 2 (matching 'Data.Bool.||').
instance (Predicate l, Predicate r) => Predicate (Or l r) where
    type PredicateName d (Or l r) = PredicateNameBOp " ∨ " 2 d l r

instance (Refine l a, Refine r a, KnownPredicateName (Or l r))
  => Refine (Or l r) a where
    validate p a =
        case l of
          Nothing -> Nothing
          Just el ->
            case r of
              Nothing -> Nothing
              Just er -> validateFail p "OR: l&r failed" [er, el]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
