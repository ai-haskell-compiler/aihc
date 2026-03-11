{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.And where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common
import Rerefined.Refine

-- | Logical conjunction. Also AND logic gate.
data And l r

-- | Precedence of 3 (matching 'Data.Bool.&&').
instance (Predicate l, Predicate r) => Predicate (And l r) where
    type PredicateName d (And l r) = PredicateNameBOp " ∧ " 3 d l r

instance (Refine l a, Refine r a, KnownPredicateName (And l r))
  => Refine (And l r) a where
    validate p a =
        case l of
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er -> validateFail p "AND:  right failed"    [er    ]
          Just el ->
            case r of
              Nothing -> validateFail p "AND:   left failed"    [    el]
              Just er -> validateFail p "AND:    l&r failed"    [er, el]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a

-- | Take just the left predicate from an 'And'.
rerefineAndL :: Refined (And l r) a -> Refined l a
rerefineAndL = unsafeRerefine

-- | Take just the right predicate from an 'And'.
rerefineAndR :: Refined (And l r) a -> Refined r a
rerefineAndR = unsafeRerefine

-- | Eliminate an 'And' by applying the left predicate, then the right.
eliminateAndLR :: Refined (And l r) a -> Refined r (Refined l a)
eliminateAndLR = unsafeRefine . unsafeRefine . unrefine

-- | Eliminate an 'And' by applying the right predicate, then the left.
eliminateAndRL :: Refined (And l r) a -> Refined l (Refined r a)
eliminateAndRL = unsafeRefine . unsafeRefine . unrefine

-- | Introduce an 'And' given a double-'Refined'. Inner is left.
introduceAndLR :: Refined r (Refined l a) -> Refined (And l r) a
introduceAndLR = unsafeRefine . unrefine . unrefine

-- | Introduce an 'And' given a double-'Refined'. Inner is right.
introduceAndRL :: Refined l (Refined r a) -> Refined (And l r) a
introduceAndRL = unsafeRefine . unrefine . unrefine
