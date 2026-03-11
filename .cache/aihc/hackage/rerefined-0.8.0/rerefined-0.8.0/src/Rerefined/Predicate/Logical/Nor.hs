{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Nor where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | NOR logic gate. Also called non-disjunction, or joint denial.
data Nor l r

-- | Precedence of 2 (matching 'Data.Bool.||').
instance (Predicate l, Predicate r) => Predicate (Nor l r) where
    type PredicateName d (Nor l r) = PredicateNameBOp " ⊽ " 2 d l r

instance (Refine l a, Refine r a, KnownPredicateName (Nor l r))
  => Refine (Nor l r) a where
    validate p a =
        case l of
          Just _ ->
            case r of
              Just _  -> Nothing
              Nothing -> validateFail p "NOR: left succeeded"  []
          Nothing ->
            case r of
              Just _  -> validateFail p "NOR: right succeeded" []
              Nothing -> validateFail p "NOR: l&r succeeded"   []
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
