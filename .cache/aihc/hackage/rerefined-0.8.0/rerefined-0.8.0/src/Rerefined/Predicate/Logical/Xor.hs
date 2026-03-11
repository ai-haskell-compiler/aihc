{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Xor where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | Logical exclusive disjunction. Also XOR logic gate.
data Xor l r

-- | Precedence of 4 (matching '==').
instance (Predicate l, Predicate r) => Predicate (Xor l r) where
    type PredicateName d (Xor l r) = PredicateNameBOp " ⊕ " 4 d l r

instance (Refine l a, Refine r a, KnownPredicateName (Xor l r))
  => Refine (Xor l r) a where
    validate p a =
        case l of
          Nothing ->
            case r of
              Just _  -> Nothing
              Nothing -> validateFail p "XOR: l&r succeeded" [      ]
          Just el ->
            case r of
              Nothing -> Nothing
              Just er -> validateFail p "XOR: l&r failed"    [er, el]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
