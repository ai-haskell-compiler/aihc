{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Iff where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | Logical biconditional ("if and only if"). Also the XNOR logic gate, or
--   equivalence (loosely).
data Iff l r

-- | Precedence of 4 (matching '==').
instance (Predicate l, Predicate r) => Predicate (Iff l r) where
    type PredicateName d (Iff l r) = PredicateNameBOp " ↔ " 4 d l r

instance (Refine l a, Refine r a, KnownPredicateName (Iff l r))
  => Refine (Iff l r) a where
    validate p a =
        case l of
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er ->
                validateFail p "IFF: left succeeded, but right failed" [er]
          Just el ->
            case r of
              Just _  -> Nothing
              Nothing ->
                validateFail p "IFF: left failed, but right succeeded" [el]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
