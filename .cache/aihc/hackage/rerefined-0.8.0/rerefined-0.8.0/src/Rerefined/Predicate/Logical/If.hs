{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.If where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | Logical implication. "If l then r".
data If l r

-- | Precedence of 4 (matching '==').
instance (Predicate l, Predicate r) => Predicate (If l r) where
    -- TODO double arrow? idk
    type PredicateName d (If l r) = PredicateNameBOp " → " 4 d l r

instance (Refine l a, Refine r a, KnownPredicateName (If l r))
  => Refine (If l r) a where
    validate p a =
        case l of
          Just _  -> Nothing
          Nothing ->
            case r of
              Nothing -> Nothing
              Just er ->
                validateFail p "IF: left succeeded, but right failed" [er]
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
