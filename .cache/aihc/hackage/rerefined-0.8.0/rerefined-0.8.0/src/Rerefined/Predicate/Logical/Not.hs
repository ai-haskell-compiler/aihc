{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Not where

import Rerefined.Predicate.Common
import TypeLevelShow.Utils

-- | Logical negation. Also NOT logic gate, or logical complement.
data Not p

-- | Precedence of 9 (one below function application).
instance Predicate p => Predicate (Not p) where
    type PredicateName d (Not p) = ShowParen (d > 9)
        ("¬ " ++ PredicateName 10 p)

instance (Refine p a, KnownPredicateName (Not p))
  => Refine (Not p) a where
    validate p a =
        case validate (proxy# @p) a of
          Just _  -> Nothing
          Nothing -> validateFail p "NOT: predicate succeeded" []
