{-# LANGUAGE UndecidableInstances #-} -- for easier PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for builder

module Rerefined.Predicate.Logical.Nand where

import Rerefined.Predicate.Common.Binary
import Rerefined.Predicate.Common

-- | NAND logic gate. Also called the Sheffer stroke, or non-conjunction.
data Nand l r

-- | Precedence of 3 (matching 'Data.Bool.&&').
instance (Predicate l, Predicate r) => Predicate (Nand l r) where
    type PredicateName d (Nand l r) = PredicateNameBOp " ⊼ " 3 d l r

instance (Refine l a, Refine r a, KnownPredicateName (Nand l r))
  => Refine (Nand l r) a where
    validate p a =
        case l of
          Just _  -> Nothing
          Nothing ->
            case r of
              Just _  -> Nothing
              Nothing -> validateFail p "NAND: l&r succeeded" []
      where
        l = validate (proxy# @l) a
        r = validate (proxy# @r) a
