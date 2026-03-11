{-# LANGUAGE OverloadedStrings   #-} -- for builder
{-# LANGUAGE AllowAmbiguousTypes #-} -- 'validateVia' is designed for this

module Rerefined.Predicate.Via where

import Rerefined.Predicate.Common
import GHC.Exts ( Proxy# )

{- | Implement 'validate' for predicate @p@ via @pVia@.

This is useful when you need a unique data type for a predicate (e.g. for
instances, clarity), but the predicate it tests already exists. In such cases,
it would be nice to reuse the existing predicate, while printing both predicates
in failures i.e. "predicate X, implemented via predicate Y, failed with Z".

Call using visible type applications:

@
'validate' = 'validateVia' \@pVia
@
-}
validateVia
    :: forall pVia p a
    .  (Refine pVia a, Predicate p, KnownPredicateName p)
    => Proxy# p -> a -> Maybe RefineFailure
validateVia p a =
    case validate (proxy# @pVia) a of
      Nothing -> Nothing
      Just e  -> validateFail p "via" [e]
