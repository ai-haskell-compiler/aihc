{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Handy utilities for defining predicates.

module Rerefined.Predicate.Common
  (
  -- * Re-exports
    module Rerefined.Predicate
  , proxy#
  , TBL.Builder

  -- * Predicate validation
  , validateFail, validateBool
  ) where

import Rerefined.Predicate
import GHC.Exts ( Proxy#, proxy#, IsString(fromString) )
import Data.Text.Builder.Linear qualified as TBL

-- | Shortcut for returning a predicate validation failure.
validateFail
    :: forall p
    .  (Predicate p, KnownPredicateName p)
    => Proxy# p -> TBL.Builder -> [RefineFailure]
    -> Maybe RefineFailure
validateFail _p msg es =
    Just $ RefineFailure (fromString $ predicateName @p) msg es

-- | Shortcut for simply validating a 'Bool'.
validateBool
    :: forall p
    .  (Predicate p, KnownPredicateName p)
    => Proxy# p -> Bool -> TBL.Builder
    -> Maybe RefineFailure
validateBool p b msg = case b of True -> Nothing; False -> validateFail p msg []
