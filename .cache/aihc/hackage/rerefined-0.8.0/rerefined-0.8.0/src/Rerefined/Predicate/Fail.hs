{-# LANGUAGE OverloadedStrings #-}

module Rerefined.Predicate.Fail where

import Rerefined.Predicate.Common

-- | Always fails.
data Fail
instance Predicate Fail where type PredicateName d Fail = "⊥"
instance Refine Fail a where validate p _ = validateFail p "fail" []
