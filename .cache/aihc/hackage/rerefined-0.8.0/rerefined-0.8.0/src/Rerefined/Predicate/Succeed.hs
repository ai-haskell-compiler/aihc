{-# LANGUAGE OverloadedStrings #-}

module Rerefined.Predicate.Succeed where

import Rerefined.Predicate.Common

-- | The unit predicate. Always succeeds.
data Succeed
instance Predicate Succeed where type PredicateName d Succeed = "⊤"
instance Refine Succeed a where validate _ _ = Nothing
