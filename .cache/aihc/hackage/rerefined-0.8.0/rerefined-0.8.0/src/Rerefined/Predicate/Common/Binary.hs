-- | Common utilities for binary combinator predicates.

module Rerefined.Predicate.Common.Binary where

import Rerefined.Predicate
import TypeLevelShow.Utils
import GHC.TypeNats

-- | Render a binary combinator predicate with an infix operator.
--
-- The operator must include the left and right spaces.
type PredicateNameBOp op prec d l r = ShowParen (d > prec)
    (PredicateName (prec+1) l ++ op ++ PredicateName (prec+1) r)
