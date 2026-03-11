-- | Predicate re-exports, for when you're heavily using refinement types.

module Rerefined.Predicates
  (
  -- * Base
    Succeed
  , Fail
  , validateVia

  -- * Logical
  , And, Iff, If, Nand, Nor, Not, Or, Xor

  -- * Relational
  , RelOp(..)
  , CompareValue
  , Sign(..)
  , CompareLength
  ) where

import Rerefined.Predicate.Succeed
import Rerefined.Predicate.Fail
import Rerefined.Predicate.Via
import Rerefined.Predicate.Logical
import Rerefined.Predicate.Relational
