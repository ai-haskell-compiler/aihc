-- | Classify constraints without a change to their evidence variables.
module Aihc.Tc.Solve.Canonicalize
  ( classifyConstraint,
    CanonResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Types

-- | Result of canonicalization.
data CanonResult
  = -- | Produce canonical equality constraints.
    CanonEqs ![Ct]
  | -- | Produce a canonical dictionary constraint.
    CanonDict !Ct
  | -- | Constraint is already solved (trivially true).
    CanonSolved
  deriving (Show)

-- | Classify a constraint as equality or dictionary.
classifyConstraint :: Ct -> CanonResult
classifyConstraint ct = case ctPred ct of
  EqPred {} -> CanonEqs [ct]
  ClassPred {} -> CanonDict ct
  QuantifiedPred {} -> CanonDict ct
  IParamPred {} -> CanonDict ct
