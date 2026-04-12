-- | Inert set for the constraint solver.
--
-- The inert set holds canonical constraints that have already been
-- processed and are at rest. New work items are compared against
-- the inert set for interaction rules.
module Aihc.Tc.Solve.InertSet
  ( InertSet (..),
    emptyInertSet,
    addInertEq,
    addInertDict,
  )
where

import Aihc.Tc.Constraint

-- | The inert set: canonical constraints already processed.
data InertSet = InertSet
  { -- | Equality constraints at rest.
    inertEqs :: ![Ct],
    -- | Dictionary constraints at rest.
    inertDicts :: ![Ct]
  }
  deriving (Show)

-- | An empty inert set.
emptyInertSet :: InertSet
emptyInertSet = InertSet [] []

-- | Add a canonical equality to the inert set.
addInertEq :: Ct -> InertSet -> InertSet
addInertEq ct is = is {inertEqs = ct : inertEqs is}

-- | Add a canonical dictionary constraint to the inert set.
addInertDict :: Ct -> InertSet -> InertSet
addInertDict ct is = is {inertDicts = ct : inertDicts is}
