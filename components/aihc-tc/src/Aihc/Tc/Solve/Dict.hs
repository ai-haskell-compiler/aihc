-- | Dictionary (class constraint) solver.
--
-- For the MVP, this is a stub. The full implementation will match
-- wanted class constraints against given dictionaries and instance
-- declarations.
module Aihc.Tc.Solve.Dict
  ( solveDict,
    DictResult (..),
  )
where

import Aihc.Tc.Constraint

-- | Result of attempting to solve a dictionary constraint.
data DictResult
  = -- | Solved by given or instance.
    DictSolved
  | -- | Cannot solve yet; leave in inert set.
    DictStuck !Ct
  deriving (Show)

-- | Attempt to solve a dictionary (class) constraint.
--
-- For the MVP, all class constraints are left unsolved (stuck).
-- The full implementation will:
--   1. Match against given dictionaries.
--   2. Match against instance declarations, emitting sub-goals.
--   3. Construct evidence dictionaries.
solveDict :: Ct -> DictResult
solveDict = DictStuck
