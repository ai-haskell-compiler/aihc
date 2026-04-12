-- | Worklist data type for the constraint solver.
module Aihc.Tc.Solve.Worklist
  ( WorkList (..),
    emptyWorkList,
    addEq,
    addDict,
    addImpl,
    popWork,
    isEmptyWorkList,
  )
where

import Aihc.Tc.Constraint

-- | The solver's work list: constraints not yet processed.
data WorkList = WorkList
  { -- | Equality constraints (highest priority).
    wlEqs :: ![Ct],
    -- | Dictionary (class) constraints.
    wlDicts :: ![Ct],
    -- | Implication constraints (lowest priority).
    wlImpls :: ![Implication]
  }
  deriving (Show)

-- | An empty work list.
emptyWorkList :: WorkList
emptyWorkList = WorkList [] [] []

-- | Add an equality constraint to the work list.
addEq :: Ct -> WorkList -> WorkList
addEq ct wl = wl {wlEqs = ct : wlEqs wl}

-- | Add a dictionary constraint to the work list.
addDict :: Ct -> WorkList -> WorkList
addDict ct wl = wl {wlDicts = ct : wlDicts wl}

-- | Add an implication to the work list.
addImpl :: Implication -> WorkList -> WorkList
addImpl impl wl = wl {wlImpls = impl : wlImpls wl}

-- | Pop the next work item, respecting priority:
-- equalities first, then dictionaries, then implications.
popWork :: WorkList -> Maybe (Either Ct Implication, WorkList)
popWork wl = case wlEqs wl of
  (ct : rest) -> Just (Left ct, wl {wlEqs = rest})
  [] -> case wlDicts wl of
    (ct : rest) -> Just (Left ct, wl {wlDicts = rest})
    [] -> case wlImpls wl of
      (impl : rest) -> Just (Right impl, wl {wlImpls = rest})
      [] -> Nothing

-- | Check if the work list is empty.
isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList wl =
  null (wlEqs wl) && null (wlDicts wl) && null (wlImpls wl)
