-- | Top-level constraint solver.
--
-- The solver uses the worklist/inert-set architecture from OutsideIn(X):
--
-- @
-- while worklist is non-empty:
--   pop constraint from worklist
--   canonicalize it
--   attempt to solve
--   either:
--     - solve it (fill evidence)
--     - add to inert set
--     - emit new work items
-- @
module Aihc.Tc.Solve
  ( solveConstraints,
    SolveResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Canonicalize
import Aihc.Tc.Solve.Dict (DictResult (..), solveDict)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.InertSet (InertSet, addInertDict, emptyInertSet)
import Aihc.Tc.Solve.Worklist
import Aihc.Tc.Types (Pred (..))

-- | Result of solving constraints.
data SolveResult = SolveResult
  { -- | Residual unsolved constraints.
    srResidual :: ![Ct],
    -- | Final inert set.
    srInerts :: !InertSet
  }
  deriving (Show)

-- | Solve a list of wanted constraints.
solveConstraints :: [Ct] -> TcM SolveResult
solveConstraints wanteds = do
  let wl = foldr addWork emptyWorkList wanteds
  solveLoop wl emptyInertSet

-- | Add a constraint to the appropriate bucket in the worklist.
addWork :: Ct -> WorkList -> WorkList
addWork ct = case ctPred ct of
  EqPred {} -> addEq ct
  ClassPred {} -> addDict ct

-- | Main solver loop.
solveLoop :: WorkList -> InertSet -> TcM SolveResult
solveLoop wl inerts = case popWork wl of
  Nothing ->
    -- Done: all constraints processed.
    pure SolveResult {srResidual = [], srInerts = inerts}
  Just (Left ct, wl') ->
    -- Process a flat constraint.
    processConstraint ct wl' inerts
  Just (Right _impl, wl') ->
    -- Implications: for MVP, skip and continue.
    -- Full implementation will enter nested solver scope.
    solveLoop wl' inerts

-- | Process a single constraint from the worklist.
processConstraint :: Ct -> WorkList -> InertSet -> TcM SolveResult
processConstraint ct wl inerts = case canonicalize ct of
  CanonSolved ->
    -- Trivially solved (e.g. reflexive equality).
    solveLoop wl inerts
  CanonEqs subCts -> do
    -- Try to solve each sub-constraint.
    wl' <- foldM processEq wl subCts
    solveLoop wl' inerts
  CanonDict dictCt -> do
    -- Try to solve dictionary constraint.
    case solveDict dictCt of
      DictSolved -> solveLoop wl inerts
      DictStuck stuckCt ->
        -- Leave in inert set for now.
        solveLoop wl (addInertDict stuckCt inerts)

-- | Process an equality constraint.
processEq :: WorkList -> Ct -> TcM WorkList
processEq wl ct = do
  result <- solveEquality ct
  case result of
    EqSolved -> pure wl
    EqStuck stuckCt -> do
      -- Cannot solve yet, re-add to worklist.
      pure (addEq stuckCt wl)
    EqError errCt -> do
      -- Report the error.
      case ctPred errCt of
        EqPred t1 t2 ->
          emitError (ctLoc errCt) (UnificationError t1 t2 (ctOrigin errCt))
        p ->
          emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
      pure wl

-- | Strict left fold in a monad.
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = pure acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs
