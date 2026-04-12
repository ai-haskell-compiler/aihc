-- | Equality solver.
--
-- Handles unification of meta-variables, decomposition of type
-- constructor equalities, and building coercion evidence.
module Aihc.Tc.Solve.Equality
  ( solveEquality,
    EqResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Evidence
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)

-- | Result of attempting to solve an equality constraint.
data EqResult
  = -- | Solved: evidence bound.
    EqSolved
  | -- | Stuck: cannot solve yet (e.g. two different skolems).
    EqStuck !Ct
  | -- | Error: types are incompatible.
    EqError !Ct
  deriving (Show)

-- | Attempt to solve an equality constraint.
solveEquality :: Ct -> TcM EqResult
solveEquality ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1
    t2' <- zonkType t2
    solveEq ct t1' t2'
  _ -> pure (EqStuck ct)

-- | Solve an equality between two zonked types.
solveEq :: Ct -> TcType -> TcType -> TcM EqResult
solveEq ct t1 t2 = case (t1, t2) of
  -- Same meta: trivially solved.
  (TcMetaTv u1, TcMetaTv u2) | u1 == u2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Meta on left: solve by binding.
  (TcMetaTv u, _) -> solveMetaEq ct u t2
  -- Meta on right: solve by binding.
  (_, TcMetaTv u) -> solveMetaEq ct u t1
  -- Same rigid variable.
  (TcTyVar v1, TcTyVar v2) | v1 == v2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Same type constructor: decompose.
  (TcTyCon tc1 args1, TcTyCon tc2 args2)
    | tc1 == tc2,
      length args1 == length args2 -> do
        bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
        pure EqSolved
  -- Same function type shape: already decomposed by canonicalization.
  (TcFunTy _ _, TcFunTy _ _) -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Incompatible types.
  _ -> pure (EqError ct)

-- | Solve a meta-variable equality by binding.
solveMetaEq :: Ct -> Unique -> TcType -> TcM EqResult
solveMetaEq ct u ty
  | occursIn u ty = pure (EqError ct)
  | otherwise = do
      writeMetaTv u ty
      bindEvidence (ctEvVar ct) (EvCoercion (Refl ty))
      pure EqSolved

-- | Occurs check: does meta-variable u appear in the type?
occursIn :: Unique -> TcType -> Bool
occursIn u = go
  where
    go (TcMetaTv u') = u == u'
    go (TcTyVar _) = False
    go (TcTyCon _ args) = any go args
    go (TcFunTy a b) = go a || go b
    go (TcForAllTy _ body) = go body
    go (TcQualTy preds body) = any goPred preds || go body
    go (TcAppTy f a) = go f || go a

    goPred (ClassPred _ args) = any go args
    goPred (EqPred a b) = go a || go b
