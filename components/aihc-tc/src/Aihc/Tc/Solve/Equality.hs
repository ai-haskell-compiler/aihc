-- | Equality solver.
--
-- Handles unification of meta-variables, decomposition of type
-- constructor equalities, and building coercion evidence.
module Aihc.Tc.Solve.Equality
  ( solveEquality,
    solveGivenEquality,
    EqResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (kindedTyConAt, tcTypeKind, unifyKindsAt)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Congruence (applyGivenSubst, givenEqualities, proveGivenEquality)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, occursOutsideFamilies, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (unless)
import Data.Map.Strict qualified as Map

-- | Preserve a proof from the current signature or pattern scope.
solveGivenEquality :: [Pred] -> Ct -> TcM Bool
solveGivenEquality givens ct = case ctPred ct of
  EqPred left right -> do
    left' <- zonkType left
    right' <- zonkType right
    predicates <- mapM zonkPred givens
    result <- proveGivenEquality predicates left' right'
    case result of
      Just proof -> do
        bindEvidence (ctEvVar ct) (EvCoercion proof)
        pure True
      Nothing -> pure False
  _ -> pure False

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
solveEquality ct = do
  givens <- getGivenPredicates
  proved <- solveGivenEquality givens ct
  if proved then pure EqSolved else solveRewrittenByGivens givens ct

-- | A wanted that still holds a meta variable cannot be proved from the
-- givens as it stands: with the given @texp ~ TExp a@, the wanted
-- @TExp t0 ~ texp@ is only provable once @t0@ is @a@. Rewriting the
-- wanted through the givens that fix a rigid variable gives
-- @TExp t0 ~ TExp a@, whose solution binds @t0@. The evidence for the
-- original wanted then comes from the givens, so the rewritten copy is
-- solved under an evidence variable that nothing reads.
solveRewrittenByGivens :: [Pred] -> Ct -> TcM EqResult
solveRewrittenByGivens givens ct = case ctPred ct of
  EqPred left right | not (null givens) -> do
    left' <- zonkType left
    right' <- zonkType right
    equalities <- concat <$> traverse (givenEqualities [] . (\predicate -> (predicate, EvGiven predicate))) givens
    substitution <- concat <$> mapM orient equalities
    let rewrittenLeft = applyGivenSubst substitution left'
        rewrittenRight = applyGivenSubst substitution right'
    if null substitution || (sameType rewrittenLeft left' && sameType rewrittenRight right')
      then solveWithoutGivens ct
      else do
        scratch <- freshEvVar
        result <- solveWithoutGivens ct {ctPred = EqPred rewrittenLeft rewrittenRight, ctEvVar = scratch}
        case result of
          EqSolved -> do
            proved <- solveGivenEquality givens ct
            pure (if proved then EqSolved else EqError ct)
          EqStuck _ -> pure (EqStuck ct)
          EqError _ -> pure (EqError ct)
  _ -> solveWithoutGivens ct
  where
    -- A rigid variable equal to a family application names that
    -- application: with the given @Sub n m ~ d@ the wanted @Proxy t0 ~
    -- Proxy d@ must bind @t0@ to @d@, not to @Sub n m@, so that the given
    -- @KnownNat d@ still solves the wanted @KnownNat t0@. So the family
    -- application rewrites to the variable, the way every other given
    -- rewrites a family application to its other side.
    orient (a, b, _) = do
      aIsFamily <- isTypeFamilyApplication a
      bIsFamily <- isTypeFamilyApplication b
      pure $ case (a, b) of
        (TcTyVar tyVar, _)
          | not (typeMentionsTyVar tyVar b) -> if bIsFamily then [(b, a)] else [(a, b)]
        (_, TcTyVar tyVar)
          | not (typeMentionsTyVar tyVar a) -> if aIsFamily then [(a, b)] else [(b, a)]
        _ -> []

solveWithoutGivens :: Ct -> TcM EqResult
solveWithoutGivens ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1 >>= reduceTypeFamilies
    t2' <- zonkType t2 >>= reduceTypeFamilies
    solveEq (ct {ctPred = EqPred t1' t2'}) t1' t2'
  _ -> pure (EqStuck ct)

-- | Solve an equality between two zonked and reduced types.
solveEq :: Ct -> TcType -> TcType -> TcM EqResult
solveEq ct rawLeft rawRight = do
  -- The extra arguments of a family application decompose like an
  -- application spine.
  t1 <- unsaturateFamilyApplication rawLeft
  t2 <- unsaturateFamilyApplication rawRight
  leftIsFamily <- isTypeFamilyApplication t1
  rightIsFamily <- isTypeFamilyApplication t2
  if (leftIsFamily || rightIsFamily) && not (isMetaTv t1) && not (isMetaTv t2)
    then
      if sameType t1 t2
        then do
          bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
          pure EqSolved
        else -- A type family application that no equation reduces waits for
        -- its arguments to become known.
          pure (EqStuck ct)
    else solveEqShapes ct t1 t2

isMetaTv :: TcType -> Bool
isMetaTv ty =
  case ty of
    TcMetaTv _ -> True
    TcArrowTy -> True
    _ -> False

solveEqShapes :: Ct -> TcType -> TcType -> TcM EqResult
solveEqShapes ct t1 t2 = case (t1, t2) of
  -- Same meta: trivially solved.
  (TcMetaTv u1, TcMetaTv u2) | u1 == u2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Meta on left: solve by binding.
  (TcMetaTv u, _) -> solveMetaEq ct u t2
  -- Meta on right: solve by binding.
  (_, TcMetaTv u) -> solveMetaEq ct u t1
  -- Same rigid variable, whatever kinds its two occurrences carry.
  (TcTyVar v1, TcTyVar v2) | sameTyVar v1 v2 -> do
    bindEvidence (ctEvVar ct) (EvCoercion (Refl t1))
    pure EqSolved
  -- Two polymorphic types are equal up to the names of their bound
  -- variables.
  (TcForAllTy v1 b1, TcForAllTy v2 b2) ->
    do
      solveDecomposed ct t1 [(b1, applySubst (Map.singleton (tvUnique v2) (TcTyVar v1)) b2)]
  (TcQualTy p1 b1, TcQualTy p2 b2)
    | length p1 == length p2,
      and (zipWith samePred p1 p2) ->
        solveDecomposed ct t1 [(b1, b2)]
  _ -> do
    children <- decomposeNominalEquality t1 t2
    case children of
      Just pairs -> solveDecomposed ct t1 pairs
      Nothing -> pure (EqError ct)

-- | Solve a meta-variable equality by binding.
solveMetaEq :: Ct -> Unique -> TcType -> TcM EqResult
solveMetaEq ct u ty
  | occursIn u ty = do
      -- An occurrence in a family argument can disappear when the family
      -- reduces. The equality waits until then.
      outside <- occursOutsideFamilies
      pure (if outside u ty then EqError ct else EqStuck ct)
  -- A meta-variable stands for a monotype. Binding it to a polytype
  -- would let inference guess an impredicative instantiation.
  | isPolyType ty = pure (EqError ct)
  | otherwise = do
      declaredKind <- readMetaTvKind u
      -- A bare poly-kinded constructor keeps the kind of the meta, as in
      -- 'Aihc.Tc.Unify.unifyMetaTv'.
      solved <- kindedTyConAt declaredKind ty
      solvedKind <- tcTypeKind solved
      unifyKindsAt (ctLoc ct) declaredKind solvedKind
      writeMetaTv u solved
      bindEvidence (ctEvVar ct) (EvCoercion (Refl solved))
      pure EqSolved

solveDecomposed :: Ct -> TcType -> [(TcType, TcType)] -> TcM EqResult
solveDecomposed ct witness pairs = do
  results <- mapM solvePair pairs
  case firstUnsolved results of
    Nothing -> do
      givens <- getGivenPredicates
      proved <- solveGivenEquality givens ct
      unless proved (bindEvidence (ctEvVar ct) (EvCoercion (Refl witness)))
      pure EqSolved
    Just EqStuck {} -> pure (EqStuck ct)
    Just result -> pure result
  where
    solvePair (left, right) = do
      evidence <- freshEvVar
      solveEquality (ct {ctPred = EqPred left right, ctEvVar = evidence})

firstUnsolved :: [EqResult] -> Maybe EqResult
firstUnsolved [] = Nothing
firstUnsolved (EqSolved : rest) = firstUnsolved rest
firstUnsolved (result : _) = Just result

-- | Occurs check: does meta-variable u appear in the type?
occursIn :: Unique -> TcType -> Bool
occursIn u = go
  where
    go (TcMetaTv u') = u == u'
    go TcArrowTy = False
    go (TcTyLit _) = False
    go (TcTyVar _) = False
    go (TcTyCon _ args) = any go args
    go (TcKindedTyCon _ kindArgs) = any go kindArgs
    go (TcFunTy a b) = go a || go b
    go (TcForAllTy _ body) = go body
    go (TcQualTy preds body) = any goPred preds || go body
    go (TcAppTy f a) = go f || go a

    goPred (ClassPred _ args) = any go args
    goPred (EqPred a b) = go a || go b
    goPred (IParamPred _ payload) = go payload
    goPred (IrredPred constraint) = go constraint
    goPred (QuantifiedPred variables antecedents consequent) =
      any (go . tvKind) variables || any goPred antecedents || goPred consequent
