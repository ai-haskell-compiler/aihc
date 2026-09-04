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
    solveWithImpls,
    SolveResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Canonicalize
import Aihc.Tc.Solve.Dict (DictResult (..), reportUnsolvedDict, solveDict, solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.InertSet (InertSet, addInertDict, emptyInertSet)
import Aihc.Tc.Solve.Worklist
import Aihc.Tc.Types (Pred (..), TcType (..), TyVarId, Unique)
import Aihc.Tc.Zonk (zonkPred, zonkType)

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
solveConstraints wanteds = solveWithImpls wanteds []

-- | Solve wanted constraints together with implication constraints.
solveWithImpls :: [Ct] -> [Implication] -> TcM SolveResult
solveWithImpls wanteds impls = do
  let wl0 = foldr addWork emptyWorkList wanteds
      wl = foldr addImpl wl0 impls
  solveLoop wl emptyInertSet

-- | Add a constraint to the appropriate bucket in the worklist.
addWork :: Ct -> WorkList -> WorkList
addWork ct = case ctPred ct of
  EqPred {} -> addEq ct
  ClassPred {} -> addDict ct
  QuantifiedPred {} -> addDict ct
  IParamPred {} -> addDict ct

-- | Main solver loop.
solveLoop :: WorkList -> InertSet -> TcM SolveResult
solveLoop wl inerts = case popWork wl of
  Nothing ->
    -- Done: all constraints processed.
    pure SolveResult {srResidual = [], srInerts = inerts}
  Just (Left ct, wl') ->
    -- Process a flat constraint.
    processConstraint ct wl' inerts
  Just (Right impl, wl') -> do
    -- Solve the implication by using its given constraints to satisfy wanteds.
    -- A wanted that is stuck on a meta variable of the enclosing scope waits
    -- in the inert set for the enclosing solve.
    deferred <- solveImplication impl
    solveLoop wl' (foldr addInertDict inerts deferred)

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
    dictResult <- solveDict dictCt
    case dictResult of
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
          emitError (ctLoc errCt) . UnificationError t1 t2 (ctOrigin errCt) =<< zonkCtEqProvenance errCt
        p ->
          emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
      pure wl

-- | Solve an implication constraint.
--
-- The implication's given constraints (from GADT pattern matches) are
-- canonicalized into atomic equalities, which are then used as rewrite
-- rules to solve the implication's wanted constraints.
-- | Solve the wanteds of an implication. The result holds the dictionary
-- wanteds that are stuck on a meta variable of the enclosing scope and do
-- not mention a skolem of the implication.
solveImplication :: Implication -> TcM [Ct]
solveImplication impl = withTcLevel $ do
  let rawGivens = implGivenCts impl
      wanteds = implWantedCts impl
      givenPredicates = map ctPred rawGivens
  -- Canonicalize the given equalities by structural decomposition.
  givenEqs <- concat <$> mapM canonicalizeGiven rawGivens
  -- Equalities refine the types mentioned by dictionary wanteds, so preserve
  -- the main worklist's equality-before-dictionary ordering inside branches.
  let (equalityWanteds, dictionaryWanteds) = partitionWanteds wanteds
  mapM_ (solveWantedWithGivens [] givenPredicates givenEqs) equalityWanteds
  concat <$> mapM (solveWantedWithGivens (implSkols impl) givenPredicates givenEqs) dictionaryWanteds

partitionWanteds :: [Ct] -> ([Ct], [Ct])
partitionWanteds = foldr partitionOne ([], [])
  where
    partitionOne ct (equalities, dictionaries) =
      case ctPred ct of
        EqPred {} -> (ct : equalities, dictionaries)
        ClassPred {} -> (equalities, ct : dictionaries)
        QuantifiedPred {} -> (equalities, ct : dictionaries)
        IParamPred {} -> (equalities, ct : dictionaries)

-- | Decompose a given constraint into atomic equalities.
-- For example, @GADT a ~ GADT Bool@ decomposes into @[(a, Bool)]@.
canonicalizeGiven :: Ct -> TcM [(TcType, TcType)]
canonicalizeGiven ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1
    t2' <- zonkType t2
    pure (decomposeEq t1' t2')
  _ -> pure []
  where
    decomposeEq t1 t2
      | t1 == t2 = []
    decomposeEq (TcTyCon tc1 args1) (TcTyCon tc2 args2)
      | tc1 == tc2,
        length args1 == length args2 =
          concatMap (uncurry decomposeEq) (zip args1 args2)
    decomposeEq (TcFunTy a1 b1) (TcFunTy a2 b2) =
      decomposeEq a1 a2 ++ decomposeEq b1 b2
    decomposeEq t1 t2 = [(t1, t2)]

-- | Apply a list of given equalities as a substitution to a type.
-- Each given @(lhs, rhs)@ rewrites occurrences of @lhs@ with @rhs@.
applyGivenSubst :: [(TcType, TcType)] -> TcType -> TcType
applyGivenSubst givens ty = foldr applyOne ty givens
  where
    applyOne (lhs, rhs) t
      | t == lhs = rhs
      | otherwise =
          case t of
            TcTyCon tc args -> TcTyCon tc (map (applyOne (lhs, rhs)) args)
            TcFunTy a b -> TcFunTy (applyOne (lhs, rhs) a) (applyOne (lhs, rhs) b)
            TcAppTy f a -> TcAppTy (applyOne (lhs, rhs) f) (applyOne (lhs, rhs) a)
            TcForAllTy tv body -> TcForAllTy tv (applyOne (lhs, rhs) body)
            _ -> t

-- | Attempt to solve a wanted constraint using given equalities.
-- Rewrites both sides of the wanted using the given substitution and
-- then tries to solve the resulting equality.
solveWantedWithGivens :: [TyVarId] -> [Pred] -> [(TcType, TcType)] -> Ct -> TcM [Ct]
solveWantedWithGivens skolems givenPredicates givenEqualities ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1
    t2' <- zonkType t2
    let t1'' = applyGivenSubst givenEqualities t1'
        t2'' = applyGivenSubst givenEqualities t2'
    result <- solveEquality (ct {ctPred = EqPred t1'' t2''})
    case result of
      EqSolved -> pure []
      EqStuck _ -> pure []
      EqError errCt -> do
        case ctPred errCt of
          EqPred et1 et2 ->
            emitError (ctLoc errCt) . UnificationError et1 et2 (ctOrigin errCt) =<< zonkCtEqProvenance errCt
          p ->
            emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
        pure []
  ClassPred className arguments -> do
    arguments' <- mapM zonkType arguments
    let rewritten = ClassPred className (map (applyGivenSubst givenEqualities) arguments')
        rewrittenGivens = map (rewritePred givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritten})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck
  quantified@QuantifiedPred {} -> do
    let rewrittenGivens = map (rewritePred givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritePred givenEqualities quantified})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck
  IParamPred name payload -> do
    payload' <- zonkType payload
    let rewritten = IParamPred name (applyGivenSubst givenEqualities payload')
        rewrittenGivens = map (rewritePred givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritten})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems stuck

-- | A stuck dictionary wanted that mentions a meta variable and no skolem
-- of the implication can still be solved by the enclosing scope, so it is
-- deferred. Every other stuck wanted is an error.
deferOrReport :: [TyVarId] -> Ct -> TcM [Ct]
deferOrReport skolems stuck = do
  predicate <- zonkPred (ctPred stuck)
  let deferrable = not (null (predMetaVars predicate)) && not (any (`elem` skolems) (predTyVars predicate))
  if deferrable
    then pure [stuck {ctPred = predicate}]
    else do
      reportUnsolvedDict stuck
      pure []

predMetaVars :: Pred -> [Unique]
predMetaVars predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeMetaVars arguments
    EqPred left right -> typeMetaVars left <> typeMetaVars right
    IParamPred _ payload -> typeMetaVars payload
    QuantifiedPred _ antecedents consequent -> concatMap predMetaVars antecedents <> predMetaVars consequent

typeMetaVars :: TcType -> [Unique]
typeMetaVars ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcTyVar _ -> []
    TcTyCon _ arguments -> concatMap typeMetaVars arguments
    TcFunTy argument result -> typeMetaVars argument <> typeMetaVars result
    TcForAllTy _ body -> typeMetaVars body
    TcQualTy predicates body -> concatMap predMetaVars predicates <> typeMetaVars body
    TcAppTy function argument -> typeMetaVars function <> typeMetaVars argument

predTyVars :: Pred -> [TyVarId]
predTyVars predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeTyVars arguments
    EqPred left right -> typeTyVars left <> typeTyVars right
    IParamPred _ payload -> typeTyVars payload
    QuantifiedPred variables antecedents consequent ->
      filter (`notElem` variables) (concatMap predTyVars antecedents <> predTyVars consequent)

typeTyVars :: TcType -> [TyVarId]
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> [tyVar]
    TcMetaTv _ -> []
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

rewritePred :: [(TcType, TcType)] -> Pred -> Pred
rewritePred equalities predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applyGivenSubst equalities) arguments)
    EqPred left right -> EqPred (applyGivenSubst equalities left) (applyGivenSubst equalities right)
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables (map (rewritePred equalities) antecedents) (rewritePred equalities consequent)
    IParamPred name payload -> IParamPred name (applyGivenSubst equalities payload)

zonkCtEqProvenance :: Ct -> TcM (Maybe EqProvenance)
zonkCtEqProvenance ct =
  traverse zonkEqProvenance (ctEqProvenance ct)

zonkEqProvenance :: EqProvenance -> TcM EqProvenance
zonkEqProvenance provenance = do
  actual <- zonkTypeTrace (eqActualTrace provenance)
  expected <- zonkTypeTrace (eqExpectedTrace provenance)
  pure
    provenance
      { eqActualTrace = actual,
        eqExpectedTrace = expected
      }

zonkTypeTrace :: TypeTrace -> TcM TypeTrace
zonkTypeTrace trace' = do
  ty <- zonkType (typeTraceType trace')
  pure (trace' {typeTraceType = ty})

-- | Strict left fold in a monad.
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = pure acc
foldM f acc (x : xs) = do
  acc' <- f acc x
  foldM f acc' xs
