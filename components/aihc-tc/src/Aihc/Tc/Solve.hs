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
import Aihc.Tc.Solve.Congruence (applyGivenSubst)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Dict (DictResult (..), reportUnsolvedDict, solveDict, solveDictWithGivens, splitTypeable)
import Aihc.Tc.Solve.Equality (EqResult (..), givenRewrites, solveEquality)
import Aihc.Tc.Solve.Family (reducePredFamilies)
import Aihc.Tc.Solve.FunDep (improveFunDeps, superClassEqualities)
import Aihc.Tc.Solve.InertSet (InertSet (..), addInertDict, addInertEq, emptyInertSet)
import Aihc.Tc.Solve.Injective (improveInjectivity)
import Aihc.Tc.Solve.Interact (improveSharedFamilyApplications)
import Aihc.Tc.Solve.Worklist
import Aihc.Tc.Types (Pred (..), TcKinds, TcType (..), TyVarId, Unique, sameType, typeMentionsMeta)
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)

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
  IrredPred {} -> addDict ct

-- | Main solver loop.
solveLoop :: WorkList -> InertSet -> TcM SolveResult
solveLoop wl inerts = case popWork wl of
  Nothing -> do
    -- An instance context can solve a meta variable in an earlier dictionary.
    (changed, unchanged) <- partitionProgress (inertDicts inerts)
    if null changed
      then improveDictionaries inerts
      else solveLoop (foldr addDict emptyWorkList changed) inerts {inertDicts = unchanged}
  Just (Left ct, wl') ->
    -- Process a flat constraint.
    processConstraint ct wl' inerts
  Just (Right impl, wl') -> do
    -- Solve the implication by using its given constraints to satisfy wanteds.
    -- A wanted that is stuck on a meta variable of the enclosing scope waits
    -- in the inert set for the enclosing solve. It keeps the givens of the
    -- implication for the time when the variable gets its solution.
    deferred <- solveImplication impl
    solveLoop wl' (foldr (addInertDict . withBranchGivens impl) inerts deferred)

improveDictionaries :: InertSet -> TcM SolveResult
improveDictionaries inerts = do
  -- A functional dependency can solve a meta variable of a dictionary
  -- that no instance or given matched on its own. Improvement only
  -- solves meta variables, so a pass that solves none ends the loop.
  givens <- getGivenPredicates
  improvedFunDeps <- improveFunDeps givens (inertDicts inerts)
  improved <-
    if improvedFunDeps
      then pure True
      else improveSuperClassEqualities (inertDicts inerts)
  if improved
    then solveLoop (foldr addDict emptyWorkList (inertDicts inerts)) inerts {inertDicts = []}
    else drained inerts

-- | A wanted class constraint entails the equality superclasses of its
-- class, so each of them must hold too. Unifying the two sides of one can
-- solve a meta variable that no instance and no functional dependency
-- determines, as GHC does with the superclasses of a wanted. The class
-- @is ~ '[i] => HasSingleIndex is i@ is an example: a wanted
-- @HasSingleIndex '[x] t0@ entails @'[x] ~ '[t0]@, which gives @t0 := x@.
-- The equalities are for improvement only and carry no evidence. A side
-- that does not unify leaves every meta variable as it was.
improveSuperClassEqualities :: [Ct] -> TcM Bool
improveSuperClassEqualities dictionaries = do
  entailed <- superClassEqualities dictionaries
  or <$> mapM improveOne entailed
  where
    improveOne predicate =
      case predicate of
        EqPred left right -> do
          left' <- zonkType left
          right' <- zonkType right
          if sameType left' right' || not (typeMentionsMeta left' || typeMentionsMeta right')
            then pure False
            else do
              saved <- lift get
              result <- unifyTypes left' right'
              case result of
                Right () -> do
                  left'' <- zonkType left'
                  right'' <- zonkType right'
                  pure (left'' /= left' || right'' /= right')
                Left _ -> do
                  lift (put saved)
                  pure False
        _ -> pure False

-- | No work is left. An equality that waits on a type family application
-- gets another attempt when a solved meta variable changed it. Failing
-- that, the injectivity annotation of a family it mentions may still
-- determine an argument, which changes it. Otherwise it is a residual that
-- the enclosing scope solves or reports.
drained :: InertSet -> TcM SolveResult
drained inerts
  | null (inertEqs inerts) = pure SolveResult {srResidual = [], srInerts = inerts}
  | otherwise = do
      (progressed, stuck) <- partitionProgress (inertEqs inerts)
      if null progressed
        then do
          givens <- getGivenPredicates
          entailed <- superClassEqualities (inertDicts inerts)
          improved <- improveStuckEqualities givens entailed (map ctPred stuck)
          if improved
            then solveLoop (foldr addEq emptyWorkList stuck) inerts {inertEqs = []}
            else pure SolveResult {srResidual = stuck, srInerts = inerts {inertEqs = []}}
        else solveLoop (foldr addEq emptyWorkList progressed) inerts {inertEqs = stuck}

-- | Improve the stuck equalities from the injectivity annotations of the
-- families they mention, and from the family applications they share.
-- The equality superclasses of the stuck dictionary wanteds share family
-- applications with them too.
--
-- The givens rewrite a copy of each equality first, so that a family
-- application that a given equates with another type is that type. With
-- the given @Key p ~ Wrap p@, the wanted @Key t0 ~ Key p@ becomes @Key t0 ~
-- Wrap p@, which shares @Key t0@ with the entailed @Key t0 ~ Wrap t0@.
improveStuckEqualities :: [Pred] -> [Pred] -> [Pred] -> TcM Bool
improveStuckEqualities givens entailed stuck = do
  improved <- improveInjectivity givens stuck
  if improved
    then pure True
    else do
      rewrites <- givenRewrites givens
      rewritten <- mapM (fmap (rewriteEquality rewrites) . zonkPred) (stuck <> entailed)
      improveSharedFamilyApplications rewritten
  where
    rewriteEquality rewrites predicate =
      case predicate of
        EqPred left right -> EqPred (applyGivenSubst rewrites left) (applyGivenSubst rewrites right)
        _ -> predicate

-- | Separate constraints that changed after a meta variable received a
-- solution. A change of a given that the constraint keeps from its scope
-- is a change too: the given can now prove the constraint.
partitionProgress :: [Ct] -> TcM ([Ct], [Ct])
partitionProgress stuckCts = do
  results <- mapM progress stuckCts
  pure ([ct | (ct, True) <- results], [ct | (ct, False) <- results])
  where
    progress ct = do
      predicate <- zonkPred (ctPred ct) >>= reducePredFamilies
      givens <- mapM progressGiven (ctBranchGivens ct)
      let changed = predicate /= ctPred ct || map ctPred givens /= map ctPred (ctBranchGivens ct)
      pure (ct {ctPred = predicate, ctBranchGivens = givens}, changed)
    progressGiven given = do
      predicate <- zonkPred (ctPred given)
      pure given {ctPred = predicate}

-- | Process a single constraint from the worklist.
processConstraint :: Ct -> WorkList -> InertSet -> TcM SolveResult
processConstraint ct wl inerts = do
  predicate <- zonkPred (ctPred ct) >>= reducePredFamilies
  case predicate of
    -- Keep the parent evidence variable for the complete equality.
    EqPred {} -> do
      (wl', inerts') <- processEq (wl, inerts) (ct {ctPred = predicate})
      solveLoop wl' inerts'
    _ -> processCanonical wl inerts (classifyConstraint (ct {ctPred = predicate}))

processCanonical :: WorkList -> InertSet -> CanonResult -> TcM SolveResult
processCanonical wl inerts result = case result of
  CanonSolved ->
    -- Trivially solved (e.g. reflexive equality).
    solveLoop wl inerts
  CanonEqs subCts -> do
    -- Try to solve each sub-constraint.
    (wl', inerts') <- foldM processEq (wl, inerts) subCts
    solveLoop wl' inerts'
  CanonDict dictCt -> do
    -- Try to solve dictionary constraint.
    dictResult <- solveDict dictCt
    case dictResult of
      DictSolved -> solveLoop wl inerts
      DictStuck stuckCt ->
        -- Leave in inert set for now.
        solveLoop wl (addInertDict stuckCt inerts)

-- | Process an equality constraint.
processEq :: (WorkList, InertSet) -> Ct -> TcM (WorkList, InertSet)
processEq (wl, inerts) ct = do
  result <- solveEquality ct
  case result of
    EqSolved -> pure (wl, inerts)
    EqStuck stuckCt ->
      -- Cannot solve yet. The loop retries it when the worklist is empty.
      pure (wl, addInertEq stuckCt inerts)
    EqError errCt -> do
      -- A wanted that left a branch keeps its givens. When one of them
      -- still waits for a meta variable, the wanted waits too (see
      -- 'solveWantedWithGivens').
      givens <- mapM (zonkPred . ctPred) (ctBranchGivens ct)
      if not (all (null . predMetaVars) givens)
        then pure (wl, addInertDict ct inerts)
        else do
          _ <- reportEqError errCt
          pure (wl, inerts)

-- | Solve an implication constraint.
--
-- The implication's given constraints (from GADT pattern matches) are
-- canonicalized into atomic equalities, which are then used as rewrite
-- rules to solve the implication's wanted constraints.
-- | Solve the wanteds of an implication. The result holds the dictionary
-- wanteds that are stuck on a meta variable of the enclosing scope and do
-- not mention a skolem of the implication.
solveImplication :: Implication -> TcM [Ct]
solveImplication impl = do
  outerPredicates <- getGivenPredicates
  let rawGivens = implGivenCts impl
      wanteds = implWantedCts impl
      givenPredicates = outerPredicates <> map ctPred rawGivens
  -- Canonicalize the given equalities by structural decomposition.
  givenEqs <- concat <$> mapM canonicalizeGiven rawGivens
  -- Equalities refine the types mentioned by dictionary wanteds, so preserve
  -- the main worklist's equality-before-dictionary ordering inside branches.
  let (equalityWanteds, dictionaryWanteds) = partitionWanteds wanteds
      skolems = implSkols impl
  improveImplicationWanteds givenPredicates dictionaryWanteds
  entailed <- superClassEqualities dictionaryWanteds
  deferredEqualities <- solveImplicationEqualities skolems givenPredicates entailed givenEqs equalityWanteds
  deferredDictionaries <- concat <$> mapM (solveWantedWithGivens skolems givenPredicates givenEqs) dictionaryWanteds
  pure (deferredEqualities <> deferredDictionaries)

-- | A wanted that leaves an implication keeps the givens of it.
withBranchGivens :: Implication -> Ct -> Ct
withBranchGivens impl ct = ct {ctBranchGivens = implGivenCts impl <> ctBranchGivens ct}

-- | Improve the dictionary wanteds of a branch from the givens of the
-- branch until improvement solves no further meta variable.
--
-- The equality superclasses of a wanted improve it in a branch too, as
-- at the top level (see 'improveSuperClassEqualities'). In @lensC f (x :&
-- xs) = fmap (x :&) (lensC f xs)@ of @class (i ~ Idx r rs) => Elem r r'
-- rs rs' i@, the wanted @Elem r r' rs1 rs' t0@ entails @t0 ~ Idx r rs1@.
-- The given of the instance then solves it.
improveImplicationWanteds :: [Pred] -> [Ct] -> TcM ()
improveImplicationWanteds givenPredicates constraints = do
  improvedFunDeps <- improveFunDeps givenPredicates constraints
  improved <-
    if improvedFunDeps
      then pure True
      else improveSuperClassEqualities constraints
  when improved (improveImplicationWanteds givenPredicates constraints)

-- | Retry equalities after argument constraints solve meta variables.
-- The result holds the equality wanteds that the enclosing scope must solve.
solveImplicationEqualities :: [TyVarId] -> [Pred] -> [Pred] -> [(TcType, TcType)] -> [Ct] -> TcM [Ct]
solveImplicationEqualities skolems predicates entailed equalities constraints = do
  results <- withGivenPredicates predicates (mapM solveEquality constraints)
  let remaining = [constraint | (constraint, result) <- zip constraints results, case result of EqSolved -> False; _ -> True]
  if length remaining < length constraints
    then solveImplicationEqualities skolems predicates entailed equalities remaining
    else do
      -- The givens of the branch can name a family application that an
      -- injectivity annotation then reads back into a family argument.
      improved <- improveStuckEqualities predicates entailed (map ctPred remaining)
      if improved
        then solveImplicationEqualities skolems predicates entailed equalities remaining
        else concat <$> mapM (solveWantedWithGivens skolems predicates equalities) remaining

partitionWanteds :: [Ct] -> ([Ct], [Ct])
partitionWanteds = foldr partitionOne ([], [])
  where
    partitionOne ct (equalities, dictionaries) =
      case ctPred ct of
        EqPred {} -> (ct : equalities, dictionaries)
        ClassPred {} -> (equalities, ct : dictionaries)
        QuantifiedPred {} -> (equalities, ct : dictionaries)
        IParamPred {} -> (equalities, ct : dictionaries)
        IrredPred {} -> (equalities, ct : dictionaries)

-- | Decompose a given constraint into atomic equalities.
-- For example, @GADT a ~ GADT Bool@ decomposes into @[(a, Bool)]@.
canonicalizeGiven :: Ct -> TcM [(TcType, TcType)]
canonicalizeGiven ct = case ctPred ct of
  EqPred t1 t2 -> do
    t1' <- zonkType t1
    t2' <- zonkType t2
    decomposeEq t1' t2'
  _ -> pure []
  where
    decomposeEq t1 t2
      | t1 == t2 = pure []
      | otherwise = do
          children <- decomposeNominalEquality t1 t2
          case children of
            Just pairs -> concat <$> mapM (uncurry decomposeEq) pairs
            Nothing -> pure [(t1, t2)]

-- | Attempt to solve a wanted constraint using given equalities.
-- Equality evidence must prove the original endpoints.
solveWantedWithGivens :: [TyVarId] -> [Pred] -> [(TcType, TcType)] -> Ct -> TcM [Ct]
solveWantedWithGivens skolems givenPredicates givenEqualities ct = case ctPred ct of
  EqPred {} -> do
    result <- withGivenPredicates givenPredicates (solveEquality ct)
    case result of
      EqSolved -> pure []
      EqStuck stuck -> deferOrReport skolems givenPredicates stuck
      EqError errCt -> do
        -- A given of the branch can still wait for a meta variable of the
        -- enclosing scope: a match on @ns@ in @(\ns -> case ns of S s ->
        -- k s) . unK@ gives @t0 ~ (x1 ': xs1)@, and only the later use
        -- of @.@ gives @t0@ the value @x ': xs@. The wanted @xs ~ xs1@
        -- then follows from the given. The wanted leaves the branch with
        -- its givens, and the enclosing scope solves it again when the
        -- givens change.
        givens <- mapM zonkPred givenPredicates
        if not (all (null . predMetaVars) givens)
          then pure [ct]
          else reportEqError errCt
  ClassPred {} -> do
    result <- solveDictWithGivens givenPredicates ct
    case result of
      DictSolved -> pure []
      DictStuck stuck -> do
        -- A Typeable wanted that mentions a skolem of the branch and a
        -- meta variable of the enclosing scope cannot leave the branch.
        -- Its parts can: the branch solves the parts of the skolem, and
        -- the parts of the meta variable wait for the enclosing scope.
        predicate <- zonkPred (ctPred stuck)
        let mixed =
              not (null (predMetaVars predicate))
                && any (`elem` skolems) (predTyVars predicate)
        parts <- if mixed then splitTypeable stuck {ctPred = predicate} else pure Nothing
        case parts of
          Just wanteds -> concat <$> mapM (solveWantedWithGivens skolems givenPredicates givenEqualities) wanteds
          Nothing -> deferOrReport skolems givenPredicates stuck
  quantified@QuantifiedPred {} -> do
    kinds <- getKinds
    let rewrittenGivens = map (rewritePred kinds givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritePred kinds givenEqualities quantified})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems givenPredicates stuck
  IrredPred {} -> do
    -- Keep the original type for the dictionary and its equality proof.
    result <- solveDictWithGivens givenPredicates ct
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems givenPredicates stuck
  IParamPred name payload -> do
    kinds <- getKinds
    payload' <- zonkType payload
    let rewritten = IParamPred name (applyGivenSubst givenEqualities payload')
        rewrittenGivens = map (rewritePred kinds givenEqualities) givenPredicates
    result <- solveDictWithGivens rewrittenGivens (ct {ctPred = rewritten})
    case result of
      DictSolved -> pure []
      DictStuck stuck -> deferOrReport skolems givenPredicates stuck

-- | Report an equality wanted that does not hold.
reportEqError :: Ct -> TcM [Ct]
reportEqError errCt = do
  case ctPred errCt of
    EqPred et1 et2 ->
      emitError (ctLoc errCt) . UnificationError et1 et2 (ctOrigin errCt) =<< zonkCtEqProvenance errCt
    p ->
      emitError (ctLoc errCt) (UnsolvedWanted p (ctOrigin errCt))
  pure []

-- | A stuck dictionary wanted that mentions no skolem of the implication
-- can still be solved by the enclosing scope when it or one of its givens
-- mentions a meta variable, so it is deferred. Every other stuck wanted is
-- an error.
deferOrReport :: [TyVarId] -> [Pred] -> Ct -> TcM [Ct]
deferOrReport skolems givenPredicates stuck = do
  predicate <- zonkPred (ctPred stuck)
  givens <- mapM zonkPred givenPredicates
  -- A given can also wait for a meta variable, as the given of a match on
  -- a scrutinee whose type the enclosing scope fixes does.
  let waits = not (null (predMetaVars predicate)) || not (all (null . predMetaVars) givens)
      deferrable = waits && not (any (`elem` skolems) (predTyVars predicate))
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
    IrredPred constraint -> typeMetaVars constraint
    QuantifiedPred _ antecedents consequent -> concatMap predMetaVars antecedents <> predMetaVars consequent

typeMetaVars :: TcType -> [Unique]
typeMetaVars ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcArrowTy -> []
    TcTyLit {} -> []
    TcTyVar _ -> []
    TcTyCon _ arguments -> concatMap typeMetaVars arguments
    TcKindedTyCon _ kindArguments -> concatMap typeMetaVars kindArguments
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
    IrredPred constraint -> typeTyVars constraint
    QuantifiedPred variables antecedents consequent ->
      filter (`notElem` variables) (concatMap predTyVars antecedents <> predTyVars consequent)

typeTyVars :: TcType -> [TyVarId]
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> [tyVar]
    TcMetaTv _ -> []
    TcArrowTy -> []
    TcTyLit {} -> []
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcKindedTyCon _ kindArguments -> concatMap typeTyVars kindArguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

rewritePred :: TcKinds -> [(TcType, TcType)] -> Pred -> Pred
rewritePred kinds equalities predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applyGivenSubst equalities) arguments)
    EqPred left right -> EqPred (applyGivenSubst equalities left) (applyGivenSubst equalities right)
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables (map (rewritePred kinds equalities) antecedents) (rewritePred kinds equalities consequent)
    IParamPred name payload -> IParamPred name (applyGivenSubst equalities payload)
    IrredPred constraint -> IrredPred (applyGivenSubst equalities constraint)

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
