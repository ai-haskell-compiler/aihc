{-# LANGUAGE OverloadedStrings #-}

-- | Shared value-binding helpers for expression-local declarations.
module Aihc.Tc.Generate.Bind
  ( InferExpr,
    freeVarsDecl,
    freeVarsMatch,
    inferLocalDecls,
    inferRhsWithLocals,
    boolTyCon,
    collectRawSigs,
    sigToScheme,
    skolemize,
    schemeToType,
    renderBinderName,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    CaseAlt (..),
    Decl (..),
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    Match (..),
    NameType (..),
    Pattern (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Annotations (pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (TyConInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Generalize (environmentMetaVars, generalizeAndCommitIgnoring, predMetaVars)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Kind (sigToScheme)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (SolveResult (..), solveConstraints)
import Aihc.Tc.Solve.InertSet (InertSet (..))
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (foldM, forM_)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)

type InferExpr = Expr -> TcM (Expr, TcType, [Ct])

-- | Infer local declarations, then infer a body under the resulting binders.
inferLocalDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDecls inferExpr decls body
  | not (null decls) && all isImplicitParamDecl decls = inferImplicitParamDecls inferExpr decls body
inferLocalDecls inferExpr decls body = do
  let groups = groupValueDecls decls
  binders <- distinctLocalBinders (concatMap groupBinders groups)
  rawSigs <- collectRawSigs decls
  sigs <- traverse sigToScheme rawSigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList [(key, ty) | (_, key, ty) <- placeholders]
  binderSet <- Set.fromList <$> traverse resolvedLocalTermKey binders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders sigs placeholders $ do
    groupResults <- mapM (inferLocalGroup inferExpr sigs placeholderMap) groups
    let bindingCts = concatMap snd groupResults
    if shouldGen
      then do
        solveResult <- solveConstraints bindingCts
        residuals <- partitionLocalResiduals binderSet placeholderMap groups binders solveResult
        polyBinders <- traverse (generalizedBinder sigs binderSet placeholderMap residuals) binders
        decls' <- annotateLocalBindingDecls polyBinders (concatMap (renderGroup . fst) groupResults)
        withReboundLocalBinders polyBinders $ do
          (bodyResult, bodyTy, bodyCts) <- body
          pure (decls', bodyResult, bodyTy, localResidualOuterCts residuals ++ bodyCts)
      else do
        monoBinders <- traverse (monomorphicBinder sigs placeholderMap) binders
        decls' <- annotateLocalBindingDecls monoBinders (concatMap (renderGroup . fst) groupResults)
        (bodyResult, bodyTy, bodyCts) <- body
        pure (decls', bodyResult, bodyTy, bindingCts ++ bodyCts)

isImplicitParamDecl :: Decl -> Bool
isImplicitParamDecl decl =
  case peelDeclAnn decl of
    DeclImplicitParam {} -> True
    _ -> False

-- | Infer a group of implicit-parameter bindings, then infer the body.
--
-- Each right-hand side sees only the enclosing bindings. The body sees the
-- new bindings. The group solves each wanted implicit parameter of the body
-- that has a bound name. Other wanted constraints of the body float out.
inferImplicitParamDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferImplicitParamDecls inferExpr decls body = do
  bindings <- mapM (inferImplicitParamDecl inferExpr) decls
  (bodyResult, bodyTy, bodyCts) <- body
  let bound = [(name, ty) | (_, name, ty, _) <- bindings]
  remainingCts <- concat <$> mapM (solveBoundImplicitParam bound) bodyCts
  pure
    ( [decl | (decl, _, _, _) <- bindings],
      bodyResult,
      bodyTy,
      concat [cts | (_, _, _, cts) <- bindings] <> remainingCts
    )

inferImplicitParamDecl :: InferExpr -> Decl -> TcM (Decl, Text, TcType, [Ct])
inferImplicitParamDecl inferExpr decl =
  case decl of
    DeclAnn ann inner -> do
      (inner', name, ty, cts) <- inferImplicitParamDecl inferExpr inner
      pure (DeclAnn ann inner', name, ty, cts)
    DeclImplicitParam name expr maybeDecls -> do
      (expr', maybeDecls', ty, cts) <-
        case maybeDecls of
          Nothing -> do
            (expr', ty, cts) <- inferExpr expr
            pure (expr', Nothing, ty, cts)
          Just whereDecls -> do
            (whereDecls', expr', ty, cts) <- inferLocalDecls inferExpr whereDecls (inferExpr expr)
            pure (expr', Just whereDecls', ty, cts)
      let annotated = DeclAnn (mkAnnotation (pendingAnnotation ty [] [] [])) (DeclImplicitParam name expr' maybeDecls')
      pure (annotated, name, ty, cts)
    _ -> abortTc "implicit-parameter group contains another declaration"

-- | Solve one wanted constraint of the body against the new bindings.
--
-- The name of an implicit parameter determines its type, so the wanted type
-- must unify with the bound type.
solveBoundImplicitParam :: [(Text, TcType)] -> Ct -> TcM [Ct]
solveBoundImplicitParam bound ct =
  case ctPred ct of
    IParamPred name ty
      | Just boundTy <- lookup name bound -> do
          bindEvidence (ctEvVar ct) (EvGiven (IParamPred name boundTy))
          ev <- freshEvVar
          pure [mkWantedCt (EqPred ty boundTy) ev (ctOrigin ct) (ctLoc ct)]
    _ -> pure [ct]

distinctLocalBinders :: [UnqualifiedName] -> TcM [UnqualifiedName]
distinctLocalBinders = fmap snd . foldM addBinder (Set.empty, [])
  where
    addBinder (keys, binders) binder = do
      key <- resolvedLocalTermKey binder
      if Set.member key keys
        then pure (keys, binders)
        else pure (Set.insert key keys, binders <> [binder])

annotateLocalBindingDecls :: [(UnqualifiedName, TcBinder)] -> [Decl] -> TcM [Decl]
annotateLocalBindingDecls binders decls = do
  binderTypes <- Map.fromList <$> mapM binderTypeEntry binders
  mapM (annotateLocalBindingDecl binderTypes) decls
  where
    binderTypeEntry (name, binder) = do
      key <- resolvedLocalTermKey name
      pure (key, binderType binder)

annotateLocalBindingDecl :: Map TcTermKey TcType -> Decl -> TcM Decl
annotateLocalBindingDecl binderTypes decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateLocalBindingDecl binderTypes inner
    DeclValue valueDecl ->
      do
        keys <- valueDeclBinderKeys valueDecl
        case keys of
          key : _
            | Just ty <- Map.lookup key binderTypes ->
                pure (DeclAnn (mkAnnotation (pendingAnnotation ty [] [] [])) decl)
          _ -> pure decl
    _ -> pure decl

binderType :: TcBinder -> TcType
binderType (TcIdBinder scheme _) = schemeToType scheme
binderType (TcMonoIdBinder ty) = ty

valueDeclBinderKeys :: ValueDecl -> TcM [TcTermKey]
valueDeclBinderKeys valueDecl =
  case valueDecl of
    FunctionBind name _ -> (: []) <$> resolvedLocalTermKey name
    PatternBind _ pat _ -> patternBinderKeyList pat

monomorphicBinder :: Map TcTermKey TypeScheme -> Map TcTermKey TcType -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
monomorphicBinder sigs placeholders name =
  do
    key <- resolvedLocalTermKey name
    case Map.lookup key sigs of
      Just scheme -> pure (name, TcIdBinder scheme Closed)
      Nothing -> do
        ty <- maybe freshMetaTv zonkType (Map.lookup key placeholders)
        pure (name, TcMonoIdBinder ty)

-- | Infer an RHS, processing attached @where@ declarations first.
inferRhsWithLocals :: InferExpr -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhsWithLocals inferExpr rhs =
  case rhs of
    UnguardedRhs sp expr maybeDecls ->
      case maybeDecls of
        Nothing -> do
          (expr', ty, cts) <- inferExpr expr
          pure (UnguardedRhs sp expr' Nothing, ty, cts)
        Just decls -> do
          (decls', expr', ty, cts) <- inferLocalDecls inferExpr decls (inferExpr expr)
          pure (UnguardedRhs sp expr' (Just decls'), ty, cts)
    GuardedRhss anns guardedRhss maybeDecls ->
      case maybeDecls of
        Nothing -> do
          (guardedRhss', ty, cts) <- inferGuardedRhss inferExpr guardedRhss
          pure (GuardedRhss anns guardedRhss' Nothing, ty, cts)
        Just decls -> do
          (decls', guardedRhss', ty, cts) <- inferLocalDecls inferExpr decls (inferGuardedRhss inferExpr guardedRhss)
          pure (GuardedRhss anns guardedRhss' (Just decls'), ty, cts)

-- | Infer guarded alternatives. Each body has the shared result type.
inferGuardedRhss :: InferExpr -> [GuardedRhs Expr] -> TcM ([GuardedRhs Expr], TcType, [Ct])
inferGuardedRhss inferExpr guardedRhss = do
  resultTy <- freshMetaTv
  results <- mapM (inferGuardedRhs inferExpr resultTy) guardedRhss
  pure (map fst results, resultTy, concatMap snd results)

inferGuardedRhs :: InferExpr -> TcType -> GuardedRhs Expr -> TcM (GuardedRhs Expr, [Ct])
inferGuardedRhs inferExpr resultTy guardedRhs = do
  let sp = sourceSpanFromAnnotations (guardedRhsAnns guardedRhs)
  (qualifiers', body', cts) <-
    inferGuardQualifiers inferExpr sp resultTy (guardedRhsGuards guardedRhs) $ do
      (body', bodyTy, bodyCts) <- inferExpr (guardedRhsBody guardedRhs)
      ev <- freshEvVar
      let bodyCt = mkWantedCt (EqPred bodyTy resultTy) ev (AppOrigin sp) sp
      pure (body', bodyCts ++ [bodyCt])
  pure (guardedRhs {guardedRhsGuards = qualifiers', guardedRhsBody = body'}, cts)

-- | Infer guard qualifiers from left to right. A pattern guard and a let
-- guard bind names for the qualifiers and the body that follow them.
inferGuardQualifiers :: InferExpr -> SourceSpan -> TcType -> [GuardQualifier] -> TcM (a, [Ct]) -> TcM ([GuardQualifier], a, [Ct])
inferGuardQualifiers inferExpr sp resultTy qualifiers rest =
  case qualifiers of
    [] -> do
      (result, cts) <- rest
      pure ([], result, cts)
    GuardAnn ann inner : more -> do
      (qualifiers', result, cts) <- inferGuardQualifiers inferExpr sp resultTy (inner : more) rest
      case qualifiers' of
        inner' : more' -> pure (GuardAnn ann inner' : more', result, cts)
        [] -> pure ([], result, cts)
    GuardExpr condition : more -> do
      (condition', conditionTy, conditionCts) <- inferExpr condition
      boolTy <- boolTyCon
      ev <- freshEvVar
      let conditionCt = mkWantedCt (EqPred conditionTy boolTy) ev (AppOrigin sp) sp
      (more', result, cts) <- inferGuardQualifiers inferExpr sp resultTy more rest
      pure (GuardExpr condition' : more', result, conditionCts ++ [conditionCt] ++ cts)
    GuardPat pat scrutinee : more -> do
      (scrutinee', scrutineeTy, scrutineeCts) <- inferExpr scrutinee
      patCheck <- checkPattern sp pat scrutineeTy
      (more', result, cts) <- withPatternBindings (pcBindings patCheck) (inferGuardQualifiers inferExpr sp resultTy more rest)
      remainingCts <- solvePatternBranch sp patCheck resultTy cts
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      pure (GuardPat pat' scrutinee' : more', result, scrutineeCts ++ remainingCts)
    GuardLet decls : more -> do
      (decls', (more', result), _ty, cts) <-
        inferLocalDecls inferExpr decls $ do
          (more', result, cts) <- inferGuardQualifiers inferExpr sp resultTy more rest
          pure ((more', result), resultTy, cts)
      pure (GuardLet decls' : more', result, cts)

-- | The 'Bool' type that guards and conditions have.
boolTyCon :: TcM TcType
boolTyCon = do
  maybeInfo <- lookupTyCon "Bool"
  case maybeInfo of
    Just info -> pure (TcTyCon (tciTyCon info) [])
    Nothing -> TcTyCon <$> mkKnownTyCon "GHC.Types" "Bool" 0 typeKindType <*> pure []

placeholderFor :: Map TcTermKey TypeScheme -> UnqualifiedName -> TcM (UnqualifiedName, TcTermKey, TcType)
placeholderFor sigs name = do
  key <- resolvedLocalTermKey name
  ty <- maybe freshMetaTv skolemize (Map.lookup key sigs)
  pure (name, key, ty)

withLocalPlaceholders :: Map TcTermKey TypeScheme -> [(UnqualifiedName, TcTermKey, TcType)] -> TcM a -> TcM a
withLocalPlaceholders sigs placeholders =
  withLocalBinders
    [ (name, maybe (TcMonoIdBinder ty) (`TcIdBinder` Closed) (Map.lookup key sigs))
    | (name, key, ty) <- placeholders
    ]

withLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withLocalBinders [] action = action
withLocalBinders ((name, binder) : rest) action =
  extendResolvedTermEnv name binder (withLocalBinders rest action)

withReboundLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withReboundLocalBinders [] action = action
withReboundLocalBinders ((name, binder) : rest) action = do
  key <- resolvedLocalTermKey name
  rebindTermEnv key binder (withReboundLocalBinders rest action)

generalizedBinder :: Map TcTermKey TypeScheme -> Set.Set TcTermKey -> Map TcTermKey TcType -> LocalResiduals -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
generalizedBinder sigs ignored placeholders residuals name =
  do
    key <- resolvedLocalTermKey name
    case Map.lookup key sigs of
      Just scheme ->
        pure (name, TcIdBinder scheme Closed)
      Nothing ->
        case Map.lookup key placeholders of
          Nothing -> do
            ty <- freshMetaTv
            pure (name, TcMonoIdBinder ty)
          Just ty
            | key `Set.member` localResidualMonomorphic residuals -> do
                ty' <- zonkType ty
                pure (name, TcMonoIdBinder ty')
            | otherwise -> do
                let preds = Map.findWithDefault [] key (localResidualPreds residuals)
                scheme <- generalizeAndCommitIgnoring ignored ty preds
                pure (name, TcIdBinder scheme Closed)

-- | Residual constraints of a local binding group after the group solve.
data LocalResiduals = LocalResiduals
  { -- | Predicates that each generalized binder abstracts over.
    localResidualPreds :: Map TcTermKey [Pred],
    -- | Binders that the monomorphism restriction keeps monomorphic.
    localResidualMonomorphic :: Set.Set TcTermKey,
    -- | Constraints that the enclosing scope must solve.
    localResidualOuterCts :: [Ct]
  }

-- | Split the residual constraints of a local binding group.
--
-- A class constraint on a type variable that a function binder generalizes
-- becomes a dictionary parameter of that binder. The monomorphism
-- restriction keeps a pattern binding or a zero-argument binding
-- monomorphic when a constraint mentions its type. All other constraints
-- go to the enclosing scope.
partitionLocalResiduals :: Set.Set TcTermKey -> Map TcTermKey TcType -> [DeclGroup] -> [UnqualifiedName] -> SolveResult -> TcM LocalResiduals
partitionLocalResiduals binderSet placeholders groups binders solveResult = do
  residualCts <- mapM zonkCtPred (srResidual solveResult <> inertDicts (srInerts solveResult))
  envMetaVars <- environmentMetaVars binderSet
  restricted <- restrictedBinderKeys groups
  binderInfos <- traverse (binderMetaInfo placeholders) binders
  let step (preds, monomorphic, outerCts, givens) ct =
        let predicate = ctPred ct
            generalizable = filter (`notElem` envMetaVars) (predMetaVars predicate)
            owners = [key | (key, metas) <- binderInfos, any (`elem` metas) generalizable]
            restrictedOwners = filter (`Set.member` restricted) owners
         in if null generalizable || null owners || not (null restrictedOwners) || not (isClassPred predicate)
              then (preds, Set.union (Set.fromList restrictedOwners) monomorphic, outerCts ++ [ct], givens)
              else (foldr (\key -> Map.insertWith (flip (++)) key [predicate]) preds owners, monomorphic, outerCts, givens ++ [ct])
      (localPreds, monomorphicKeys, outer, givenCts) = foldl step (Map.empty, Set.empty, [], []) residualCts
  forM_ givenCts $ \ct ->
    bindEvidence (ctEvVar ct) (EvGiven (ctPred ct))
  pure
    LocalResiduals
      { localResidualPreds = localPreds,
        localResidualMonomorphic = monomorphicKeys,
        localResidualOuterCts = outer
      }
  where
    zonkCtPred ct = do
      predicate <- zonkPred (ctPred ct)
      pure (ct {ctPred = predicate})
    isClassPred ClassPred {} = True
    isClassPred _ = False
    binderMetaInfo placeholderMap name = do
      key <- resolvedLocalTermKey name
      ty <- maybe (pure Nothing) (fmap Just . zonkType) (Map.lookup key placeholderMap)
      pure (key, maybe [] typeMetaVars ty)

-- | Binders that the monomorphism restriction applies to: pattern bindings
-- and function bindings without arguments.
restrictedBinderKeys :: [DeclGroup] -> TcM (Set.Set TcTermKey)
restrictedBinderKeys groups = Set.fromList . concat <$> mapM restrictedKeys groups
  where
    restrictedKeys group =
      case group of
        MergedFunctionBind name _ (match : _)
          | null (matchPats match) -> (: []) <$> resolvedLocalTermKey name
        MergedFunctionBind {} -> pure []
        SingleDecl decl ->
          case peelDeclAnn decl of
            DeclValue (PatternBind _ pat _) -> patternBinderKeyList pat
            DeclValue (FunctionBind name (match : _))
              | null (matchPats match) -> (: []) <$> resolvedLocalTermKey name
            _ -> pure []

-- | Free meta-variables of a zonked type.
typeMetaVars :: TcType -> [Unique]
typeMetaVars ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcTyVar _ -> []
    TcTyCon _ args -> concatMap typeMetaVars args
    TcFunTy a b -> typeMetaVars a ++ typeMetaVars b
    TcForAllTy _ body -> typeMetaVars body
    TcQualTy ps body -> concatMap predMetaVars ps ++ typeMetaVars body
    TcAppTy f a -> typeMetaVars f ++ typeMetaVars a

inferLocalGroup :: InferExpr -> Map TcTermKey TypeScheme -> Map TcTermKey TcType -> DeclGroup -> TcM (DeclGroup, [Ct])
inferLocalGroup inferExpr sigs placeholders group =
  case group of
    MergedFunctionBind name decls matches -> do
      (matches', _ty, cts) <- inferLocalFunction inferExpr sigs placeholders name matches
      pure (MergedFunctionBind name (replaceFunctionDeclMatches matches' decls) matches', cts)
    SingleDecl decl -> do
      (decl', cts) <- inferLocalSingleDecl inferExpr sigs placeholders decl
      pure (SingleDecl decl', cts)

inferLocalSingleDecl :: InferExpr -> Map TcTermKey TypeScheme -> Map TcTermKey TcType -> Decl -> TcM (Decl, [Ct])
inferLocalSingleDecl inferExpr sigs placeholders decl =
  case decl of
    DeclAnn ann inner -> do
      (inner', cts) <- inferLocalSingleDecl inferExpr sigs placeholders inner
      pure (DeclAnn ann inner', cts)
    DeclValue valueDecl ->
      case valueDecl of
        PatternBind mult pat rhs ->
          case patternBinderName pat of
            Just name -> do
              (rhs', _ty, cts) <- inferLocalPatternBind inferExpr sigs placeholders name rhs
              pure (DeclValue (PatternBind mult pat rhs'), cts)
            Nothing -> do
              (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
              let sourceSpan = NoSourceSpan
              patCheck <- checkPatternsWithGivens sourceSpan [(pat, rhsTy)]
              patternCts <- solvePatternBranch sourceSpan patCheck rhsTy rhsCts
              cts <- foldM (tiePatternPlaceholder placeholders) patternCts (pcBindings patCheck)
              let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
              pure (DeclValue (PatternBind mult pat' rhs'), cts)
        FunctionBind name matches -> do
          (matches', _ty, cts) <- inferLocalFunction inferExpr sigs placeholders name matches
          pure (DeclValue (FunctionBind name matches'), cts)
    _ -> pure (decl, [])

inferLocalFunction :: InferExpr -> Map TcTermKey TypeScheme -> Map TcTermKey TcType -> UnqualifiedName -> [Match] -> TcM ([Match], TcType, [Ct])
inferLocalFunction inferExpr sigs placeholders name matches = do
  key <- resolvedLocalTermKey name
  (matches', ty, cts) <-
    case Map.lookup key sigs of
      Just scheme -> do
        sigTy <- maybe (skolemize scheme) pure (Map.lookup key placeholders)
        let nArgs =
              case matches of
                m : _ -> length (matchPats m)
                [] -> 0
            (argTys, resTy) = splitFunTy sigTy nArgs
        results <- mapM (tcMatchEquation inferExpr argTys resTy) matches
        let matches' = map fst results
            matchCts = concatMap snd results
        pure (matches', sigTy, matchCts)
      Nothing ->
        tcMatches inferExpr matches
  cts' <- tiePlaceholder placeholders key ty cts
  pure (matches', ty, cts')

inferLocalPatternBind :: InferExpr -> Map TcTermKey TypeScheme -> Map TcTermKey TcType -> UnqualifiedName -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferLocalPatternBind inferExpr sigs placeholders name rhs = do
  key <- resolvedLocalTermKey name
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
  ty <-
    case Map.lookup key sigs of
      Just scheme -> maybe (skolemize scheme) pure (Map.lookup key placeholders)
      Nothing -> pure rhsTy
  cts <- tiePlaceholder placeholders key ty rhsCts
  pure (rhs', ty, cts)

tiePlaceholder :: Map TcTermKey TcType -> TcTermKey -> TcType -> [Ct] -> TcM [Ct]
tiePlaceholder placeholders key ty cts =
  case Map.lookup key placeholders of
    Nothing -> pure cts
    Just placeholderTy -> do
      ev <- freshEvVar
      let eqCt = mkWantedCt (EqPred placeholderTy ty) ev (LetOrigin NoSourceSpan) NoSourceSpan
      pure (cts ++ [eqCt])

tiePatternPlaceholder :: Map TcTermKey TcType -> [Ct] -> (UnqualifiedName, TcType) -> TcM [Ct]
tiePatternPlaceholder placeholders cts (name, ty) = do
  key <- resolvedLocalTermKey name
  tiePlaceholder placeholders key ty cts

tcMatches :: InferExpr -> [Match] -> TcM ([Match], TcType, [Ct])
tcMatches _ [] = do
  ty <- freshMetaTv
  pure ([], ty, [])
tcMatches inferExpr matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      (firstMatch, ty0, cts0) <- inferZeroArgMatch inferExpr m0
      restResults <- mapM (unifyMatchRhs inferExpr ty0) (drop 1 matches)
      let restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (firstMatch : restMatches, ty0, cts0 ++ restCts)
    else do
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      results <- mapM (tcMatchEquation inferExpr argTys resTy) matches
      let matches' = map fst results
          allCts = concatMap snd results
      pure (matches', foldr TcFunTy resTy argTys, allCts)

inferZeroArgMatch :: InferExpr -> Match -> TcM (Match, TcType, [Ct])
inferZeroArgMatch inferExpr match = do
  (rhs', ty, cts) <- inferRhsWithLocals inferExpr (matchRhs match)
  pure (match {matchRhs = rhs'}, ty, cts)

tcMatchEquation :: InferExpr -> [TcType] -> TcType -> Match -> TcM (Match, [Ct])
tcMatchEquation inferExpr argTys resTy match = do
  let pats = matchPats match
      matchSpan = sourceSpanFromAnnotations (matchAnns match)
  patCheck <- checkFunctionPatternsWithGivens matchSpan (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithLocals inferExpr (matchRhs match))
  ev <- freshEvVar
  let rhsLocation = orSourceSpan (rhsSourceSpan (matchRhs match)) matchSpan
      pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin rhsLocation) rhsLocation
      bodyWanteds = rhsCts ++ [resCt]
  remainingCts <- solvePatternBranch rhsLocation patCheck resTy bodyWanteds
  pure (match {matchPats = pats', matchRhs = rhs'}, remainingCts)

sourceSpanFromAnnotations :: [Annotation] -> SourceSpan
sourceSpanFromAnnotations annotations =
  case mapMaybe fromAnnotation annotations of
    sourceSpan : _ -> sourceSpan
    [] -> NoSourceSpan

unifyMatchRhs :: InferExpr -> TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs inferExpr expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr (matchRhs match)
  ev <- freshEvVar
  let rhsLocation = orSourceSpan (rhsSourceSpan (matchRhs match)) (sourceSpanFromAnnotations (matchAnns match))
      eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin rhsLocation) rhsLocation
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

rhsSourceSpan :: Rhs body -> SourceSpan
rhsSourceSpan rhs =
  case rhs of
    UnguardedRhs annotations _ _ -> sourceSpanFromAnnotations annotations
    GuardedRhss annotations _ _ -> sourceSpanFromAnnotations annotations

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sourceSpan _ = sourceSpan

shouldGeneralizeLocal :: Set.Set TcTermKey -> [Decl] -> TcM Bool
shouldGeneralizeLocal binderSet decls = do
  monoLocal <- tcMonoLocalBinds
  if not monoLocal || any hasPartialTypeSig decls
    then pure True
    else do
      freeVars <- freeVarsDecls decls
      let externalVars = Set.toList (Set.difference freeVars binderSet)
      allM isClosedVar externalVars

isClosedVar :: TcTermKey -> TcM Bool
isClosedVar key = do
  env <- getTermEnv
  pure $
    case Map.lookup key env of
      Just (TcIdBinder _ Closed) -> True
      _ -> False

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM p = foldM step True
  where
    step False _ = pure False
    step True x = p x

data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind UnqualifiedName [Decl] [Match]

renderGroup :: DeclGroup -> [Decl]
renderGroup group =
  case group of
    SingleDecl decl -> [decl]
    MergedFunctionBind _ decls _ -> decls

groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) =
  case extractFunctionBind d of
    Just (name, matches) ->
      let (sameNameDecls, rest) = span (hasSameName name) ds
          groupDecls = d : sameNameDecls
          allMatches = matches ++ concatMap (maybe [] snd . extractFunctionBind) sameNameDecls
       in MergedFunctionBind name groupDecls allMatches : groupValueDecls rest
    Nothing -> SingleDecl d : groupValueDecls ds

groupBinders :: DeclGroup -> [UnqualifiedName]
groupBinders group =
  case group of
    MergedFunctionBind name _ _ -> [name]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []

patternBinderNames :: Pattern -> [UnqualifiedName]
patternBinderNames pat =
  case pat of
    PVar name -> [name]
    PAnn _ inner -> patternBinderNames inner
    PParen inner -> patternBinderNames inner
    PAs name inner -> name : patternBinderNames inner
    PStrict inner -> patternBinderNames inner
    PIrrefutable inner -> patternBinderNames inner
    PCon _ _ pats -> concatMap patternBinderNames pats
    PInfix lhs _ rhs -> patternBinderNames lhs <> patternBinderNames rhs
    _ -> []

extractFunctionBind :: Decl -> Maybe (UnqualifiedName, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> Just (name, matches)
    _ -> Nothing

hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name decl =
  case extractFunctionBind decl of
    Just (declName, _) -> unqualifiedNameText declName == unqualifiedNameText name
    Nothing -> False

replaceFunctionDeclMatches :: [Match] -> [Decl] -> [Decl]
replaceFunctionDeclMatches matches decls =
  snd (mapAccumL replace matches decls)
  where
    replace remaining decl =
      let count = functionDeclMatchCount decl
          (here, rest) = splitAt count remaining
       in (rest, replaceDeclFunctionMatches here decl)

functionDeclMatchCount :: Decl -> Int
functionDeclMatchCount decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind _ matches) -> length matches
    _ -> 0

replaceDeclFunctionMatches :: [Match] -> Decl -> Decl
replaceDeclFunctionMatches matches decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replaceDeclFunctionMatches matches inner)
    DeclValue (FunctionBind name _) -> DeclValue (FunctionBind name matches)
    _ -> decl

collectRawSigs :: [Decl] -> TcM (Map TcTermKey Type)
collectRawSigs decls = Map.fromList . concat <$> mapM extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      mapM (fmap (,ty) . resolvedLocalTermKey) names
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = pure []

skolemize :: TypeScheme -> TcM TcType
skolemize (ForAll _ _ body) = pure body

splitFunTy :: TcType -> Int -> ([TcType], TcType)
splitFunTy ty 0 = ([], ty)
splitFunTy (TcFunTy a rest) n =
  let (args, res) = splitFunTy rest (n - 1)
   in (a : args, res)
splitFunTy ty _ = ([], ty)

schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs
schemeToType (ForAll [] preds ty) = TcQualTy preds ty
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs

patternBinderName :: Pattern -> Maybe UnqualifiedName
patternBinderName (PVar n) = Just n
patternBinderName (PParen inner) = patternBinderName inner
patternBinderName (PAnn _ inner) = patternBinderName inner
patternBinderName _ = Nothing

renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

freeVarsDecls :: [Decl] -> TcM (Set.Set TcTermKey)
freeVarsDecls decls =
  Set.unions <$> mapM freeVarsDecl decls

freeVarsDecl :: Decl -> TcM (Set.Set TcTermKey)
freeVarsDecl decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> do
      vars <- Set.unions <$> mapM freeVarsMatch matches
      binder <- resolvedUnqualifiedTermKey name
      pure (Set.delete binder vars)
    DeclValue (PatternBind _ pat rhs) -> do
      vars <- freeVarsRhs rhs
      patVars <- freeVarsPattern pat
      binders <- patternBinderKeys pat
      pure (Set.difference (vars <> patVars) binders)
    DeclImplicitParam _ expr maybeDecls -> freeVarsRhs (UnguardedRhs [] expr maybeDecls)
    DeclTypeSig {} -> pure Set.empty
    _ -> pure Set.empty

freeVarsMatch :: Match -> TcM (Set.Set TcTermKey)
freeVarsMatch match = do
  vars <- freeVarsRhs (matchRhs match)
  patVars <- Set.unions <$> mapM freeVarsPattern (matchPats match)
  binders <- Set.unions <$> mapM patternBinderKeys (matchPats match)
  pure (Set.difference (vars <> patVars) binders)

-- | The term variables that the view functions of a pattern use.
freeVarsPattern :: Pattern -> TcM (Set.Set TcTermKey)
freeVarsPattern pat =
  case pat of
    PAnn _ inner -> freeVarsPattern inner
    PParen inner -> freeVarsPattern inner
    PAs _ inner -> freeVarsPattern inner
    PStrict inner -> freeVarsPattern inner
    PIrrefutable inner -> freeVarsPattern inner
    PTypeSig inner _ -> freeVarsPattern inner
    PList items -> Set.unions <$> mapM freeVarsPattern items
    PTuple _ items -> Set.unions <$> mapM freeVarsPattern items
    PUnboxedSum _ _ inner -> freeVarsPattern inner
    PCon _ _ subPats -> Set.unions <$> mapM freeVarsPattern subPats
    PInfix lhs _ rhs -> Set.union <$> freeVarsPattern lhs <*> freeVarsPattern rhs
    PRecord _ fields _ -> Set.unions <$> mapM (freeVarsPattern . recordFieldValue) fields
    PView viewExpr inner -> Set.union <$> freeVarsExpr viewExpr <*> freeVarsPattern inner
    _ -> pure Set.empty

freeVarsRhs :: Rhs Expr -> TcM (Set.Set TcTermKey)
freeVarsRhs rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls -> do
      exprVars <- freeVarsExpr expr
      declVars <- maybe (pure Set.empty) freeVarsDecls maybeDecls
      pure (exprVars <> declVars)
    GuardedRhss _ alternatives maybeDecls -> do
      altVars <- Set.unions <$> mapM freeVarsGuardedRhs alternatives
      declVars <- maybe (pure Set.empty) freeVarsDecls maybeDecls
      pure (altVars <> declVars)

freeVarsGuardedRhs :: GuardedRhs Expr -> TcM (Set.Set TcTermKey)
freeVarsGuardedRhs alternative =
  freeVarsGuardQualifiers (guardedRhsGuards alternative) (freeVarsExpr (guardedRhsBody alternative))

-- | The free variables of guard qualifiers and of the body they scope over.
-- A pattern guard or a let guard binds names for the later qualifiers.
freeVarsGuardQualifiers :: [GuardQualifier] -> TcM (Set.Set TcTermKey) -> TcM (Set.Set TcTermKey)
freeVarsGuardQualifiers qualifiers bodyVars =
  case qualifiers of
    [] -> bodyVars
    GuardAnn _ inner : rest -> freeVarsGuardQualifiers (inner : rest) bodyVars
    GuardExpr condition : rest ->
      Set.union <$> freeVarsExpr condition <*> freeVarsGuardQualifiers rest bodyVars
    GuardPat pat scrutinee : rest -> do
      scrutVars <- freeVarsExpr scrutinee
      patVars <- freeVarsPattern pat
      binders <- patternBinderKeys pat
      restVars <- freeVarsGuardQualifiers rest bodyVars
      pure (scrutVars <> patVars <> Set.difference restVars binders)
    GuardLet decls : rest -> do
      declVars <- freeVarsDecls decls
      localBinders <- declBinderKeys decls
      restVars <- freeVarsGuardQualifiers rest bodyVars
      pure (Set.difference (declVars <> restVars) localBinders)

freeVarsExpr :: Expr -> TcM (Set.Set TcTermKey)
freeVarsExpr expr =
  case expr of
    EVar name -> Set.singleton <$> resolvedTermKey name
    EAnn _ inner -> freeVarsExpr inner
    EIf a b c -> Set.unions <$> mapM freeVarsExpr [a, b, c]
    ELambdaPats pats body -> do
      bodyVars <- freeVarsExpr body
      patVars <- Set.unions <$> mapM freeVarsPattern pats
      binders <- Set.unions <$> mapM patternBinderKeys pats
      pure (Set.difference (bodyVars <> patVars) binders)
    EInfix lhs op rhs -> do
      lhsVars <- freeVarsExpr lhs
      rhsVars <- freeVarsExpr rhs
      opKey <- resolvedTermKey op
      pure (Set.insert opKey (lhsVars <> rhsVars))
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> do
      innerVars <- freeVarsExpr inner
      opKey <- resolvedTermKey op
      pure (Set.insert opKey innerVars)
    ESectionR op inner -> do
      innerVars <- freeVarsExpr inner
      opKey <- resolvedTermKey op
      pure (Set.insert opKey innerVars)
    ELetDecls decls body -> do
      declVars <- freeVarsDecls decls
      bodyVars <- freeVarsExpr body
      localBinders <- declBinderKeys decls
      pure (Set.difference (declVars <> bodyVars) localBinders)
    ECase scrut alts -> do
      scrutVars <- freeVarsExpr scrut
      altVars <- Set.unions <$> mapM freeVarsAlt alts
      pure (scrutVars <> altVars)
    ETypeSig inner _ -> freeVarsExpr inner
    EParen inner -> freeVarsExpr inner
    EList items -> Set.unions <$> mapM freeVarsExpr items
    EArithSeq arithSeq -> freeVarsArithSeq arithSeq
    ETuple _ items -> Set.unions <$> mapM (maybe (pure Set.empty) freeVarsExpr) items
    EApp f a -> do
      fVars <- freeVarsExpr f
      aVars <- freeVarsExpr a
      pure (fVars <> aVars)
    _ -> pure Set.empty

freeVarsArithSeq :: ArithSeq -> TcM (Set.Set TcTermKey)
freeVarsArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> do
      innerVars <- freeVarsArithSeq inner
      case fromAnnotation ann :: Maybe ResolutionAnnotation of
        Just resolution
          | resolutionNamespace resolution == ResolutionNamespaceTerm,
            IdentifierNamed methodName <- resolutionIdentifier resolution -> do
              methodKey <- resolvedTargetTermKey methodName (resolutionTarget resolution)
              pure (Set.insert methodKey innerVars)
        _ -> pure innerVars
    ArithSeqFrom from -> freeVarsExpr from
    ArithSeqFromThen from thenExpr -> Set.union <$> freeVarsExpr from <*> freeVarsExpr thenExpr
    ArithSeqFromTo from to -> Set.union <$> freeVarsExpr from <*> freeVarsExpr to
    ArithSeqFromThenTo from thenExpr to -> Set.unions <$> mapM freeVarsExpr [from, thenExpr, to]

freeVarsAlt :: CaseAlt Expr -> TcM (Set.Set TcTermKey)
freeVarsAlt (CaseAlt _ pat rhs) = do
  vars <- freeVarsRhs rhs
  patVars <- freeVarsPattern pat
  binders <- patternBinderKeys pat
  pure (Set.difference (vars <> patVars) binders)

declBinderKeys :: [Decl] -> TcM (Set.Set TcTermKey)
declBinderKeys decls =
  Set.unions <$> mapM declBinderKeySet decls

declBinderKeySet :: Decl -> TcM (Set.Set TcTermKey)
declBinderKeySet decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name _) -> Set.singleton <$> resolvedUnqualifiedTermKey name
    DeclValue (PatternBind _ pat _) -> patternBinderKeys pat
    _ -> pure Set.empty

patternBinderKeys :: Pattern -> TcM (Set.Set TcTermKey)
patternBinderKeys pat =
  case pat of
    PVar name -> Set.singleton <$> resolvedUnqualifiedTermKey name
    PAnn _ inner -> patternBinderKeys inner
    PParen inner -> patternBinderKeys inner
    PAs name inner -> do
      key <- resolvedUnqualifiedTermKey name
      Set.insert key <$> patternBinderKeys inner
    PStrict inner -> patternBinderKeys inner
    PIrrefutable inner -> patternBinderKeys inner
    PCon _ _ pats -> Set.unions <$> mapM patternBinderKeys pats
    PInfix lhs _ rhs -> do
      lhsKeys <- patternBinderKeys lhs
      rhsKeys <- patternBinderKeys rhs
      pure (lhsKeys <> rhsKeys)
    _ -> pure Set.empty

patternBinderKeyList :: Pattern -> TcM [TcTermKey]
patternBinderKeyList pat =
  case pat of
    PVar name -> (: []) <$> resolvedLocalTermKey name
    PAnn _ inner -> patternBinderKeyList inner
    PParen inner -> patternBinderKeyList inner
    PAs name inner -> do
      key <- resolvedLocalTermKey name
      (key :) <$> patternBinderKeyList inner
    PStrict inner -> patternBinderKeyList inner
    PIrrefutable inner -> patternBinderKeyList inner
    PCon _ _ pats -> concat <$> mapM patternBinderKeyList pats
    PInfix lhs _ rhs -> (++) <$> patternBinderKeyList lhs <*> patternBinderKeyList rhs
    _ -> pure []

hasPartialTypeSig :: Decl -> Bool
hasPartialTypeSig decl =
  case peelDeclAnn decl of
    DeclTypeSig _ ty -> hasWildcardType ty
    _ -> False

hasWildcardType :: Type -> Bool
hasWildcardType ty =
  case ty of
    TWildcard -> True
    TApp f a -> hasWildcardType f || hasWildcardType a
    TFun _ a b -> hasWildcardType a || hasWildcardType b
    TParen inner -> hasWildcardType inner
    TAnn _ inner -> hasWildcardType inner
    TContext preds inner -> any hasWildcardType preds || hasWildcardType inner
    TForall _ inner -> hasWildcardType inner
    TTuple _ _ args -> any hasWildcardType args
    TList _ args -> any hasWildcardType args
    _ -> False
