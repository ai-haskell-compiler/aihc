{-# LANGUAGE OverloadedStrings #-}

-- | Shared value-binding helpers for expression-local declarations.
module Aihc.Tc.Generate.Bind
  ( InferExpr,
    inferLocalDecls,
    inferRhsWithLocals,
    collectRawSigs,
    sigToScheme,
    skolemize,
    schemeToType,
    renderBinderName,
  )
where

import Aihc.Parser.Syntax
  ( CaseAlt (..),
    Decl (..),
    Expr (..),
    Match (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    mkAnnotation,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Tc.Annotations (pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Generalize (generalizeIgnoring)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Kind (sigToScheme)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (foldM)
import Data.List (mapAccumL, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)

type InferExpr = Expr -> TcM (Expr, TcType, [Ct])

-- | Infer local declarations, then infer a body under the resulting binders.
inferLocalDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDecls inferExpr decls body = do
  let groups = groupValueDecls decls
      binders = nub (concatMap groupBinders groups)
  rawSigs <- collectRawSigs decls
  sigs <- traverse sigToScheme rawSigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList [(key, ty) | (_, key, ty) <- placeholders]
  binderSet <- Set.fromList <$> traverse resolvedLocalTermKey binders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders placeholders $ do
    groupResults <- mapM (inferLocalGroup inferExpr sigs placeholderMap) groups
    let bindingCts = concatMap snd groupResults
    if shouldGen
      then do
        _ <- solveConstraints bindingCts
        polyBinders <- traverse (generalizedBinder sigs binderSet placeholderMap) binders
        decls' <- annotateLocalBindingDecls polyBinders (concatMap (renderGroup . fst) groupResults)
        withLocalBinders polyBinders $ do
          (bodyResult, bodyTy, bodyCts) <- body
          pure (decls', bodyResult, bodyTy, bodyCts)
      else do
        monoBinders <- traverse (monomorphicBinder sigs placeholderMap) binders
        decls' <- annotateLocalBindingDecls monoBinders (concatMap (renderGroup . fst) groupResults)
        (bodyResult, bodyTy, bodyCts) <- body
        pure (decls', bodyResult, bodyTy, bindingCts ++ bodyCts)

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
    GuardedRhss {} -> do
      ty <- freshMetaTv
      pure (rhs, ty, [])

placeholderFor :: Map TcTermKey TypeScheme -> UnqualifiedName -> TcM (UnqualifiedName, TcTermKey, TcType)
placeholderFor sigs name = do
  key <- resolvedLocalTermKey name
  ty <- maybe freshMetaTv skolemize (Map.lookup key sigs)
  pure (name, key, ty)

withLocalPlaceholders :: [(UnqualifiedName, TcTermKey, TcType)] -> TcM a -> TcM a
withLocalPlaceholders placeholders =
  withLocalBinders
    [ (name, TcMonoIdBinder ty)
    | (name, _, ty) <- placeholders
    ]

withLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withLocalBinders [] action = action
withLocalBinders ((name, binder) : rest) action =
  extendResolvedTermEnv name binder (withLocalBinders rest action)

generalizedBinder :: Map TcTermKey TypeScheme -> Set.Set TcTermKey -> Map TcTermKey TcType -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
generalizedBinder sigs ignored placeholders name =
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
          Just ty -> do
            scheme <- generalizeIgnoring ignored ty []
            pure (name, TcIdBinder scheme Closed)

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
              (rhs', _ty, cts) <- inferRhsWithLocals inferExpr rhs
              pure (DeclValue (PatternBind mult pat rhs'), cts)
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
  patCheck <- checkPatterns NoSourceSpan (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithLocals inferExpr (matchRhs match))
  ev <- freshEvVar
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) pats
      resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (match {matchPats = pats', matchRhs = rhs'}, pcWantedCts patCheck ++ rhsCts ++ [resCt])

unifyMatchRhs :: InferExpr -> TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs inferExpr expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr (matchRhs match)
  ev <- freshEvVar
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

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
        DeclValue (PatternBind _ pat _) -> maybeToList (patternBinderName pat)
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
skolemize (ForAll tvs _preds body) = do
  subst <- Map.fromList <$> mapM mkSubst tvs
  pure (Aihc.Tc.Instantiate.applySubst subst body)
  where
    mkSubst tv = do
      sk <- freshSkolemTv (tvName tv)
      pure (tvUnique tv, TcTyVar sk)

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
      binder <- resolvedLocalTermKey name
      pure (Set.delete binder vars)
    DeclValue (PatternBind _ pat rhs) -> do
      vars <- freeVarsRhs rhs
      binders <- patternBinderKeys pat
      pure (Set.difference vars binders)
    DeclTypeSig {} -> pure Set.empty
    _ -> pure Set.empty

freeVarsMatch :: Match -> TcM (Set.Set TcTermKey)
freeVarsMatch match = do
  vars <- freeVarsRhs (matchRhs match)
  binders <- Set.unions <$> mapM patternBinderKeys (matchPats match)
  pure (Set.difference vars binders)

freeVarsRhs :: Rhs Expr -> TcM (Set.Set TcTermKey)
freeVarsRhs rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls -> do
      exprVars <- freeVarsExpr expr
      declVars <- maybe (pure Set.empty) freeVarsDecls maybeDecls
      pure (exprVars <> declVars)
    GuardedRhss _ _ maybeDecls ->
      maybe (pure Set.empty) freeVarsDecls maybeDecls

freeVarsExpr :: Expr -> TcM (Set.Set TcTermKey)
freeVarsExpr expr =
  case expr of
    EVar name -> Set.singleton <$> resolvedTermKey name
    EAnn _ inner -> freeVarsExpr inner
    EIf a b c -> Set.unions <$> mapM freeVarsExpr [a, b, c]
    ELambdaPats pats body -> do
      bodyVars <- freeVarsExpr body
      binders <- Set.unions <$> mapM patternBinderKeys pats
      pure (Set.difference bodyVars binders)
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
    ETuple _ items -> Set.unions <$> mapM (maybe (pure Set.empty) freeVarsExpr) items
    EApp f a -> do
      fVars <- freeVarsExpr f
      aVars <- freeVarsExpr a
      pure (fVars <> aVars)
    _ -> pure Set.empty

freeVarsAlt :: CaseAlt Expr -> TcM (Set.Set TcTermKey)
freeVarsAlt (CaseAlt _ pat rhs) = do
  vars <- freeVarsRhs rhs
  binders <- patternBinderKeys pat
  pure (Set.difference vars binders)

declBinderKeys :: [Decl] -> TcM (Set.Set TcTermKey)
declBinderKeys decls =
  Set.unions <$> mapM declBinderKeySet decls

declBinderKeySet :: Decl -> TcM (Set.Set TcTermKey)
declBinderKeySet decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name _) -> Set.singleton <$> resolvedLocalTermKey name
    DeclValue (PatternBind _ pat _) -> patternBinderKeys pat
    _ -> pure Set.empty

patternBinderKeys :: Pattern -> TcM (Set.Set TcTermKey)
patternBinderKeys pat =
  case pat of
    PVar name -> Set.singleton <$> resolvedLocalTermKey name
    PAnn _ inner -> patternBinderKeys inner
    PParen inner -> patternBinderKeys inner
    PAs name inner -> do
      key <- resolvedLocalTermKey name
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
