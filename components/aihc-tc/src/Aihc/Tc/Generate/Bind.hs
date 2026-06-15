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
import Data.Set qualified as Set
import Data.Text (Text)

type InferExpr = Expr -> TcM (Expr, TcType, [Ct])

-- | Infer local declarations, then infer a body under the resulting binders.
inferLocalDecls :: InferExpr -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDecls inferExpr decls body = do
  let rawSigs = collectRawSigs decls
      groups = groupValueDecls decls
      binders = nub (concatMap groupBinders groups)
      binderNames = map unqualifiedNameText binders
  sigs <- traverse sigToScheme rawSigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList [(unqualifiedNameText name, ty) | (name, ty) <- placeholders]
      ignored = binderNames
  binderSet <- Set.fromList <$> traverse resolvedLocalTermKey binders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders placeholders $ do
    groupResults <- mapM (inferLocalGroup inferExpr sigs placeholderMap) groups
    let bindingCts = concatMap snd groupResults
    if shouldGen
      then do
        _ <- solveConstraints bindingCts
        polyBinders <- traverse (generalizedBinder sigs ignored placeholderMap) binders
        let decls' = annotateLocalBindingDecls polyBinders (concatMap (renderGroup . fst) groupResults)
        withLocalBinders polyBinders $ do
          (bodyResult, bodyTy, bodyCts) <- body
          pure (decls', bodyResult, bodyTy, bodyCts)
      else do
        monoBinders <- traverse (monomorphicBinder sigs placeholderMap) binders
        let decls' = annotateLocalBindingDecls monoBinders (concatMap (renderGroup . fst) groupResults)
        (bodyResult, bodyTy, bodyCts) <- body
        pure (decls', bodyResult, bodyTy, bindingCts ++ bodyCts)

annotateLocalBindingDecls :: [(UnqualifiedName, TcBinder)] -> [Decl] -> [Decl]
annotateLocalBindingDecls binders =
  map (annotateLocalBindingDecl binderTypes)
  where
    binderTypes = Map.fromList [(unqualifiedNameText name, binderType binder) | (name, binder) <- binders]

annotateLocalBindingDecl :: Map Text TcType -> Decl -> Decl
annotateLocalBindingDecl binderTypes decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (annotateLocalBindingDecl binderTypes inner)
    DeclValue valueDecl ->
      case valueDeclBinderNames valueDecl of
        name : _
          | Just ty <- Map.lookup name binderTypes ->
              DeclAnn (mkAnnotation (pendingAnnotation ty [] [] [])) decl
        _ -> decl
    _ -> decl

binderType :: TcBinder -> TcType
binderType (TcIdBinder _ scheme _) = schemeToType scheme
binderType (TcMonoIdBinder _ ty) = ty

valueDeclBinderNames :: ValueDecl -> [Text]
valueDeclBinderNames valueDecl =
  case valueDecl of
    FunctionBind name _ -> [unqualifiedNameText name]
    PatternBind _ pat _ -> patternBinderTexts pat

monomorphicBinder :: Map Text TypeScheme -> Map Text TcType -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
monomorphicBinder sigs placeholders name =
  let binderName = unqualifiedNameText name
   in case Map.lookup binderName sigs of
        Just scheme -> pure (name, TcIdBinder binderName scheme Closed)
        Nothing -> do
          ty <- maybe freshMetaTv zonkType (Map.lookup binderName placeholders)
          pure (name, TcMonoIdBinder binderName ty)

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

placeholderFor :: Map Text TypeScheme -> UnqualifiedName -> TcM (UnqualifiedName, TcType)
placeholderFor sigs name =
  case Map.lookup (unqualifiedNameText name) sigs of
    Just scheme -> (name,) <$> skolemize scheme
    Nothing -> (name,) <$> freshMetaTv

withLocalPlaceholders :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withLocalPlaceholders placeholders =
  withLocalBinders
    [ (name, TcMonoIdBinder (unqualifiedNameText name) ty)
    | (name, ty) <- placeholders
    ]

withLocalBinders :: [(UnqualifiedName, TcBinder)] -> TcM a -> TcM a
withLocalBinders [] action = action
withLocalBinders ((name, binder) : rest) action =
  extendResolvedTermEnv name binder (withLocalBinders rest action)

generalizedBinder :: Map Text TypeScheme -> [Text] -> Map Text TcType -> UnqualifiedName -> TcM (UnqualifiedName, TcBinder)
generalizedBinder sigs ignored placeholders name =
  let binderName = unqualifiedNameText name
   in case Map.lookup binderName sigs of
        Just scheme ->
          pure (name, TcIdBinder binderName scheme Closed)
        Nothing ->
          case Map.lookup binderName placeholders of
            Nothing -> do
              ty <- freshMetaTv
              pure (name, TcMonoIdBinder binderName ty)
            Just ty -> do
              scheme <- generalizeIgnoring ignored ty []
              pure (name, TcIdBinder binderName scheme Closed)

inferLocalGroup :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> DeclGroup -> TcM (DeclGroup, [Ct])
inferLocalGroup inferExpr sigs placeholders group =
  case group of
    MergedFunctionBind name decls matches -> do
      (matches', _ty, cts) <- inferLocalFunction inferExpr sigs placeholders (unqualifiedNameText name) matches
      pure (MergedFunctionBind name (replaceFunctionDeclMatches matches' decls) matches', cts)
    SingleDecl decl -> do
      (decl', cts) <- inferLocalSingleDecl inferExpr sigs placeholders decl
      pure (SingleDecl decl', cts)

inferLocalSingleDecl :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> Decl -> TcM (Decl, [Ct])
inferLocalSingleDecl inferExpr sigs placeholders decl =
  case decl of
    DeclAnn ann inner -> do
      (inner', cts) <- inferLocalSingleDecl inferExpr sigs placeholders inner
      pure (DeclAnn ann inner', cts)
    DeclValue valueDecl ->
      case valueDecl of
        PatternBind mult pat rhs ->
          case patternBinderName pat of
            Just (_displayName, name) -> do
              (rhs', _ty, cts) <- inferLocalPatternBind inferExpr sigs placeholders name rhs
              pure (DeclValue (PatternBind mult pat rhs'), cts)
            Nothing -> do
              (rhs', _ty, cts) <- inferRhsWithLocals inferExpr rhs
              pure (DeclValue (PatternBind mult pat rhs'), cts)
        FunctionBind name matches -> do
          (matches', _ty, cts) <- inferLocalFunction inferExpr sigs placeholders (unqualifiedNameText name) matches
          pure (DeclValue (FunctionBind name matches'), cts)
    _ -> pure (decl, [])

inferLocalFunction :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> Text -> [Match] -> TcM ([Match], TcType, [Ct])
inferLocalFunction inferExpr sigs placeholders name matches = do
  (matches', ty, cts) <-
    case Map.lookup name sigs of
      Just scheme -> do
        sigTy <- maybe (skolemize scheme) pure (Map.lookup name placeholders)
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
  cts' <- tiePlaceholder placeholders name ty cts
  pure (matches', ty, cts')

inferLocalPatternBind :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> Text -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferLocalPatternBind inferExpr sigs placeholders name rhs = do
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
  ty <-
    case Map.lookup name sigs of
      Just scheme -> maybe (skolemize scheme) pure (Map.lookup name placeholders)
      Nothing -> pure rhsTy
  cts <- tiePlaceholder placeholders name ty rhsCts
  pure (rhs', ty, cts)

tiePlaceholder :: Map Text TcType -> Text -> TcType -> [Ct] -> TcM [Ct]
tiePlaceholder placeholders name ty cts =
  case Map.lookup name placeholders of
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
      Just (TcIdBinder _ _ Closed) -> True
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
        DeclValue (PatternBind _ pat _) -> maybe [] ((: []) . fst) (patternBinderName pat)
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

collectRawSigs :: [Decl] -> Map Text Type
collectRawSigs decls = Map.fromList $ concatMap extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      [(unqualifiedNameText n, ty) | n <- names]
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = []

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

patternBinderName :: Pattern -> Maybe (UnqualifiedName, Text)
patternBinderName (PVar n) = Just (n, unqualifiedNameText n)
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

patternBinderTexts :: Pattern -> [Text]
patternBinderTexts pat =
  case pat of
    PVar name -> [unqualifiedNameText name]
    PAnn _ inner -> patternBinderTexts inner
    PParen inner -> patternBinderTexts inner
    PAs name inner -> unqualifiedNameText name : patternBinderTexts inner
    PStrict inner -> patternBinderTexts inner
    PIrrefutable inner -> patternBinderTexts inner
    PCon _ _ pats -> concatMap patternBinderTexts pats
    PInfix lhs _ rhs -> patternBinderTexts lhs ++ patternBinderTexts rhs
    _ -> []

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
