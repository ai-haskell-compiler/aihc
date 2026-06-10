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
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    mkAnnotation,
    nameText,
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
import Data.List (mapAccumL, nub, (\\))
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
  sigs <- traverse sigToScheme rawSigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList placeholders
      binderSet = Set.fromList binders
      ignored = binders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders placeholderMap $ do
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

annotateLocalBindingDecls :: [(Text, TcBinder)] -> [Decl] -> [Decl]
annotateLocalBindingDecls binders =
  map (annotateLocalBindingDecl binderTypes)
  where
    binderTypes = Map.fromList [(name, binderType binder) | (name, binder) <- binders]

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
    PatternBind _ pat _ -> patternBinders pat

monomorphicBinder :: Map Text TypeScheme -> Map Text TcType -> Text -> TcM (Text, TcBinder)
monomorphicBinder sigs placeholders name =
  case Map.lookup name sigs of
    Just scheme -> pure (name, TcIdBinder name scheme Closed)
    Nothing -> do
      ty <- maybe freshMetaTv zonkType (Map.lookup name placeholders)
      pure (name, TcMonoIdBinder name ty)

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

placeholderFor :: Map Text TypeScheme -> Text -> TcM (Text, TcType)
placeholderFor sigs name =
  case Map.lookup name sigs of
    Just scheme -> (name,) <$> skolemize scheme
    Nothing -> (name,) <$> freshMetaTv

withLocalPlaceholders :: Map Text TcType -> TcM a -> TcM a
withLocalPlaceholders placeholders =
  withLocalBinders
    [ (name, TcMonoIdBinder name ty)
    | (name, ty) <- Map.toList placeholders
    ]

withLocalBinders :: [(Text, TcBinder)] -> TcM a -> TcM a
withLocalBinders [] action = action
withLocalBinders ((name, binder) : rest) action =
  extendTermEnv name binder (withLocalBinders rest action)

generalizedBinder :: Map Text TypeScheme -> [Text] -> Map Text TcType -> Text -> TcM (Text, TcBinder)
generalizedBinder sigs ignored placeholders name =
  case Map.lookup name sigs of
    Just scheme ->
      pure (name, TcIdBinder name scheme Closed)
    Nothing ->
      case Map.lookup name placeholders of
        Nothing -> do
          ty <- freshMetaTv
          pure (name, TcMonoIdBinder name ty)
        Just ty -> do
          scheme <- generalizeIgnoring ignored ty []
          pure (name, TcIdBinder name scheme Closed)

inferLocalGroup :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> DeclGroup -> TcM (DeclGroup, [Ct])
inferLocalGroup inferExpr sigs placeholders group =
  case group of
    MergedFunctionBind name decls matches -> do
      (matches', _ty, cts) <- inferLocalFunction inferExpr sigs placeholders name matches
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
  let resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (match {matchRhs = rhs'}, pcWantedCts patCheck ++ rhsCts ++ [resCt])

unifyMatchRhs :: InferExpr -> TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs inferExpr expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr (matchRhs match)
  ev <- freshEvVar
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

shouldGeneralizeLocal :: Set.Set Text -> [Decl] -> TcM Bool
shouldGeneralizeLocal binderSet decls = do
  monoLocal <- tcMonoLocalBinds
  if not monoLocal || any hasPartialTypeSig decls
    then pure True
    else do
      let freeVars =
            Set.toList $
              Set.difference
                (Set.fromList (concatMap freeVarsDecl decls))
                binderSet
      allM isClosedVar freeVars

isClosedVar :: Text -> TcM Bool
isClosedVar name = do
  mBinder <- lookupTerm name
  pure $
    case mBinder of
      Just (TcIdBinder _ _ Closed) -> True
      _ -> False

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM p = foldM step True
  where
    step False _ = pure False
    step True x = p x

data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind Text [Decl] [Match]

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

groupBinders :: DeclGroup -> [Text]
groupBinders group =
  case group of
    MergedFunctionBind name _ _ -> [name]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [unqualifiedNameText name]
        DeclValue (PatternBind _ pat _) -> maybe [] ((: []) . snd) (patternBinderName pat)
        _ -> []

extractFunctionBind :: Decl -> Maybe (Text, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> Just (unqualifiedNameText name, matches)
    _ -> Nothing

hasSameName :: Text -> Decl -> Bool
hasSameName name decl =
  case extractFunctionBind decl of
    Just (declName, _) -> declName == name
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

patternBinderName :: Pattern -> Maybe (Text, Text)
patternBinderName (PVar n) = Just (renderBinderName n, unqualifiedNameText n)
patternBinderName (PParen inner) = patternBinderName inner
patternBinderName (PAnn _ inner) = patternBinderName inner
patternBinderName _ = Nothing

renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

nameToText :: Name -> Text
nameToText n =
  case nameQualifier n of
    Nothing -> nameText n
    Just q -> q <> "." <> nameText n

freeVarsDecl :: Decl -> [Text]
freeVarsDecl decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText name]
    DeclValue (PatternBind _ pat rhs) ->
      freeVarsRhs rhs \\ patternBinders pat
    DeclTypeSig {} -> []
    _ -> []

freeVarsMatch :: Match -> [Text]
freeVarsMatch match =
  freeVarsRhs (matchRhs match) \\ concatMap patternBinders (matchPats match)

freeVarsRhs :: Rhs Expr -> [Text]
freeVarsRhs rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls ->
      freeVarsExpr expr ++ maybe [] (concatMap freeVarsDecl) maybeDecls
    GuardedRhss _ _ maybeDecls ->
      maybe [] (concatMap freeVarsDecl) maybeDecls

freeVarsExpr :: Expr -> [Text]
freeVarsExpr expr =
  case expr of
    EVar name -> [nameToText name]
    EAnn _ inner -> freeVarsExpr inner
    EIf a b c -> freeVarsExpr a ++ freeVarsExpr b ++ freeVarsExpr c
    ELambdaPats pats body -> freeVarsExpr body \\ concatMap patternBinders pats
    EInfix lhs op rhs -> nameToText op : freeVarsExpr lhs ++ freeVarsExpr rhs
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> nameToText op : freeVarsExpr inner
    ESectionR op inner -> nameToText op : freeVarsExpr inner
    ELetDecls decls body ->
      let localBinders = concatMap declBinders decls
       in (concatMap freeVarsDecl decls ++ freeVarsExpr body) \\ localBinders
    ECase scrut alts -> freeVarsExpr scrut ++ concatMap freeVarsAlt alts
    ETypeSig inner _ -> freeVarsExpr inner
    EParen inner -> freeVarsExpr inner
    EList items -> concatMap freeVarsExpr items
    ETuple _ items -> concatMap (maybe [] freeVarsExpr) items
    EApp f a -> freeVarsExpr f ++ freeVarsExpr a
    _ -> []

freeVarsAlt :: CaseAlt Expr -> [Text]
freeVarsAlt (CaseAlt _ pat rhs) =
  freeVarsRhs rhs \\ patternBinders pat

declBinders :: Decl -> [Text]
declBinders decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name _) -> [unqualifiedNameText name]
    DeclValue (PatternBind _ pat _) -> patternBinders pat
    DeclTypeSig names _ -> map unqualifiedNameText names
    _ -> []

patternBinders :: Pattern -> [Text]
patternBinders pat =
  case pat of
    PVar name -> [unqualifiedNameText name]
    PAnn _ inner -> patternBinders inner
    PParen inner -> patternBinders inner
    PAs name inner -> unqualifiedNameText name : patternBinders inner
    PStrict inner -> patternBinders inner
    PIrrefutable inner -> patternBinders inner
    PCon _ _ pats -> concatMap patternBinders pats
    PInfix lhs _ rhs -> patternBinders lhs ++ patternBinders rhs
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
