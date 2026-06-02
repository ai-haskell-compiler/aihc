{-# LANGUAGE OverloadedStrings #-}

-- | Shared value-binding helpers for expression-local declarations.
module Aihc.Tc.Generate.Bind
  ( InferExpr,
    inferLocalDeclBinders,
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
    nameText,
    peelDeclAnn,
    unqualifiedNameText,
  )
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
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

type InferExpr = Expr -> TcM (TcType, [Ct])

-- | Infer local declarations, then infer a body under the resulting binders.
inferLocalDecls :: InferExpr -> [Decl] -> TcM (TcType, [Ct]) -> TcM (TcType, [Ct])
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
    bindingCts <- concat <$> mapM (inferLocalGroup inferExpr sigs placeholderMap) groups
    if shouldGen
      then do
        _ <- solveConstraints bindingCts
        polyBinders <- traverse (generalizedBinder sigs ignored placeholderMap) binders
        withLocalBinders polyBinders $ do
          (bodyTy, bodyCts) <- body
          pure (bodyTy, bodyCts)
      else do
        (bodyTy, bodyCts) <- body
        pure (bodyTy, bindingCts ++ bodyCts)

inferLocalDeclBinders :: InferExpr -> [(Text, TcBinder)] -> [Decl] -> TcM [(Text, TcBinder)]
inferLocalDeclBinders inferExpr ambientBinders decls =
  withLocalBinders ambientBinders $ do
    inferLocalDeclBindersInner inferExpr decls

inferLocalDeclBindersInner :: InferExpr -> [Decl] -> TcM [(Text, TcBinder)]
inferLocalDeclBindersInner inferExpr decls = do
  let rawSigs = collectRawSigs decls
      groups = groupValueDecls decls
      binders = nub (concatMap groupBinders groups)
      binderSet = Set.fromList binders
      ignored = binders
  sigs <- traverse sigToScheme rawSigs
  placeholders <- traverse (placeholderFor sigs) binders
  let placeholderMap = Map.fromList placeholders
  shouldGen <- shouldGeneralizeLocal binderSet decls
  withLocalPlaceholders placeholderMap $ do
    bindingCts <- concat <$> mapM (inferLocalGroup inferExpr sigs placeholderMap) groups
    _ <- solveConstraints bindingCts
    if shouldGen
      then traverse (generalizedBinder sigs ignored placeholderMap) binders
      else traverse (monomorphicBinder sigs placeholderMap) binders

monomorphicBinder :: Map Text TypeScheme -> Map Text TcType -> Text -> TcM (Text, TcBinder)
monomorphicBinder sigs placeholders name =
  case Map.lookup name sigs of
    Just scheme -> pure (name, TcIdBinder name scheme Closed)
    Nothing -> do
      ty <- maybe freshMetaTv zonkType (Map.lookup name placeholders)
      pure (name, TcMonoIdBinder name ty)

-- | Infer an RHS, processing attached @where@ declarations first.
inferRhsWithLocals :: InferExpr -> Rhs Expr -> TcM (TcType, [Ct])
inferRhsWithLocals inferExpr rhs =
  case rhs of
    UnguardedRhs _sp expr maybeDecls ->
      case maybeDecls of
        Nothing -> inferExpr expr
        Just decls -> inferLocalDecls inferExpr decls (inferExpr expr)
    GuardedRhss _sp _guards _decls -> do
      ty <- freshMetaTv
      pure (ty, [])

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

inferLocalGroup :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> DeclGroup -> TcM [Ct]
inferLocalGroup inferExpr sigs placeholders group =
  case group of
    MergedFunctionBind name matches ->
      inferLocalFunction inferExpr sigs placeholders name matches
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (PatternBind _ pat rhs) ->
          case patternBinderName pat of
            Just (_displayName, name) ->
              inferLocalPatternBind inferExpr sigs placeholders name rhs
            Nothing -> do
              (_ty, cts) <- inferRhsWithLocals inferExpr rhs
              pure cts
        DeclValue (FunctionBind name matches) ->
          inferLocalFunction inferExpr sigs placeholders (unqualifiedNameText name) matches
        _ -> pure []

inferLocalFunction :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> Text -> [Match] -> TcM [Ct]
inferLocalFunction inferExpr sigs placeholders name matches = do
  (ty, cts) <-
    case Map.lookup name sigs of
      Just scheme -> do
        sigTy <- maybe (skolemize scheme) pure (Map.lookup name placeholders)
        let nArgs =
              case matches of
                m : _ -> length (matchPats m)
                [] -> 0
            (argTys, resTy) = splitFunTy sigTy nArgs
        matchCts <- concat <$> mapM (tcMatchEquation inferExpr argTys resTy) matches
        pure (sigTy, matchCts)
      Nothing ->
        tcMatches inferExpr matches
  tiePlaceholder placeholders name ty cts

inferLocalPatternBind :: InferExpr -> Map Text TypeScheme -> Map Text TcType -> Text -> Rhs Expr -> TcM [Ct]
inferLocalPatternBind inferExpr sigs placeholders name rhs = do
  (rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
  ty <-
    case Map.lookup name sigs of
      Just scheme -> maybe (skolemize scheme) pure (Map.lookup name placeholders)
      Nothing -> pure rhsTy
  tiePlaceholder placeholders name ty rhsCts

tiePlaceholder :: Map Text TcType -> Text -> TcType -> [Ct] -> TcM [Ct]
tiePlaceholder placeholders name ty cts =
  case Map.lookup name placeholders of
    Nothing -> pure cts
    Just placeholderTy -> do
      ev <- freshEvVar
      let eqCt = mkWantedCt (EqPred placeholderTy ty) ev (LetOrigin NoSourceSpan) NoSourceSpan
      pure (cts ++ [eqCt])

tcMatches :: InferExpr -> [Match] -> TcM (TcType, [Ct])
tcMatches _ [] = do
  ty <- freshMetaTv
  pure (ty, [])
tcMatches inferExpr matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      (ty0, cts0) <- inferRhsWithLocals inferExpr (matchRhs m0)
      restCts <- concat <$> mapM (unifyMatchRhs inferExpr ty0) (drop 1 matches)
      pure (ty0, cts0 ++ restCts)
    else do
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      allCts <- concat <$> mapM (tcMatchEquation inferExpr argTys resTy) matches
      pure (foldr TcFunTy resTy argTys, allCts)

tcMatchEquation :: InferExpr -> [TcType] -> TcType -> Match -> TcM [Ct]
tcMatchEquation inferExpr argTys resTy match = do
  let pats = matchPats match
  patCheck <- checkPatterns NoSourceSpan (zip pats argTys)
  (rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithLocals inferExpr (matchRhs match))
  ev <- freshEvVar
  let resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (pcWantedCts patCheck ++ rhsCts ++ [resCt])

unifyMatchRhs :: InferExpr -> TcType -> Match -> TcM [Ct]
unifyMatchRhs inferExpr expectedTy match = do
  (rhsTy, rhsCts) <- inferRhsWithLocals inferExpr (matchRhs match)
  ev <- freshEvVar
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (rhsCts ++ [eqCt])

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
  | MergedFunctionBind Text [Match]

groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) =
  case extractFunctionBind d of
    Just (name, matches) ->
      let (sameNameDecls, rest) = span (hasSameName name) ds
          allMatches = matches ++ concatMap (maybe [] snd . extractFunctionBind) sameNameDecls
       in MergedFunctionBind name allMatches : groupValueDecls rest
    Nothing -> SingleDecl d : groupValueDecls ds

groupBinders :: DeclGroup -> [Text]
groupBinders group =
  case group of
    MergedFunctionBind name _ -> [name]
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
