{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for declarations.
--
-- Processes top-level data declarations and value bindings from a module.
module Aihc.Tc.Generate.Decl
  ( tcModule,
    TcBindingResult (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BangType (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr (..),
    FieldDecl (..),
    ForeignDecl (..),
    ForeignDirection (..),
    GadtBody (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    gadtBodyResultType,
    instanceHeadName,
    instanceHeadTypes,
    mkAnnotation,
    nameText,
    peelClassDeclItemAnn,
    peelDeclAnn,
    tyVarBinderName,
  )
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    annotateDecl,
    annotateExpr,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Env (InstanceInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..))
import Aihc.Tc.Generalize (generalizeIgnoring)
import Aihc.Tc.Generate.Bind (inferLocalDeclBinders, inferRhsWithLocals)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkSurfaceType, convertSurfaceType, defaultKindMetas, freeTypeVars, freshKindMeta, kindToTcType, makeParamEnv, sigToScheme, surfacePredToPred, tyConKindFromParams)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints, solveWithImpls)
import Aihc.Tc.Solve.Dict (DictResult (..), solveDictWithGivens)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

peelDeclSpan :: SourceSpan -> Decl -> SourceSpan
peelDeclSpan ambient (DeclAnn ann inner) =
  peelDeclSpan (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
peelDeclSpan ambient _ = ambient

-- | Result of type-checking a single binding.
data TcBindingResult = TcBindingResult
  { -- | Canonical binder identity. Symbolic binders are stored without
    -- prefix-position parentheses, e.g. @++@ rather than @(++)@.
    tbName :: !Text,
    -- | Human-facing rendering for diagnostics and golden output.
    tbDisplayName :: !Text,
    tbType :: !TcType
  }
  deriving (Show)

-- | Type-check a module, returning the inferred types for each
-- top-level declaration: type constructors (with their kinds),
-- data constructors (with their types), and value bindings.
tcModule :: Module -> TcM ([TcBindingResult], Module)
tcModule m = do
  -- Phase 1: collect data declarations, register constructors,
  --          and report their types.
  dataResults <- concat <$> mapM registerDecl (moduleDecls m)
  -- Phase 2: collect type signatures and convert them to schemes.
  let rawSigs = collectRawSigs (moduleDecls m)
  schemes <- traverse sigToScheme rawSigs
  -- Phase 3: group and type-check value bindings using signatures.
  let grouped = sortDeclGroups (groupValueDecls (moduleDecls m))
  valueResults <- concat <$> mapM (tcDeclGroup schemes) grouped
  annotatedModule <- annotateModuleTc m
  pure (dataResults ++ valueResults, annotatedModule)

annotateModuleTc :: Module -> TcM Module
annotateModuleTc m = do
  let classMethods = collectClassMethodNames (moduleDecls m)
  decls <- mapM (annotateDeclTc classMethods) (moduleDecls m)
  pure (m {moduleDecls = decls})

annotateDeclTc :: Map Text [Text] -> Decl -> TcM Decl
annotateDeclTc classMethods decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateDeclTc classMethods inner
    DeclValue valueDecl -> DeclValue <$> annotateValueDeclTc valueDecl
    DeclClass classDecl -> annotateClassDeclTc classDecl
    DeclInstance instanceDecl -> annotateInstanceDeclTc classMethods instanceDecl
    _ -> pure decl

annotateClassDeclTc :: ClassDecl -> TcM Decl
annotateClassDeclTc classDecl = do
  methods <- zipWithM annotateClassMethod [0 :: Int ..] (classDeclMethodNames classDecl)
  pure (DeclAnn (mkAnnotation (TcClassAnnotation methods)) (DeclClass classDecl))

annotateClassMethod :: Int -> Text -> TcM TcClassMethodAnnotation
annotateClassMethod index methodName = do
  methodTy <- bindingType methodName
  let (tvs, _) = peelForAlls methodTy
  dictTy <- selectorDictTypeTc methodName methodTy
  pure
    TcClassMethodAnnotation
      { tcClassMethodName = methodName,
        tcClassMethodType = methodTy,
        tcClassMethodTyVars = tvs,
        tcClassMethodDictType = dictTy,
        tcClassMethodIndex = index
      }

annotateValueDeclTc :: ValueDecl -> TcM ValueDecl
annotateValueDeclTc valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      expected <- bindingType (unqualifiedNameText name)
      FunctionBind name <$> annotateMatchesTc expected matches
    PatternBind anns pat rhs ->
      case patternBinderName pat of
        Just (_displayName, name) -> do
          expected <- bindingType name
          PatternBind anns pat <$> annotateRhsTc Map.empty (qualifiedPreds expected) expected rhs
        Nothing -> pure valueDecl

annotateInstanceDeclTc :: Map Text [Text] -> InstanceDecl -> TcM Decl
annotateInstanceDeclTc classMethods instanceDecl =
  case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of
    (_, []) -> pure (DeclInstance instanceDecl)
    (Nothing, _) -> pure (DeclInstance instanceDecl)
    (Just className, headArgTypes) -> do
      let explicitTyVars = map tyVarBinderName (instanceDeclForall instanceDecl)
          freeVars = nub (explicitTyVars <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> headArgTypes))
      tvIds <- mapM freshSkolemTv freeVars
      let tvMap = Map.fromList (zip freeVars tvIds)
          classNameText = nameText className
      headTys <- mapM (convertSurfaceType tvMap) headArgTypes
      let dictName = instanceDictName classNameText headTys
      context <- mapM (surfacePredToPred (simpleTvKindEnv tvMap)) (instanceDeclContext instanceDecl)
      let contextDicts = map predDictBinder context
          dictTy = foldr TcForAllTy (TcQualTy context (predType (ClassPred classNameText headTys))) tvIds
          methodOrder = fromMaybe [] (Map.lookup classNameText classMethods)
          instAnn =
            TcInstanceAnnotation
              { tcInstanceDictName = dictName,
                tcInstanceDictType = dictTy,
                tcInstanceTyVars = tvIds,
                tcInstanceHeadTypes = headTys,
                tcInstanceContextDicts = contextDicts,
                tcInstanceMethodOrder = methodOrder
              }
      items <- mapM (annotateInstanceItemTc context headTys) (instanceDeclItems instanceDecl)
      pure (DeclAnn (mkAnnotation instAnn) (DeclInstance (instanceDecl {instanceDeclItems = items})))

annotateInstanceItemTc :: [Pred] -> [TcType] -> InstanceDeclItem -> TcM InstanceDeclItem
annotateInstanceItemTc givens headTys item =
  case item of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> annotateInstanceItemTc givens headTys inner
    InstanceItemBind (FunctionBind name matches) -> do
      let methodName = unqualifiedNameText name
      expected <- methodExpectedType headTys (unqualifiedNameText name)
      matches' <- annotateMatchesWithLocalsAndGivensTc Map.empty givens expected matches
      pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName expected)) (InstanceItemBind (FunctionBind name matches')))
    InstanceItemBind (PatternBind anns pat rhs) ->
      case patternBinderName pat of
        Just (_displayName, methodName) -> do
          expected <- methodExpectedType headTys methodName
          rhs' <- annotateRhsTc Map.empty givens expected rhs
          pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName expected)) (InstanceItemBind (PatternBind anns pat rhs')))
        Nothing -> pure item
    _ -> pure item

annotateMatchesTc :: TcType -> [Match] -> TcM [Match]
annotateMatchesTc expected =
  annotateMatchesWithLocalsAndGivensTc Map.empty (qualifiedPreds expected) expected

annotateMatchesWithLocalsAndGivensTc :: Map Text TcType -> [Pred] -> TcType -> [Match] -> TcM [Match]
annotateMatchesWithLocalsAndGivensTc locals givens expected =
  mapM (annotateMatchTc locals givens expected)

annotateMatchTc :: Map Text TcType -> [Pred] -> TcType -> Match -> TcM Match
annotateMatchTc outerLocals givens expected match = do
  let (argTys, resTy) = splitFunTy (qualifiedBody expected) (length (matchPats match))
  matchLocals <- Map.fromList . concat <$> zipWithM patternBindingsFrom (matchPats match) argTys
  let locals = outerLocals <> matchLocals
  rhs <- annotateRhsTc locals givens resTy (matchRhs match)
  pure (match {matchRhs = rhs})

annotateRhsTc :: Map Text TcType -> [Pred] -> TcType -> Rhs Expr -> TcM (Rhs Expr)
annotateRhsTc locals givens expected rhs =
  case rhs of
    UnguardedRhs sp expr Nothing -> do
      expr' <- annotateExprTc locals givens expected expr
      pure (UnguardedRhs sp expr' Nothing)
    UnguardedRhs sp expr (Just decls) -> do
      (localTypes, decls') <- annotateLocalDeclsTc givens locals decls
      expr' <- annotateExprTc (locals <> localTypes) givens expected expr
      pure (UnguardedRhs sp expr' (Just decls'))
    GuardedRhss {} -> pure rhs

annotateExprTc :: Map Text TcType -> [Pred] -> TcType -> Expr -> TcM Expr
annotateExprTc locals givens expected expr =
  case expr of
    EVar name -> annotateVarTc locals givens expected expr name
    EAnn ann inner -> EAnn ann <$> annotateExprTc locals givens expected inner
    EParen inner -> EParen <$> annotateExprTc locals givens expected inner
    ETypeSig inner ty -> (`ETypeSig` ty) <$> annotateExprTc locals givens expected inner
    EApp fun arg -> do
      argTy <- appArgTypeTc locals fun arg expected
      fun' <- annotateExprTc locals givens (TcFunTy argTy expected) fun
      arg' <- annotateExprTc locals givens argTy arg
      pure (annotateExpr (TcAnnotation expected [] [] []) (EApp fun' arg'))
    EInfix lhs op rhs ->
      annotateExprTc locals givens expected (EApp (EApp (EVar op) lhs) rhs)
    EList elems -> do
      elemTyFromItems <- listElemTypeFromItems locals elems
      elemTy <- maybe (missingTypeInfo "list element type") pure (listElemTyTc expected <|> elemTyFromItems)
      elems' <- mapM (annotateExprTc locals givens elemTy) elems
      pure (annotateExpr (TcAnnotation expected [elemTy] [] []) (EList elems'))
    EListComp body stmts -> do
      elemTy <- maybe (missingTypeInfo "list comprehension element type") pure (listElemTyTc expected)
      (locals', stmts') <- annotateCompStmtsTc locals givens stmts
      body' <- annotateExprTc locals' givens elemTy body
      pure (annotateExpr (TcAnnotation expected [elemTy] [] []) (EListComp body' stmts'))
    ETuple flavor elems -> do
      elemTys <- tupleElemTypesTc expected (length elems)
      elems' <- zipWithM (traverse . annotateExprTc locals givens) elemTys elems
      pure (annotateExpr (TcAnnotation expected elemTys [] []) (ETuple flavor elems'))
    EIf cond thenE elseE -> do
      cond' <- annotateExprTc locals givens boolTy cond
      then' <- annotateExprTc locals givens expected thenE
      else' <- annotateExprTc locals givens expected elseE
      pure (annotateExpr (TcAnnotation expected [] [] []) (EIf cond' then' else'))
    ELambdaPats pats body -> do
      let (argTys, resTy) = splitFunTy (qualifiedBody expected) (length pats)
      patBindings <- concat <$> zipWithM patternBindingsFrom pats argTys
      let locals' = locals <> Map.fromList patBindings
      body' <- annotateExprTc locals' givens resTy body
      pure (annotateExpr (TcAnnotation expected [] [] argTys) (ELambdaPats pats body'))
    ECase scrut alts -> do
      maybeScrutTy <- exprTypeMaybeTc locals scrut
      scrutTy <- maybe (missingTypeInfo ("case scrutinee type for " <> take 80 (show scrut))) pure maybeScrutTy
      scrut' <- annotateExprTc locals givens scrutTy scrut
      alts' <- mapM (annotateCaseAltTc locals givens scrutTy expected) alts
      pure (annotateExpr (TcAnnotation expected [] [] []) (ECase scrut' alts'))
    ELetDecls decls body -> do
      (localTypes, decls') <- annotateLocalDeclsTc givens locals decls
      body' <- annotateExprTc (locals <> localTypes) givens expected body
      pure (annotateExpr (TcAnnotation expected [] [] []) (ELetDecls decls' body'))
    _ -> pure expr

annotateLocalDeclsTc :: [Pred] -> Map Text TcType -> [Decl] -> TcM (Map Text TcType, [Decl])
annotateLocalDeclsTc givens outerLocals decls = do
  let ambientBinders = [(name, TcMonoIdBinder name ty) | (name, ty) <- Map.toList outerLocals]
  binders <- inferLocalDeclBinders inferExpr ambientBinders decls
  let localTypes = Map.fromList [(name, binderType binder) | (name, binder) <- binders]
      locals = outerLocals <> localTypes
  decls' <- mapM (annotateLocalDeclTc givens locals) decls
  pure (localTypes, decls')

annotateLocalDeclTc :: [Pred] -> Map Text TcType -> Decl -> TcM Decl
annotateLocalDeclTc givens locals decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateLocalDeclTc givens locals inner
    DeclValue valueDecl -> do
      (ty, valueDecl') <- annotateLocalValueDeclTc givens locals valueDecl
      pure (annotateDecl (TcAnnotation ty [] [] []) (DeclValue valueDecl'))
    _ -> pure decl

annotateLocalValueDeclTc :: [Pred] -> Map Text TcType -> ValueDecl -> TcM (TcType, ValueDecl)
annotateLocalValueDeclTc givens locals valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      expected <- localBindingType locals (unqualifiedNameText name)
      valueDecl' <- FunctionBind name <$> annotateMatchesWithLocalsAndGivensTc locals givens expected matches
      pure (expected, valueDecl')
    PatternBind anns pat rhs ->
      case patternBinderName pat of
        Just (_displayName, name) -> do
          expected <- localBindingType locals name
          valueDecl' <- PatternBind anns pat <$> annotateRhsTc locals givens expected rhs
          pure (expected, valueDecl')
        Nothing -> do
          ty <- missingTypeInfo ("local pattern binding " <> show pat)
          pure (ty, valueDecl)

localBindingType :: Map Text TcType -> Text -> TcM TcType
localBindingType locals name =
  case Map.lookup name locals of
    Just ty -> pure ty
    Nothing -> missingTypeInfo ("local binding " <> T.unpack name)

annotateCompStmtsTc :: Map Text TcType -> [Pred] -> [CompStmt] -> TcM (Map Text TcType, [CompStmt])
annotateCompStmtsTc locals _givens [] = pure (locals, [])
annotateCompStmtsTc locals givens (stmt : rest) =
  case stmt of
    CompAnn ann inner -> do
      (locals', stmts') <- annotateCompStmtsTc locals givens (inner : rest)
      case stmts' of
        inner' : rest' -> pure (locals', CompAnn ann inner' : rest')
        [] -> pure (locals', [])
    CompGen pat src -> do
      maybeSrcTy <- exprTypeMaybeTc locals src
      elemTy <-
        case maybeSrcTy >>= listElemTyTc of
          Just ty -> pure ty
          Nothing -> missingTypeInfo ("list comprehension generator element type for " <> take 80 (show src))
      src' <- annotateExprTc locals givens (listType elemTy) src
      patBindings <- patternBindingsFrom pat elemTy
      let localsWithPat = locals <> Map.fromList patBindings
      (locals', rest') <- annotateCompStmtsTc localsWithPat givens rest
      pure (locals', CompGen pat src' : rest')
    CompGuard guard -> do
      guard' <- annotateExprTc locals givens boolTy guard
      (locals', rest') <- annotateCompStmtsTc locals givens rest
      pure (locals', CompGuard guard' : rest')
    CompLetDecls decls -> do
      (letTypes, decls') <- annotateLocalDeclsTc givens locals decls
      let localsWithDecls = locals <> letTypes
      (locals', rest') <- annotateCompStmtsTc localsWithDecls givens rest
      pure (locals', CompLetDecls decls' : rest')
    CompThen expr -> do
      expr' <- annotateExprTc locals givens expectedTransformType expr
      (locals', rest') <- annotateCompStmtsTc locals givens rest
      pure (locals', CompThen expr' : rest')
    CompThenBy f byExpr -> do
      f' <- annotateExprTc locals givens expectedTransformType f
      byExpr' <- annotateExprTc locals givens expectedTransformType byExpr
      (locals', rest') <- annotateCompStmtsTc locals givens rest
      pure (locals', CompThenBy f' byExpr' : rest')
    CompGroupUsing expr -> do
      expr' <- annotateExprTc locals givens expectedTransformType expr
      (locals', rest') <- annotateCompStmtsTc locals givens rest
      pure (locals', CompGroupUsing expr' : rest')
    CompGroupByUsing byExpr usingExpr -> do
      byExpr' <- annotateExprTc locals givens expectedTransformType byExpr
      usingExpr' <- annotateExprTc locals givens expectedTransformType usingExpr
      (locals', rest') <- annotateCompStmtsTc locals givens rest
      pure (locals', CompGroupByUsing byExpr' usingExpr' : rest')
  where
    expectedTransformType = TcMetaTv (Unique (-1))

annotateCaseAltTc :: Map Text TcType -> [Pred] -> TcType -> TcType -> CaseAlt Expr -> TcM (CaseAlt Expr)
annotateCaseAltTc locals givens scrutTy expected (CaseAlt anns pat rhs) = do
  patBindings <- patternBindingsFrom pat scrutTy
  let locals' = locals <> Map.fromList patBindings
  rhs' <- annotateRhsTc locals' givens expected rhs
  pure (CaseAlt anns pat rhs')

annotateVarTc :: Map Text TcType -> [Pred] -> TcType -> Expr -> Name -> TcM Expr
annotateVarTc locals givens expected expr name =
  case lookupLocalName name locals of
    Just ty -> pure (annotateExpr (TcAnnotation ty [] [] []) expr)
    Nothing -> do
      mBinder <- lookupTermName name
      case mBinder of
        Just (TcIdBinder _ scheme _) -> do
          ann <- annotationForScheme givens scheme expected
          pure (annotateExpr ann expr)
        Just (TcMonoIdBinder _ ty) ->
          pure (annotateExpr (TcAnnotation ty [] [] []) expr)
        Nothing -> do
          _ <- missingTypeInfo ("variable " <> T.unpack (nameText name))
          pure expr

annotationForScheme :: [Pred] -> TypeScheme -> TcType -> TcM TcAnnotation
annotationForScheme givens scheme@(ForAll tvs preds body) expected =
  let needsInstantiation = not (null tvs && null preds)
   in do
        subst <-
          case matchTypes [body] [expected] of
            Just subst -> pure subst
            Nothing
              | needsInstantiation -> do
                  _ <- missingTypeInfo ("instantiation of " <> show scheme <> " at " <> show expected)
                  pure Map.empty
              | otherwise -> pure Map.empty
        let typeArgs = [substType subst (TcTyVar tv) | tv <- tvs]
            evidencePreds = map (substPred subst) preds
            occurrenceTy = case tvs of
              [] -> schemeToType scheme
              _ -> expected
        evidenceTerms <- mapM (evidenceForPred givens) evidencePreds
        pure (TcAnnotation occurrenceTy typeArgs evidenceTerms [])

evidenceForPred :: [Pred] -> Pred -> TcM EvTerm
evidenceForPred givens pred' =
  case pred' of
    ClassPred {} -> do
      ev <- freshEvVar
      result <- solveDictWithGivens givens (mkWantedCt pred' ev (OccurrenceOf "<annotation>") NoSourceSpan)
      case result of
        DictSolved -> do
          maybeEvidence <- lookupEvidence ev
          case maybeEvidence of
            Just evidence -> pure evidence
            Nothing -> do
              _ <- missingTypeInfo ("evidence for solved predicate " <> show pred')
              pure (EvGiven pred')
        DictStuck _ ->
          pure (EvGiven pred')
    EqPred left _right ->
      pure (EvCoercion (Refl left))

bindingType :: Text -> TcM TcType
bindingType name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just binder -> pure (binderType binder)
    Nothing -> missingTypeInfo ("binding " <> T.unpack name)

binderType :: TcBinder -> TcType
binderType (TcIdBinder _ scheme _) = schemeToType scheme
binderType (TcMonoIdBinder _ ty) = ty

appArgTypeTc :: Map Text TcType -> Expr -> Expr -> TcType -> TcM TcType
appArgTypeTc locals fun arg expected = do
  maybeArgTy <- exprTypeMaybeTc locals arg
  case maybeArgTy of
    Just ty | isUsableKnownType ty -> pure ty
    _ -> do
      maybeFunTy <- exprTypeMaybeTc locals fun
      case maybeFunTy >>= (`appArgTypeFromFunTc` expected) of
        Just ty -> pure ty
        Nothing -> missingTypeInfo ("application argument for " <> take 80 (show arg))

appArgTypeFromFunTc :: TcType -> TcType -> Maybe TcType
appArgTypeFromFunTc funTy expected =
  case qualifiedBody funTy of
    TcFunTy formalArg formalResult -> do
      subst <- matchTypes [formalResult] [expected]
      pure (substType subst formalArg)
    _ -> Nothing

exprTypeMaybeTc :: Map Text TcType -> Expr -> TcM (Maybe TcType)
exprTypeMaybeTc locals expr =
  case expr of
    EAnn _ inner -> exprTypeMaybeTc locals inner
    EParen inner -> exprTypeMaybeTc locals inner
    EInt {} -> pure (Just intTy)
    EChar {} -> pure (Just charTy)
    EString {} -> pure (Just (listType charTy))
    EVar name
      | nameText name == "True" || nameText name == "False" -> pure (Just boolTy)
      | otherwise ->
          case lookupLocalName name locals of
            Just ty -> pure (Just ty)
            Nothing -> fmap binderType <$> lookupTermName name
    EApp fun arg -> do
      funTy <- exprTypeMaybeTc locals fun
      argTy <- exprTypeMaybeTc locals arg
      pure (funTy >>= \fTy -> argTy >>= appResultTypeTc fTy)
    EInfix lhs op rhs -> exprTypeMaybeTc locals (EApp (EApp (EVar op) lhs) rhs)
    EList (item : _) -> fmap listType <$> exprTypeMaybeTc locals item
    _ -> pure Nothing

appResultTypeTc :: TcType -> TcType -> Maybe TcType
appResultTypeTc funTy argTy
  | isInstantiableType argTy = Nothing
  | otherwise =
      case qualifiedBody funTy of
        TcFunTy formalArg formalResult -> do
          subst <- matchTypes [formalArg] [argTy]
          pure (substType subst formalResult)
        _ -> Nothing

patternBindingsFrom :: Pattern -> TcType -> TcM [(Text, TcType)]
patternBindingsFrom pat ty =
  case pat of
    PVar name -> pure [(unqualifiedNameText name, ty)]
    PAnn _ inner -> patternBindingsFrom inner ty
    PParen inner -> patternBindingsFrom inner ty
    PAs name inner -> ((unqualifiedNameText name, ty) :) <$> patternBindingsFrom inner ty
    PStrict inner -> patternBindingsFrom inner ty
    PIrrefutable inner -> patternBindingsFrom inner ty
    PInfix lhs op rhs
      | nameText op == ":" ->
          case listElemTyTc ty of
            Just elemTy -> (++) <$> patternBindingsFrom lhs elemTy <*> patternBindingsFrom rhs ty
            Nothing -> do
              _ <- missingTypeInfo ("list pattern element type for " <> take 80 (show pat))
              pure []
      | otherwise -> (++) <$> patternBindingsFrom lhs ty <*> patternBindingsFrom rhs ty
    _ -> pure []

methodExpectedType :: [TcType] -> Text -> TcM TcType
methodExpectedType headTys methodName = do
  mBinder <- lookupTerm methodName
  case mBinder of
    Just (TcIdBinder _ (ForAll _ preds body) _) ->
      case firstClassPredSubst preds headTys of
        Just subst -> pure (substType subst body)
        Nothing -> missingTypeInfo ("class method receiver for " <> T.unpack methodName)
    Just (TcMonoIdBinder _ ty) -> pure ty
    Nothing -> missingTypeInfo ("class method " <> T.unpack methodName)

listElemTyTc :: TcType -> Maybe TcType
listElemTyTc (TcTyCon (TyCon "[]" 1) [elemTy]) = Just elemTy
listElemTyTc _ = Nothing

listElemTypeFromItems :: Map Text TcType -> [Expr] -> TcM (Maybe TcType)
listElemTypeFromItems _ [] = pure Nothing
listElemTypeFromItems locals (item : _) = exprTypeMaybeTc locals item

tupleElemTypesTc :: TcType -> Int -> TcM [TcType]
tupleElemTypesTc (TcTyCon (TyCon _ arity) elemTys) expectedArity
  | arity == expectedArity,
    length elemTys == expectedArity =
      pure elemTys
tupleElemTypesTc ty arity =
  replicate arity <$> missingTypeInfo ("tuple element types for " <> show arity <> "-tuple: " <> show ty)

firstClassPredSubst :: [Pred] -> [TcType] -> Maybe (Map Unique TcType)
firstClassPredSubst preds headTys =
  case [classArgs | ClassPred _ classArgs <- preds] of
    classArgs : _ -> matchTypes classArgs headTys
    [] -> Nothing

missingTypeInfo :: String -> TcM TcType
missingTypeInfo msg = do
  emitError NoSourceSpan (OtherError ("internal type annotation error: missing " <> msg))
  pure (TcMetaTv (Unique (-1)))

qualifiedBody :: TcType -> TcType
qualifiedBody ty =
  case splitQualifiedType ty of
    Just (_tvs, _preds, body) -> body
    Nothing -> snd (peelForAlls ty)

selectorDictTypeTc :: Text -> TcType -> TcM TcType
selectorDictTypeTc methodName methodTy =
  case snd (peelForAlls methodTy) of
    TcQualTy (pred' : _) _ -> pure (predType pred')
    _ -> missingTypeInfo ("class dictionary type for method selector " <> T.unpack methodName)

qualifiedPreds :: TcType -> [Pred]
qualifiedPreds ty =
  case splitQualifiedType ty of
    Just (_tvs, preds, _body) -> preds
    Nothing -> []

splitQualifiedType :: TcType -> Maybe ([TyVarId], [Pred], TcType)
splitQualifiedType ty =
  let (tvs, body) = peelForAlls ty
   in case body of
        TcQualTy preds inner -> Just (tvs, preds, inner)
        _ -> Nothing

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv body) =
  let (tvs, inner) = peelForAlls body
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

predDictBinder :: Pred -> TcDictBinderAnnotation
predDictBinder pred' =
  case pred' of
    ClassPred className args ->
      TcDictBinderAnnotation className args (predType pred')
    EqPred {} ->
      TcDictBinderAnnotation "<constraint>" [] (predType pred')

collectClassMethodNames :: [Decl] -> Map Text [Text]
collectClassMethodNames = Map.fromList . mapMaybe collect
  where
    collect decl =
      case peelDeclAnn decl of
        DeclClass classDecl ->
          Just (unqualifiedNameText (binderHeadName (classDeclHead classDecl)), classDeclMethodNames classDecl)
        _ -> Nothing

classDeclMethodNames :: ClassDecl -> [Text]
classDeclMethodNames classDecl = concatMap classItemMethodNames (classDeclItems classDecl)

classItemMethodNames :: ClassDeclItem -> [Text]
classItemMethodNames item =
  case peelClassDeclItemAnn item of
    ClassItemTypeSig names _ -> map unqualifiedNameText names
    _ -> []

isInstantiableType :: TcType -> Bool
isInstantiableType ty =
  case ty of
    TcForAllTy {} -> True
    TcQualTy {} -> True
    _ -> False

isUsableKnownType :: TcType -> Bool
isUsableKnownType ty = not (isInstantiableType ty) && not (hasMetaType ty)

hasMetaType :: TcType -> Bool
hasMetaType ty =
  case ty of
    TcMetaTv {} -> True
    TcTyCon _ args -> any hasMetaType args
    TcFunTy a b -> hasMetaType a || hasMetaType b
    TcForAllTy _ body -> hasMetaType body
    TcQualTy preds body -> any hasMetaPred preds || hasMetaType body
    TcAppTy f a -> hasMetaType f || hasMetaType a
    TcTyVar {} -> False
  where
    hasMetaPred (ClassPred _ args) = any hasMetaType args
    hasMetaPred (EqPred left right) = hasMetaType left || hasMetaType right

matchTypes :: [TcType] -> [TcType] -> Maybe (Map Unique TcType)
matchTypes patterns targets
  | length patterns /= length targets = Nothing
  | otherwise = foldM matchOne Map.empty (zip patterns targets)

matchOne :: Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchOne subst (TcTyVar tv, target) =
  case Map.lookup (tvUnique tv) subst of
    Nothing -> Just (Map.insert (tvUnique tv) target subst)
    Just existing
      | existing == target -> Just subst
      | otherwise -> Nothing
matchOne subst (TcTyCon tc args, TcTyCon targetTc targetArgs)
  | tc == targetTc,
    length args == length targetArgs =
      foldM matchOne subst (zip args targetArgs)
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) =
  matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB)
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) =
  matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA)
matchOne subst (patternTy, targetTy)
  | patternTy == targetTy = Just subst
  | otherwise = Nothing

substPred :: Map Unique TcType -> Pred -> Pred
substPred subst (ClassPred className args) = ClassPred className (map (substType subst) args)
substPred subst (EqPred left right) = EqPred (substType subst left) (substType subst right)

substType :: Map Unique TcType -> TcType -> TcType
substType = Aihc.Tc.Instantiate.applySubst

nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

lookupLocalName :: Name -> Map Text TcType -> Maybe TcType
lookupLocalName name locals =
  Map.lookup (nameToText name) locals <|> Map.lookup (nameText name) locals

lookupTermName :: Name -> TcM (Maybe TcBinder)
lookupTermName name = do
  qualified <- lookupTerm (nameToText name)
  case qualified of
    Just binder -> pure (Just binder)
    Nothing -> lookupTerm (nameText name)

intTy :: TcType
intTy = TcTyCon (TyCon "Int" 0) []

charTy :: TcType
charTy = TcTyCon (TyCon "Char" 0) []

boolTy :: TcType
boolTy = TcTyCon (TyCon "Bool" 0) []

-- | Collect type signatures from a list of declarations.
collectRawSigs :: [Decl] -> Map Text Type
collectRawSigs decls = Map.fromList $ concatMap extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      [(unqualifiedNameText n, ty) | n <- names]
    extractSig (DeclForeign foreignDecl)
      | isForeignImport foreignDecl =
          [(unqualifiedNameText (foreignName foreignDecl), foreignType foreignDecl)]
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = []

listType :: TcType -> TcType
listType ty = TcTyCon (TyCon "[]" 1) [ty]

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

simpleTvKindEnv :: Map Text TyVarId -> TvKindEnv
simpleTvKindEnv = Map.map (,KType)

-- | Instantiate a type scheme with fresh skolems for type-checking.
-- Unlike regular instantiation (which uses metas), this produces rigid
-- type variables that cannot be unified during constraint solving.
skolemize :: TypeScheme -> TcM TcType
skolemize (ForAll tvs _preds body) = do
  subst <- Map.fromList <$> mapM mkSubst tvs
  pure (Aihc.Tc.Instantiate.applySubst subst body)
  where
    mkSubst tv = do
      sk <- freshSkolemTv (tvName tv)
      pure (tvUnique tv, TcTyVar sk)

-- | Split a function type into argument types and result type.
splitFunTy :: TcType -> Int -> ([TcType], TcType)
splitFunTy ty 0 = ([], ty)
splitFunTy (TcFunTy a rest) n =
  let (args, res) = splitFunTy rest (n - 1)
   in (a : args, res)
splitFunTy ty _ = ([], ty)

-- | A group of declarations that should be typechecked together.
-- Multiple FunctionBind equations for the same name are merged.
data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind SourceSpan UnqualifiedName [Match]

-- | Group consecutive FunctionBind declarations with the same name.
groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) = case extractFunctionBind d of
  Just (sp, name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] (\(_, _, ms) -> ms) . extractFunctionBind) sameNameDecls
     in MergedFunctionBind sp name allMatches : groupValueDecls rest
  Nothing -> SingleDecl d : groupValueDecls ds

-- | Extract function bind info from a declaration.
extractFunctionBind :: Decl -> Maybe (SourceSpan, UnqualifiedName, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) ->
      let sp = peelDeclSpan NoSourceSpan decl
       in Just (sp, name, matches)
    _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name d = case extractFunctionBind d of
  Just (_, n, _) -> unqualifiedNameText n == unqualifiedNameText name
  Nothing -> False

-- | Sort top-level groups so acyclic forward references are checked after
-- their dependencies have been generalized into the global environment.
sortDeclGroups :: [DeclGroup] -> [DeclGroup]
sortDeclGroups groups =
  concatMap flattenScc (stronglyConnComp nodes)
  where
    allBinders = Set.fromList (concatMap declGroupBinders groups)
    nodes =
      [ (group, groupKey ix group, Set.toList (Set.intersection allBinders (Set.fromList (freeVarsGroup group))))
      | (ix, group) <- zip [(0 :: Int) ..] groups
      ]
    flattenScc (AcyclicSCC group) = [group]
    flattenScc (CyclicSCC cyclicGroups) = cyclicGroups

groupKey :: Int -> DeclGroup -> Text
groupKey ix group =
  case declGroupBinders group of
    name : _ -> name
    [] -> "<decl-" <> fromString (show ix) <> ">"

declGroupBinders :: DeclGroup -> [Text]
declGroupBinders group =
  case group of
    MergedFunctionBind _sp binder _matches -> [unqualifiedNameText binder]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind binder _) -> [unqualifiedNameText binder]
        DeclValue (PatternBind _ pat _) -> maybe [] ((: []) . snd) (patternBinderName pat)
        _ -> []

freeVarsGroup :: DeclGroup -> [Text]
freeVarsGroup group =
  case group of
    MergedFunctionBind _sp binder matches ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText binder]
    SingleDecl decl -> freeVarsDecl decl

freeVarsDecl :: Decl -> [Text]
freeVarsDecl decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind binder matches) ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText binder]
    DeclValue (PatternBind _ pat rhs) ->
      freeVarsRhs rhs \\ patternBinders pat
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
    EVar name -> [patNameToText name]
    EAnn _ inner -> freeVarsExpr inner
    EIf cond trueBranch falseBranch ->
      freeVarsExpr cond ++ freeVarsExpr trueBranch ++ freeVarsExpr falseBranch
    ELambdaPats pats body ->
      freeVarsExpr body \\ concatMap patternBinders pats
    EInfix lhs op rhs ->
      patNameToText op : freeVarsExpr lhs ++ freeVarsExpr rhs
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> patNameToText op : freeVarsExpr inner
    ESectionR op inner -> patNameToText op : freeVarsExpr inner
    ELetDecls decls body ->
      let localBinders = concatMap declBinders decls
       in (concatMap freeVarsDecl decls ++ freeVarsExpr body) \\ localBinders
    ECase scrut alts ->
      freeVarsExpr scrut ++ concatMap freeVarsAlt alts
    ETypeSig inner _ -> freeVarsExpr inner
    EParen inner -> freeVarsExpr inner
    EList items -> concatMap freeVarsExpr items
    ETuple _ items -> concatMap (maybe [] freeVarsExpr) items
    EApp fun arg -> freeVarsExpr fun ++ freeVarsExpr arg
    _ -> []

freeVarsAlt :: CaseAlt Expr -> [Text]
freeVarsAlt (CaseAlt _ pat rhs) =
  freeVarsRhs rhs \\ patternBinders pat

declBinders :: Decl -> [Text]
declBinders decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind binder _) -> [unqualifiedNameText binder]
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

-- | Type-check a declaration group.
tcDeclGroup :: Map Text TypeScheme -> DeclGroup -> TcM [TcBindingResult]
tcDeclGroup sigs (SingleDecl d) =
  case peelDeclAnn d of
    DeclValue (PatternBind _ pat rhs)
      | Just (displayName, name) <- patternBinderName pat,
        Just scheme <- Map.lookup name sigs ->
          tcFunctionWithSig displayName name scheme [zeroArgMatch rhs]
    _ -> tcDecl d
tcDeclGroup sigs (MergedFunctionBind _sp binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  case Map.lookup name sigs of
    Just scheme -> do
      -- Use the declared type signature for checking.
      tcFunctionWithSig displayName name scheme matches
    Nothing -> do
      -- No signature: infer the type.
      tcFunctionInfer displayName name matches

-- | Type-check a function with a known type signature.
-- The signature's type variables are opened as rigid skolems so that
-- the body is checked against them. GADT patterns generate implication
-- constraints using the signature's skolems as given equalities.
tcFunctionWithSig :: Text -> Text -> TypeScheme -> [Match] -> TcM [TcBindingResult]
tcFunctionWithSig displayName name scheme matches = do
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
  -- Open the scheme with skolems (not metas) for checking.
  sigTy <- skolemize scheme
  let nArgs = case matches of
        (m : _) -> length (matchPats m)
        [] -> 0
      (argTys, resTy) = splitFunTy sigTy nArgs
  -- Check each equation against the signature types.
  results <- mapM (tcMatchEquation argTys resTy) matches
  let (ctsList, implsList) = unzip results
      allCts = concat ctsList
      allImpls = concat implsList
  _ <- solveWithImpls allCts allImpls
  -- Report the declared scheme as the binding's type.
  let declaredTy = schemeToType scheme
  zonkedTy <- zonkType declaredTy
  pure [TcBindingResult name displayName zonkedTy]

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: Text -> Text -> [Match] -> TcM [TcBindingResult]
tcFunctionInfer displayName name matches = do
  placeholderTy <- freshMetaTv
  extendTermEnvPermanent name (TcMonoIdBinder name placeholderTy)
  (ty, cts, impls) <- tcMatches matches
  _ <- solveWithImpls cts impls
  scheme <- generalizeIgnoring [name] ty []
  let schemeTy = schemeToType scheme
  zonkedTy <- zonkType schemeTy
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
  pure [TcBindingResult name displayName zonkedTy]

-- | Register a declaration in the environment (data types, etc.).
-- Returns binding results for the declared names.
registerDecl :: Decl -> TcM [TcBindingResult]
registerDecl (DeclData dd) = registerDataDecl dd
registerDecl (DeclClass classDecl) = registerClassDecl classDecl
registerDecl (DeclInstance instanceDecl) = registerInstanceDecl instanceDecl
registerDecl (DeclForeign foreignDecl)
  | isForeignImport foreignDecl =
      registerForeignImport foreignDecl
registerDecl (DeclAnn _ inner) = registerDecl inner
registerDecl _ = pure []

isForeignImport :: ForeignDecl -> Bool
isForeignImport foreignDecl =
  foreignDirection foreignDecl == ForeignImport

registerForeignImport :: ForeignDecl -> TcM [TcBindingResult]
registerForeignImport foreignDecl = do
  scheme <- sigToScheme (foreignType foreignDecl)
  let name = unqualifiedNameText (foreignName foreignDecl)
      displayName = renderBinderName (foreignName foreignDecl)
      declaredTy = schemeToType scheme
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
  zonkedTy <- zonkType declaredTy
  pure [TcBindingResult name displayName zonkedTy]

registerClassDecl :: ClassDecl -> TcM [TcBindingResult]
registerClassDecl classDecl = do
  let className = unqualifiedNameText (binderHeadName (classDeclHead classDecl))
      params = binderHeadParams (classDeclHead classDecl)
  paramInfos <- makeParamEnv params
  let paramTyVars = map paramTyVar paramInfos
      paramKinds = map paramKind paramInfos
      paramTvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos]
      classPred = ClassPred className (map TcTyVar paramTyVars)
  classKind <- defaultKindMetas (foldr KFun KConstraint paramKinds)
  extendTyConEnvPermanent
    className
    TyConInfo
      { tciName = className,
        tciArity = length params,
        tciTyCon = TyCon className (length params),
        tciKind = classKind
      }
  superPreds <- concat <$> traverse (mapM (surfacePredToPred paramTvEnv)) (maybeToList (classDeclContext classDecl))
  concat <$> mapM (registerClassItem classPred superPreds paramTvEnv paramTyVars) (classDeclItems classDecl)

registerClassItem :: Pred -> [Pred] -> TvKindEnv -> [TyVarId] -> ClassDeclItem -> TcM [TcBindingResult]
registerClassItem classPred superPreds classTvEnv classTyVars item =
  case peelClassDeclItemAnn item of
    ClassItemTypeSig names ty -> do
      let (context, body) = splitContext ty
          classVarNames = Map.keys classTvEnv
          freeVars = freeTypeVars ty \\ classVarNames
      extraTyVars <- mapM freshSkolemTv freeVars
      extraKinds <- mapM (const freshKindMeta) freeVars
      let tvEnv = classTvEnv <> Map.fromList (zip freeVars (zip extraTyVars extraKinds))
      methodBody <- checkSurfaceType tvEnv body KType
      contextPreds <- mapM (surfacePredToPred tvEnv) context
      let preds = classPred : superPreds <> contextPreds
          scheme = ForAll (classTyVars <> extraTyVars) preds methodBody
          declaredTy = schemeToType scheme
      mapM
        ( \methodName -> do
            let name = unqualifiedNameText methodName
                displayName = renderBinderName methodName
            extendTermEnvPermanent name (TcIdBinder name scheme Closed)
            zonkedTy <- zonkType declaredTy
            pure (TcBindingResult name displayName zonkedTy)
        )
        names
    _ -> pure []

registerInstanceDecl :: InstanceDecl -> TcM [TcBindingResult]
registerInstanceDecl instanceDecl =
  case instanceHeadName (instanceDeclHead instanceDecl) of
    Nothing -> pure []
    Just className -> do
      let headArgs = instanceHeadTypes (instanceDeclHead instanceDecl)
          explicitTyVars = map tyVarBinderName (instanceDeclForall instanceDecl)
          freeVars = nub (explicitTyVars <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> headArgs))
      tvIds <- mapM freshSkolemTv freeVars
      let tvMap = Map.fromList (zip freeVars tvIds)
          classNameText = nameText className
      headTys <- mapM (convertSurfaceType tvMap) headArgs
      let dictName = instanceDictName classNameText headTys
      context <- mapM (surfacePredToPred (simpleTvKindEnv tvMap)) (instanceDeclContext instanceDecl)
      let dictTy = foldr TcForAllTy (TcQualTy context (predType (ClassPred classNameText headTys))) tvIds
      addInstance
        InstanceInfo
          { iiClassName = classNameText,
            iiDictName = dictName,
            iiDictType = dictTy,
            iiTyVars = tvIds,
            iiContext = context,
            iiHead = headTys
          }
      pure [TcBindingResult dictName dictName dictTy]

predType :: Pred -> TcType
predType (ClassPred className args) = TcTyCon (TyCon className (length args)) args
predType (EqPred left right) = TcTyCon (TyCon "~" 2) [left, right]

instanceDictName :: Text -> [TcType] -> Text
instanceDictName className tys = "$f" <> className <> T.concat (map typeSuffix tys)

typeSuffix :: TcType -> Text
typeSuffix ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [_] -> "List"
    TcTyCon tc args -> tyConName tc <> T.concat (map typeSuffix args)
    _ -> "T"

-- | Register a data declaration's type constructor and data constructors.
--
-- For @data Bool = True | False@, this produces:
--   - @Bool :: *@
--   - @True :: Bool@
--   - @False :: Bool@
registerDataDecl :: DataDecl -> TcM [TcBindingResult]
registerDataDecl dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
      params = binderHeadParams (dataDeclHead dd)
      arity = length params
      tc = dataDeclTyCon tyName arity
  paramInfos <- makeParamEnv params
  tyConKind <- tyConKindFromParams paramInfos (dataDeclKind dd)
  extendTyConEnvPermanent
    tyName
    TyConInfo
      { tciName = tyName,
        tciArity = arity,
        tciTyCon = tc,
        tciKind = tyConKind
      }
  conResults <- mapM (registerDataCon tc paramInfos) (dataDeclConstructors dd)
  zonkedKind <- defaultKindMetas tyConKind
  let tyConResult = TcBindingResult tyName tyName (kindToTcType zonkedKind)
  pure (tyConResult : conResults)

dataDeclTyCon :: Text -> Int -> TyCon
dataDeclTyCon "List" 1 = TyCon "[]" 1
dataDeclTyCon name arity = TyCon name arity

-- | Register a single data constructor as a polymorphic binding.
-- Returns the binding result for the constructor.
registerDataCon :: TyCon -> [ParamInfo] -> DataConDecl -> TcM TcBindingResult
registerDataCon tc paramInfos con = case con of
  DataConAnn _ inner -> registerDataCon tc paramInfos inner
  PrefixCon _docs _ctx conName args ->
    mapM (fieldTypeTc . bangType) args >>= registerNamedDataCon (unqualifiedNameText conName)
  InfixCon _docs _ctx lhs conName rhs ->
    mapM (fieldTypeTc . bangType) [lhs, rhs] >>= registerNamedDataCon (unqualifiedNameText conName)
  RecordCon _docs _ctx conName fields ->
    mapM (fieldTypeTc . bangType . fieldType) fields >>= registerNamedDataCon (unqualifiedNameText conName)
  TupleCon _docs _ctx flavor fields ->
    mapM (fieldTypeTc . bangType) fields >>= registerNamedDataCon (tupleConText flavor (length fields))
  UnboxedSumCon _docs _ctx pos arity field ->
    fieldTypeTc (bangType field) >>= \fieldTy -> registerNamedDataCon (unboxedSumConText pos arity) [fieldTy]
  ListCon {} ->
    registerNamedDataCon "[]" []
  GadtCon _forallBinders _ctx names body ->
    do
      let resultSurfTy = gadtBodyResultType body
          argSurfTys = gadtBodyArgTypes body
      gadtResTy <- checkSurfaceType paramEnv resultSurfTy KType
      gadtArgTys <- mapM (\argTy -> checkSurfaceType paramEnv argTy KType) argSurfTys
      let conTy = foldr TcFunTy gadtResTy gadtArgTys
          gadtScheme = ForAll [] [] conTy
      mapM_
        ( \n -> do
            let nm = unqualifiedNameText n
            extendTermEnvPermanent nm (TcIdBinder nm gadtScheme Closed)
            markGadtCon nm
        )
        names
      case names of
        (n : _) -> do
          zonkedTy <- zonkType conTy
          let name = unqualifiedNameText n
           in pure (TcBindingResult name name zonkedTy)
        [] -> pure (TcBindingResult "<gadt>" "<gadt>" gadtResTy)
  where
    paramEnv =
      Map.fromList
        [ (paramName param, (paramTyVar param, paramKind param))
        | param <- paramInfos
        ]
    paramVarIds = map paramTyVar paramInfos
    resTy = TcTyCon tc (map TcTyVar paramVarIds)
    conScheme argTys = ForAll paramVarIds [] (foldr TcFunTy resTy argTys)

    fieldTypeTc ty = checkSurfaceType paramEnv ty KType

    registerNamedDataCon name argTys = do
      let conTy = foldr TcFunTy resTy argTys
          scheme = conScheme argTys
      extendTermEnvPermanent name (TcIdBinder name scheme Closed)
      zonkedTy <- zonkType conTy
      pure (TcBindingResult name name zonkedTy)

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

unboxedSumConText :: Int -> Int -> Text
unboxedSumConText pos arity = "(#" <> bars (pos - 1) <> "_" <> bars (arity - pos) <> "#)"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = mconcat (replicate (n - 1) ",")

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")

-- | Extract argument types from a GadtBody.
gadtBodyArgTypes :: GadtBody -> [Type]
gadtBodyArgTypes (GadtPrefixBody argsWithKinds _) = map (bangType . fst) argsWithKinds
gadtBodyArgTypes _ = []

-- | Type-check a declaration, returning binding results for value bindings.
tcDecl :: Decl -> TcM [TcBindingResult]
tcDecl (DeclValue vd) = tcValueDecl vd
tcDecl (DeclAnn _ inner) = tcDecl inner
tcDecl _ = pure []

-- | Type-check a value declaration.
tcValueDecl :: ValueDecl -> TcM [TcBindingResult]
tcValueDecl (FunctionBind binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  tcFunctionInfer displayName name matches
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of
  -- Bare variable pattern (e.g. @x = 5@, @(.>.) = (++)@): type-check as a
  -- zero-argument function so that the binding gets generalized and registered
  -- in the environment.
  Just (displayName, name) -> do
    tcFunctionInfer displayName name [zeroArgMatch rhs]
  -- Non-trivial pattern binding: infer the RHS type without generalization.
  Nothing -> do
    ty <- tcRhs rhs
    zonkedTy <- zonkType ty
    pure [TcBindingResult "<pattern>" "<pattern>" zonkedTy]

-- | Extract the binder name from a pattern binding's LHS, if it is a bare
-- variable pattern.  Returns @(displayName, envName)@ for simple variable
-- patterns (possibly wrapped in parens or annotations), 'Nothing' for
-- non-trivial patterns like tuples or constructors.
patternBinderName :: Pattern -> Maybe (Text, Text)
patternBinderName (PVar n) = Just (renderBinderName n, unqualifiedNameText n)
patternBinderName (PParen inner) = patternBinderName inner
patternBinderName (PAnn _ inner) = patternBinderName inner
patternBinderName _ = Nothing

zeroArgMatch :: Rhs Expr -> Match
zeroArgMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }

-- | Convert a type scheme to a displayable type.
schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs
schemeToType (ForAll [] preds ty) = TcQualTy preds ty
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs

-- | Type-check a list of matches (equations for a function binding).
--
-- All equations must have the same number of patterns and produce
-- a consistent function type. We infer the type from each equation
-- and unify them.
tcMatches :: [Match] -> TcM (TcType, [Ct], [Implication])
tcMatches [] = do
  ty <- freshMetaTv
  pure (ty, [], [])
tcMatches matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      -- No patterns: just infer the RHS of the first match.
      (ty0, cts0) <- inferRhsExpr (matchRhs m0)
      restCts <- concatMapM (unifyMatchRhs ty0) (drop 1 matches)
      pure (ty0, cts0 ++ restCts, [])
    else do
      -- Create fresh meta-variables for the argument types and result type.
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      -- Process each equation.
      results <- mapM (tcMatchEquation argTys resTy) matches
      let (ctsList, implsList) = unzip results
          allCts = concat ctsList
          allImpls = concat implsList
          funTy = foldr TcFunTy resTy argTys
      pure (funTy, allCts, allImpls)

-- | Type-check a single match equation against expected arg/result types.
-- Returns flat wanted constraints and implication constraints.
tcMatchEquation :: [TcType] -> TcType -> Match -> TcM ([Ct], [Implication])
tcMatchEquation argTys resTy match = do
  let pats = matchPats match
      sp = sourceSpanFromAnns (matchAnns match)
  -- Bind pattern variables with their corresponding arg types.
  let bindings = concatMap extractPatternBindings (zip pats argTys)
  -- Collect pattern constraints, separating GADT givens from regular wanteds.
  (wantedPatCts, givenCts) <-
    unzipPair . concat <$> zipWithM (inferPatConstraints sp) pats argTys
  -- Infer the RHS under the extended environment.
  (rhsTy, rhsCts) <- withPatBindings bindings (inferRhsExpr (matchRhs match))
  -- RHS type must match the expected result type.
  ev <- freshEvVar
  let resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  let bodyWanteds = wantedPatCts ++ rhsCts ++ [resCt]
  if null givenCts
    then -- No GADT givens: emit everything as flat wanteds.
      pure (bodyWanteds, [])
    else do
      -- GADT givens: wrap body wanteds in an implication.
      level <- getTcLevel
      let impl =
            Implication
              { implSkols = [],
                implGivenEvs = map ctEvVar givenCts,
                implGivenCts = givenCts,
                implWantedCts = bodyWanteds,
                implTcLevel = level,
                implInfo = AppOrigin sp
              }
      pure ([], [impl])

-- | Unzip a list of pairs of constraint lists.
unzipPair :: [([Ct], [Ct])] -> ([Ct], [Ct])
unzipPair pairs =
  let (as, bs) = unzip pairs
   in (concat as, concat bs)

-- | Infer constraints from a single pattern, separated into regular wanted
-- constraints (for non-GADT constructors) and given constraints (for GADT
-- constructors that refine the scrutinee's type parameters).
--
-- Returns @(wantedCts, givenCts)@.
inferPatConstraints :: SourceSpan -> Pattern -> TcType -> TcM [([Ct], [Ct])]
inferPatConstraints sp pat scrutTy = case pat of
  PCon name _typeArgs _subPats -> do
    let conName = patNameToText name
    mBinder <- lookupTerm conName
    case mBinder of
      Just (TcIdBinder _ scheme _) -> do
        (conTy, _preds) <- instantiateSch scheme
        let conResTy = resultType conTy
        ev <- freshEvVar
        gadtCon <- isGadtCon conName
        if gadtCon
          then do
            -- GADT constructor: emit the scrutinee equality as a GIVEN.
            let givenCt =
                  Ct
                    { ctPred = EqPred scrutTy conResTy,
                      ctFlavor = Given,
                      ctEvVar = ev,
                      ctOrigin = AppOrigin sp,
                      ctLoc = sp
                    }
            pure [([], [givenCt])]
          else do
            -- Regular constructor: emit as a WANTED constraint.
            let wantedCt = mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp
            pure [([wantedCt], [])]
      _ -> pure [([], [])]
  PAnn _ann inner -> inferPatConstraints sp inner scrutTy
  PParen inner -> inferPatConstraints sp inner scrutTy
  PStrict inner -> inferPatConstraints sp inner scrutTy
  PIrrefutable inner -> inferPatConstraints sp inner scrutTy
  _ -> pure [([], [])]

-- | Unify an additional match equation's RHS with the expected type.
unifyMatchRhs :: TcType -> Match -> TcM [Ct]
unifyMatchRhs expectedTy match = do
  (rhsTy, rhsCts) <- inferRhsExpr (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin sp) sp
  pure (rhsCts ++ [eqCt])

-- | Convert a Name to Text for lookup.
patNameToText :: Name -> Text
patNameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Extract the result type from a (possibly nested) function type.
resultType :: TcType -> TcType
resultType (TcFunTy _ res) = resultType res
resultType ty = ty

-- | Instantiate a type scheme (delegating to the Instantiate module).
instantiateSch :: TypeScheme -> TcM (TcType, [Pred])
instantiateSch = Aihc.Tc.Instantiate.instantiate

-- | Extract variable bindings from a pattern paired with its expected type.
extractPatternBindings :: (Pattern, TcType) -> [(Text, TcType)]
extractPatternBindings (pat, ty) = case pat of
  PVar uname -> [(unqualifiedNameText uname, ty)]
  PAnn _ann inner -> extractPatternBindings (inner, ty)
  PParen inner -> extractPatternBindings (inner, ty)
  PWildcard {} -> []
  PLit {} -> []
  PNegLit {} -> []
  PAs name inner -> (unqualifiedNameText name, ty) : extractPatternBindings (inner, ty)
  PStrict inner -> extractPatternBindings (inner, ty)
  PIrrefutable inner -> extractPatternBindings (inner, ty)
  PCon _name _typeArgs subPats ->
    concatMap (\p -> extractPatternBindings (p, ty)) subPats
  PInfix lhs op rhs
    | nameText op == ":" ->
        let elemTy = listElementType ty
         in extractPatternBindings (lhs, elemTy) ++ extractPatternBindings (rhs, ty)
    | otherwise ->
        extractPatternBindings (lhs, ty) ++ extractPatternBindings (rhs, ty)
  _ -> []

listElementType :: TcType -> TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> elemTy
    _ -> ty

-- | Run a computation with pattern bindings in scope.
withPatBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatBindings [] m = m
withPatBindings ((name, ty) : rest) m =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatBindings rest m)

-- | Infer the type of a right-hand side expression.
inferRhsExpr :: Rhs Expr -> TcM (TcType, [Ct])
inferRhsExpr = inferRhsWithLocals inferExpr

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs Expr -> TcM TcType
tcRhs rhs = do
  (ty, cts) <- inferRhsWithLocals inferExpr rhs
  _ <- solveConstraints cts
  pure ty

-- | Render an unqualified name for display.
-- Operators (NameVarSym, NameConSym) are wrapped in parentheses.
renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

-- | Strict 'concatMap' in a monad.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
