{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for declarations.
--
-- Processes top-level data declarations and value bindings from a module.
module Aihc.Tc.Generate.Decl
  ( tcModule,
    moduleBindings,
    TcBindingResult (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BangType (..),
    BinderHead (..),
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
    GuardQualifier (..),
    GuardedRhs (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    LambdaCaseAlt (..),
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
    mkUnqualifiedName,
    nameText,
    peelClassDeclItemAnn,
    peelDeclAnn,
    tyVarBinderName,
  )
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcBindingAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    annotateDecl,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Env (InstanceInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcDiagnostic)
import Aihc.Tc.Generalize (generalizeIgnoring)
import Aihc.Tc.Generate.Annotate (annotatePatternBindingWithReport)
import Aihc.Tc.Generate.Expr (attachRhsFailure, inferRhs)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkSurfaceType, convertSurfaceType, defaultKindMetas, freeTypeVars, freshKindMeta, kindToTcType, makeParamEnv, sigToScheme, surfacePredToPred, tyConKindFromParams)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints, solveWithImpls)
import Aihc.Tc.Solve.Dict (solveDictWithGivens)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (foldM, unless, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
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

data UserSig = UserSig
  { userSigName :: !Text,
    userSigType :: !Type,
    userSigSpan :: !SourceSpan
  }
  deriving (Show)

data CheckedSig = CheckedSig
  { checkedSigName :: !Text,
    checkedSigScheme :: !TypeScheme,
    checkedSigSpan :: !SourceSpan,
    checkedSigDiagnostics :: ![TcDiagnostic]
  }
  deriving (Show)

moduleBindings :: Module -> [TcBindingResult]
moduleBindings modu =
  concatMap declBindings (moduleDecls modu)

declBindings :: Decl -> [TcBindingResult]
declBindings decl =
  case decl of
    DeclAnn ann inner ->
      annotationBindings ann inner <> declBindings inner
    DeclData dataDecl ->
      concatMap dataConBindings (dataDeclConstructors dataDecl)
    _ -> []

annotationBindings :: Annotation -> Decl -> [TcBindingResult]
annotationBindings ann decl =
  tcAnnotationBindings ann decl
    <> classAnnotationBindings ann decl
    <> instanceAnnotationBindings ann

tcAnnotationBindings :: Annotation -> Decl -> [TcBindingResult]
tcAnnotationBindings ann decl =
  case fromAnnotation ann of
    Nothing -> []
    Just tcAnn ->
      case decl of
        DeclValue valueDecl ->
          [ TcBindingResult name displayName (tcAnnType tcAnn)
          | (name, displayName) <- valueDeclBindingNames valueDecl
          ]
        DeclData dataDecl ->
          let name = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
           in [TcBindingResult name name (tcAnnType tcAnn)]
        DeclForeign foreignDecl ->
          let name = unqualifiedNameText (foreignName foreignDecl)
              displayName = renderBinderName (foreignName foreignDecl)
           in [TcBindingResult name displayName (tcAnnType tcAnn)]
        _ -> []

classAnnotationBindings :: Annotation -> Decl -> [TcBindingResult]
classAnnotationBindings ann decl =
  case (fromAnnotation ann, decl) of
    (Just (TcClassAnnotation methods), DeclClass {}) ->
      [ TcBindingResult (tcClassMethodName method) (tcClassMethodName method) (tcClassMethodType method)
      | method <- methods
      ]
    _ -> []

instanceAnnotationBindings :: Annotation -> [TcBindingResult]
instanceAnnotationBindings ann =
  case fromAnnotation ann of
    Just instAnn ->
      [TcBindingResult (tcInstanceDictName instAnn) (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)]
    Nothing -> []

dataConBindings :: DataConDecl -> [TcBindingResult]
dataConBindings dataConDecl =
  case dataConDecl of
    DataConAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn ->
          [ TcBindingResult name displayName (tcAnnType tcAnn)
          | (name, displayName) <- dataConBindingNames inner
          ]
        Nothing -> dataConBindings inner
    _ -> []

valueDeclBindingNames :: ValueDecl -> [(Text, Text)]
valueDeclBindingNames valueDecl =
  case valueDecl of
    FunctionBind binder _ -> [binderBindingName binder]
    PatternBind _ pat _ -> patternBindingNames pat

patternBindingNames :: Pattern -> [(Text, Text)]
patternBindingNames pat =
  case pat of
    PVar name -> [binderBindingName name]
    PAnn _ inner -> patternBindingNames inner
    PParen inner -> patternBindingNames inner
    PAs name inner -> binderBindingName name : patternBindingNames inner
    PStrict inner -> patternBindingNames inner
    PIrrefutable inner -> patternBindingNames inner
    PInfix lhs _ rhs -> patternBindingNames lhs <> patternBindingNames rhs
    PCon _ _ pats -> concatMap patternBindingNames pats
    _ -> []

dataConBindingNames :: DataConDecl -> [(Text, Text)]
dataConBindingNames dataConDecl =
  case dataConDecl of
    DataConAnn _ inner -> dataConBindingNames inner
    PrefixCon _ _ name _ -> [dataConBindingName name]
    InfixCon _ _ _ name _ -> [dataConBindingName name]
    RecordCon _ _ name _ -> [dataConBindingName name]
    GadtCon _ _ names _ -> map dataConBindingName names
    TupleCon _ _ flavor fields ->
      let name = tupleConText flavor (length fields)
       in [(name, name)]
    UnboxedSumCon _ _ pos arity _ ->
      let name = unboxedSumConText pos arity
       in [(name, name)]
    ListCon {} -> [("[]", "[]")]

binderBindingName :: UnqualifiedName -> (Text, Text)
binderBindingName name =
  (unqualifiedNameText name, renderBinderName name)

dataConBindingName :: UnqualifiedName -> (Text, Text)
dataConBindingName name =
  let raw = unqualifiedNameText name
   in (raw, raw)

-- | Type-check a module, returning the same syntax tree annotated with the
-- inferred interface. Call 'moduleBindings' when a flat compatibility view is
-- needed by older callers.
tcModule :: Module -> TcM Module
tcModule m = do
  -- Phase 1: collect data declarations, register constructors,
  --          and report their types.
  mapM_ registerDecl (moduleDecls m)
  -- Phase 2: collect type signatures and convert them to schemes.
  let rawSigs = collectUserSigs (moduleDecls m)
  schemes <- traverse checkUserSig rawSigs
  -- Phase 3: group and type-check value bindings using signatures.
  let grouped = sortDeclGroups (groupValueDecls (moduleDecls m))
  groupResults <- mapM (tcDeclGroup schemes) grouped
  let declUpdates = signatureDiagnosticUpdates schemes <> mconcat (map tcDeclGroupUpdates groupResults)
  mWithValueDiagnostics <- applyDeclUpdates declUpdates m
  -- Phase 4: type-check instance method bodies. They are not top-level
  -- value bindings, but their occurrences still need the same instantiation
  -- and evidence records as ordinary expressions.
  instanceDecls <- mapM tcInstanceDeclBodies (moduleDecls mWithValueDiagnostics)
  let mWithDiagnostics = mWithValueDiagnostics {moduleDecls = instanceDecls}
  annotateModuleTc mWithDiagnostics

annotateModuleTc :: Module -> TcM Module
annotateModuleTc m = do
  let classMethods = collectClassMethodNames (moduleDecls m)
  decls <- mapM (annotateDeclTc classMethods) (moduleDecls m)
  pure (m {moduleDecls = decls})

annotateDeclTc :: Map Text [Text] -> Decl -> TcM Decl
annotateDeclTc classMethods decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateDeclTc classMethods inner
    DeclValue {} -> pure decl
    DeclData dataDecl -> annotateDataDeclTc dataDecl
    DeclForeign foreignDecl
      | isForeignImport foreignDecl -> annotateForeignDeclTc foreignDecl
    DeclClass classDecl -> annotateClassDeclTc classDecl
    DeclInstance instanceDecl -> annotateInstanceDeclTc classMethods instanceDecl
    _ -> pure decl

annotateClassDeclTc :: ClassDecl -> TcM Decl
annotateClassDeclTc classDecl = do
  methods <- zipWithM annotateClassMethod [0 :: Int ..] (classDeclMethodNames classDecl)
  let methodTypes = Map.fromList [(tcClassMethodName method, tcClassMethodType method) | method <- methods]
      items = map (annotateClassItemBindings methodTypes) (classDeclItems classDecl)
  pure (DeclAnn (mkAnnotation (TcClassAnnotation methods)) (DeclClass (classDecl {classDeclItems = items})))

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

annotateDataDeclTc :: DataDecl -> TcM Decl
annotateDataDeclTc dataDecl = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
  ty <- tyConBindingType tyName
  constructors <- mapM annotateDataConDeclTc (dataDeclConstructors dataDecl)
  pure (annotateTypedDecl (TcAnnotation ty [] [] []) (DeclData (dataDecl {dataDeclConstructors = constructors})))

annotateDataConDeclTc :: DataConDecl -> TcM DataConDecl
annotateDataConDeclTc dataConDecl = do
  case dataConBindingNames dataConDecl of
    [] -> pure dataConDecl
    (name, _) : _ -> do
      ty <- dataConBindingType name
      pure (annotateDataConBindings ty (DataConAnn (mkAnnotation (TcAnnotation ty [] [] [])) dataConDecl))

dataConBindingType :: Text -> TcM TcType
dataConBindingType name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ (ForAll _ _ body) _) -> zonkType body
    Just (TcMonoIdBinder _ ty) -> zonkType ty
    Nothing -> missingTypeInfo ("data constructor " <> T.unpack name)

annotateForeignDeclTc :: ForeignDecl -> TcM Decl
annotateForeignDeclTc foreignDecl = do
  ty <- bindingType (unqualifiedNameText (foreignName foreignDecl))
  pure (annotateTypedDecl (TcAnnotation ty [] [] []) (DeclForeign foreignDecl))

annotateTypedDecl :: TcAnnotation -> Decl -> Decl
annotateTypedDecl tcAnn decl =
  annotateDecl tcAnn (annotateDeclBindings (tcAnnType tcAnn) decl)

annotateDeclBindings :: TcType -> Decl -> Decl
annotateDeclBindings ty decl =
  case decl of
    DeclValue valueDecl -> DeclValue (annotateValueDeclBindings ty valueDecl)
    DeclData dataDecl ->
      DeclData (dataDecl {dataDeclHead = annotateBinderHead (annotateBindingName ty) (dataDeclHead dataDecl)})
    DeclForeign foreignDecl ->
      DeclForeign (foreignDecl {foreignName = annotateBindingName ty (foreignName foreignDecl)})
    _ -> decl

annotateValueDeclBindings :: TcType -> ValueDecl -> ValueDecl
annotateValueDeclBindings ty valueDecl =
  case valueDecl of
    FunctionBind name matches ->
      FunctionBind (annotateBindingName ty name) matches
    PatternBind multiplicity pat rhs ->
      PatternBind multiplicity (annotatePatternBinding ty pat) rhs

annotateBinderHead :: (UnqualifiedName -> UnqualifiedName) -> BinderHead UnqualifiedName -> BinderHead UnqualifiedName
annotateBinderHead f head' =
  case head' of
    PrefixBinderHead name params -> PrefixBinderHead (f name) params
    InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (f name) rhs params

annotateBindingName :: TcType -> UnqualifiedName -> UnqualifiedName
annotateBindingName ty name =
  name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation (TcBindingAnnotation name ty)]}

syntheticBindingAnnotation :: Text -> TcType -> TcBindingAnnotation
syntheticBindingAnnotation name =
  TcBindingAnnotation (mkUnqualifiedName NameVarId name)

annotateDataConBindings :: TcType -> DataConDecl -> DataConDecl
annotateDataConBindings ty dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> DataConAnn ann (annotateDataConBindings ty inner)
    PrefixCon foralls context name fields ->
      PrefixCon foralls context (annotateBindingName ty name) fields
    InfixCon foralls context lhs name rhs ->
      InfixCon foralls context lhs (annotateBindingName ty name) rhs
    RecordCon foralls context name fields ->
      RecordCon foralls context (annotateBindingName ty name) fields
    GadtCon foralls context names body ->
      GadtCon foralls context (map annotateDataConName names) body
    TupleCon {} -> dataConDecl
    UnboxedSumCon {} -> dataConDecl
    ListCon {} -> dataConDecl
  where
    annotateDataConName =
      annotateBindingName ty

annotateClassItemBindings :: Map Text TcType -> ClassDeclItem -> ClassDeclItem
annotateClassItemBindings methodTypes item =
  case item of
    ClassItemAnn ann inner -> ClassItemAnn ann (annotateClassItemBindings methodTypes inner)
    ClassItemTypeSig names _ ->
      case mapM annotateMethod names of
        Just names' -> replaceClassItemNames names' item
        Nothing -> item
    ClassItemDefaultSig name _ ->
      case annotateMethod name of
        Just name' -> replaceClassItemNames [name'] item
        Nothing -> item
    ClassItemDefault valueDecl ->
      ClassItemDefault (annotateClassDefaultValue methodTypes valueDecl)
    _ -> item
  where
    annotateMethod name = do
      ty <- Map.lookup (unqualifiedNameText name) methodTypes
      pure (annotateBindingName ty name)

replaceClassItemNames :: [UnqualifiedName] -> ClassDeclItem -> ClassDeclItem
replaceClassItemNames names item =
  case item of
    ClassItemTypeSig _ ty -> ClassItemTypeSig names ty
    ClassItemDefaultSig _ ty ->
      case names of
        name : _ -> ClassItemDefaultSig name ty
        [] -> item
    _ -> item

annotateClassDefaultValue :: Map Text TcType -> ValueDecl -> ValueDecl
annotateClassDefaultValue methodTypes valueDecl =
  case valueDecl of
    FunctionBind name matches ->
      case Map.lookup (unqualifiedNameText name) methodTypes of
        Just ty -> FunctionBind (annotateBindingName ty name) matches
        Nothing -> valueDecl
    PatternBind multiplicity pat rhs ->
      case patternBinderName pat of
        Just (_, methodName) ->
          case Map.lookup methodName methodTypes of
            Just ty -> PatternBind multiplicity (annotatePatternBinding ty pat) rhs
            Nothing -> valueDecl
        Nothing -> valueDecl

unqualifiedNameSpan :: UnqualifiedName -> SourceSpan
unqualifiedNameSpan =
  sourceSpanFromAnns . unqualifiedNameAnns

tyConBindingType :: Text -> TcM TcType
tyConBindingType name = do
  mInfo <- lookupTyCon name
  case mInfo of
    Just info -> kindToTcType <$> defaultKindMetas (tciKind info)
    Nothing -> missingTypeInfo ("type constructor " <> T.unpack name)

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
          annotatedHead = annotateInstanceHeadBinding dictName dictTy (instanceDeclHead instanceDecl)
          instAnn =
            TcInstanceAnnotation
              { tcInstanceDictName = dictName,
                tcInstanceDictType = dictTy,
                tcInstanceTyVars = tvIds,
                tcInstanceHeadTypes = headTys,
                tcInstanceContextDicts = contextDicts,
                tcInstanceMethodOrder = methodOrder
              }
      items <- mapM (annotateInstanceItemTc headTys) (instanceDeclItems instanceDecl)
      pure (DeclAnn (mkAnnotation instAnn) (DeclInstance (instanceDecl {instanceDeclHead = annotatedHead, instanceDeclItems = items})))

annotateInstanceHeadBinding :: Text -> TcType -> Type -> Type
annotateInstanceHeadBinding dictName dictTy =
  go
  where
    dictAnn = syntheticBindingAnnotation dictName dictTy

    go ty =
      case ty of
        TAnn ann inner -> TAnn ann (go inner)
        TApp fun arg -> TApp (go fun) arg
        TTypeApp fun arg -> TTypeApp (go fun) arg
        TParen inner -> TParen (go inner)
        TCon name promoted -> TCon (annotateNameBinding dictAnn name) promoted
        TInfix lhs name promoted rhs -> TInfix lhs (annotateNameBinding dictAnn name) promoted rhs
        _ -> ty

annotateNameBinding :: TcBindingAnnotation -> Name -> Name
annotateNameBinding ann name =
  name {nameAnns = nameAnns name <> [mkAnnotation ann]}

annotateInstanceItemTc :: [TcType] -> InstanceDeclItem -> TcM InstanceDeclItem
annotateInstanceItemTc headTys item =
  case item of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> annotateInstanceItemTc headTys inner
    InstanceItemBind (FunctionBind name matches) -> do
      let methodName = unqualifiedNameText name
      methodTy <- methodExpectedType headTys (unqualifiedNameText name)
      let name' = annotateBindingName methodTy name
      pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) (InstanceItemBind (FunctionBind name' matches)))
    InstanceItemBind (PatternBind anns pat rhs) ->
      case patternBinderName pat of
        Just (_, methodName) -> do
          methodTy <- methodExpectedType headTys methodName
          let pat' = annotateInstancePatternMethod methodName methodTy pat
          pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) (InstanceItemBind (PatternBind anns pat' rhs)))
        Nothing -> pure item
    _ -> pure item

annotateInstancePatternMethod :: Text -> TcType -> Pattern -> Pattern
annotateInstancePatternMethod methodName ty pat =
  case patternBinderName pat of
    Just (displayName, _)
      | displayName == methodName -> annotatePatternBinding ty pat
      | otherwise -> pat
    Nothing -> pat

tcInstanceDeclBodies :: Decl -> TcM Decl
tcInstanceDeclBodies (DeclAnn ann inner) =
  DeclAnn ann <$> tcInstanceDeclBodies inner
tcInstanceDeclBodies (DeclInstance instanceDecl) =
  case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of
    (_, []) -> pure (DeclInstance instanceDecl)
    (Nothing, _) -> pure (DeclInstance instanceDecl)
    (Just _, headArgTypes) -> do
      let explicitTyVars = map tyVarBinderName (instanceDeclForall instanceDecl)
          freeVars = nub (explicitTyVars <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> headArgTypes))
      tvIds <- mapM freshSkolemTv freeVars
      let tvMap = Map.fromList (zip freeVars tvIds)
      headTys <- mapM (convertSurfaceType tvMap) headArgTypes
      givens <- mapM (surfacePredToPred (simpleTvKindEnv tvMap)) (instanceDeclContext instanceDecl)
      items <- mapM (tcInstanceItemBody givens headTys) (instanceDeclItems instanceDecl)
      pure (DeclInstance (instanceDecl {instanceDeclItems = items}))
tcInstanceDeclBodies decl =
  pure decl

tcInstanceItemBody :: [Pred] -> [TcType] -> InstanceDeclItem -> TcM InstanceDeclItem
tcInstanceItemBody givens headTys item =
  case item of
    InstanceItemAnn ann inner ->
      InstanceItemAnn ann <$> tcInstanceItemBody givens headTys inner
    InstanceItemBind (FunctionBind name matches) -> do
      methodTy <- methodExpectedType headTys (unqualifiedNameText name)
      let (argTys, resTy) = splitFunTy methodTy (matchArity matches)
      matches' <-
        solveCircularWith (solveBodyConstraintsWithGivens givens) $ \report -> do
          results <- mapM (tcMatchEquation report Nothing argTys resTy) matches
          let (matchesList, ctsList, implsList) = unzip3 results
          pure (matchesList, concat ctsList, concat implsList)
      pure (InstanceItemBind (FunctionBind name matches'))
    InstanceItemBind (PatternBind multiplicity pat rhs) ->
      case patternBinderName pat of
        Just (_, methodName) -> do
          methodTy <- methodExpectedType headTys methodName
          matches' <-
            solveCircularWith (solveBodyConstraintsWithGivens givens) $ \report -> do
              result <- tcMatchEquation report Nothing [] methodTy (zeroArgMatch (patternSpan pat) rhs)
              let (match', cts, impls) = result
              pure ([match'], cts, impls)
          case matches' of
            match' : _ ->
              pure (InstanceItemBind (PatternBind multiplicity pat (matchRhs match')))
            [] -> pure item
        Nothing -> pure item
    _ -> pure item

matchArity :: [Match] -> Int
matchArity (match : _) = length (matchPats match)
matchArity [] = 0

solveBodyConstraintsWithGivens :: [Pred] -> [Ct] -> [Implication] -> TcM ()
solveBodyConstraintsWithGivens givens cts impls = do
  _ <- solveWithImpls cts impls
  mapM_ solveClassCt cts
  where
    solveClassCt ct@Ct {ctPred = ClassPred {}} = do
      _ <- solveDictWithGivens givens ct
      pure ()
    solveClassCt _ = pure ()

annotatePatternBinding :: TcType -> Pattern -> Pattern
annotatePatternBinding ty pat =
  case pat of
    PAnn ann inner -> PAnn ann (annotatePatternBinding ty inner)
    PVar name -> PVar (annotateBindingName ty name)
    PParen inner -> PParen (annotatePatternBinding ty inner)
    PAs name inner -> PAs (annotateBindingName ty name) (annotatePatternBinding ty inner)
    PStrict inner -> PStrict (annotatePatternBinding ty inner)
    PIrrefutable inner -> PIrrefutable (annotatePatternBinding ty inner)
    PList items ->
      case listElementType ty of
        Just elemTy -> PList (map (annotatePatternBinding elemTy) items)
        Nothing -> pat
    PTuple flavor items -> PTuple flavor (zipWith annotatePatternBinding (tupleElementTypes ty) items)
    PUnboxedSum alt arity inner -> PUnboxedSum alt arity (annotatePatternBinding ty inner)
    PInfix lhs op rhs
      | nameText op == ":" ->
          case listElementType ty of
            Just elemTy -> PInfix (annotatePatternBinding elemTy lhs) op (annotatePatternBinding ty rhs)
            Nothing -> pat
      | otherwise -> pat
    PView expr inner -> PView expr (annotatePatternBinding ty inner)
    PTypeSig inner sigTy -> PTypeSig (annotatePatternBinding ty inner) sigTy
    _ -> pat

listElementType :: TcType -> Maybe TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> Just elemTy
    _ -> Nothing

tupleElementTypes :: TcType -> [TcType]
tupleElementTypes ty =
  case ty of
    TcTyCon (TyCon _ arity) elemTys
      | arity == length elemTys -> elemTys
    _ -> []

bindingType :: Text -> TcM TcType
bindingType name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just binder -> pure (binderType binder)
    Nothing -> missingTypeInfo ("binding " <> T.unpack name)

binderType :: TcBinder -> TcType
binderType (TcIdBinder _ scheme _) = schemeToType scheme
binderType (TcMonoIdBinder _ ty) = ty

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

firstClassPredSubst :: [Pred] -> [TcType] -> Maybe (Map Unique TcType)
firstClassPredSubst preds headTys =
  case [classArgs | ClassPred _ classArgs <- preds] of
    classArgs : _ -> matchTypes classArgs headTys
    [] -> Nothing

missingTypeInfo :: String -> TcM a
missingTypeInfo msg =
  abortTc ("internal type annotation error: missing " <> msg)

selectorDictTypeTc :: Text -> TcType -> TcM TcType
selectorDictTypeTc methodName methodTy =
  case snd (peelForAlls methodTy) of
    TcQualTy (pred' : _) _ -> pure (predType pred')
    _ -> missingTypeInfo ("class dictionary type for method selector " <> T.unpack methodName)

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

-- | Collect type signatures from a list of declarations.
collectUserSigs :: [Decl] -> Map Text UserSig
collectUserSigs decls = Map.fromList $ concatMap (extractSig NoSourceSpan) decls
  where
    extractSig ambient (DeclTypeSig names ty) =
      [ (name, UserSig name ty sigSp)
      | n <- names,
        let name = unqualifiedNameText n,
        let sigSp = ambient `orSourceSpan` unqualifiedNameSpan n `orSourceSpan` typeSpan ty
      ]
    extractSig ambient (DeclForeign foreignDecl)
      | isForeignImport foreignDecl =
          let name = unqualifiedNameText (foreignName foreignDecl)
              sigSp = ambient `orSourceSpan` unqualifiedNameSpan (foreignName foreignDecl) `orSourceSpan` typeSpan (foreignType foreignDecl)
           in [(name, UserSig name (foreignType foreignDecl) sigSp)]
    extractSig ambient (DeclAnn ann inner) =
      extractSig (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
    extractSig _ _ = []

checkUserSig :: UserSig -> TcM CheckedSig
checkUserSig userSig = do
  (scheme, diagnostics) <- captureDiagnostics (sigToScheme (userSigType userSig))
  pure
    CheckedSig
      { checkedSigName = userSigName userSig,
        checkedSigScheme = scheme,
        checkedSigSpan = userSigSpan userSig,
        checkedSigDiagnostics = diagnostics
      }

captureDiagnostics :: TcM a -> TcM (a, [TcDiagnostic])
captureDiagnostics action = do
  before <- lift get
  result <- action
  after <- lift get
  let newCount = length (tcsDiagnostics after) - length (tcsDiagnostics before)
      diagnostics = reverse (take newCount (tcsDiagnostics after))
  pure (result, diagnostics)

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

simpleTvKindEnv :: Map Text TyVarId -> TvKindEnv
simpleTvKindEnv = Map.map (,KType)

-- | Instantiate a type scheme with fresh skolems for type-checking while
-- preserving the scheme predicates as scoped givens for the checked body.
-- Unlike regular instantiation (which uses metas), this produces rigid
-- type variables that cannot be unified during constraint solving.
skolemizeQualified :: TypeScheme -> TcM ([Pred], TcType)
skolemizeQualified (ForAll tvs preds body) = do
  subst <- Map.fromList <$> mapM mkSubst tvs
  pure (map (substPred subst) preds, substType subst body)
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

data TcDeclGroupResult = TcDeclGroupResult
  { tcDeclGroupBindings :: ![TcBindingResult],
    tcDeclGroupUpdates :: !DeclUpdates
  }

data DeclUpdates = DeclUpdates
  { declFunctionUpdates :: !(Map Text [FunctionDeclUpdate]),
    declPatternUpdates :: !(Map Text [PatternDeclUpdate]),
    declAnonymousPatternUpdates :: ![PatternDeclUpdate],
    declTypeSigUpdates :: !(Map Text [TcDiagnostic])
  }

data FunctionDeclUpdate = FunctionDeclUpdate !(Maybe TcType) ![Match]

data PatternDeclUpdate = PatternDeclUpdate !(Maybe TcType) !(Rhs Expr)

instance Semigroup DeclUpdates where
  left <> right =
    DeclUpdates
      { declFunctionUpdates = Map.unionWith (<>) (declFunctionUpdates left) (declFunctionUpdates right),
        declPatternUpdates = Map.unionWith (<>) (declPatternUpdates left) (declPatternUpdates right),
        declAnonymousPatternUpdates = declAnonymousPatternUpdates left <> declAnonymousPatternUpdates right,
        declTypeSigUpdates = Map.unionWith (<>) (declTypeSigUpdates left) (declTypeSigUpdates right)
      }

instance Monoid DeclUpdates where
  mempty =
    DeclUpdates
      { declFunctionUpdates = Map.empty,
        declPatternUpdates = Map.empty,
        declAnonymousPatternUpdates = [],
        declTypeSigUpdates = Map.empty
      }

checkedBindings :: [TcBindingResult] -> DeclUpdates -> TcDeclGroupResult
checkedBindings bindings updates =
  TcDeclGroupResult
    { tcDeclGroupBindings = bindings,
      tcDeclGroupUpdates = updates
    }

functionUpdate :: Text -> Maybe TcType -> [Match] -> DeclUpdates
functionUpdate name maybeTy matches =
  mempty {declFunctionUpdates = Map.singleton name [FunctionDeclUpdate maybeTy matches]}

patternUpdate :: Text -> Maybe TcType -> Rhs Expr -> DeclUpdates
patternUpdate name maybeTy rhs =
  mempty {declPatternUpdates = Map.singleton name [PatternDeclUpdate maybeTy rhs]}

anonymousPatternUpdate :: Rhs Expr -> DeclUpdates
anonymousPatternUpdate rhs =
  mempty {declAnonymousPatternUpdates = [PatternDeclUpdate Nothing rhs]}

signatureDiagnosticUpdates :: Map Text CheckedSig -> DeclUpdates
signatureDiagnosticUpdates sigs =
  mempty
    { declTypeSigUpdates =
        Map.fromList
          [ (checkedSigName sig, checkedSigDiagnostics sig)
          | sig <- Map.elems sigs,
            not (null (checkedSigDiagnostics sig))
          ]
    }

applyDeclUpdates :: DeclUpdates -> Module -> TcM Module
applyDeclUpdates updates modu = do
  (updates', reversedDecls) <-
    foldM
      ( \(updatesAcc, declsAcc) decl -> do
          (updatesNext, decl') <- applyDeclUpdate updatesAcc decl
          pure (updatesNext, decl' : declsAcc)
      )
      (updates, [])
      (moduleDecls modu)
  unless (nullDeclUpdates updates') $
    abortTc ("internal type annotation error: unapplied declaration diagnostics " <> showDeclUpdates updates')
  pure (modu {moduleDecls = reverse reversedDecls})

applyDeclUpdate :: DeclUpdates -> Decl -> TcM (DeclUpdates, Decl)
applyDeclUpdate updates decl =
  case decl of
    DeclAnn ann inner ->
      do
        (updates', inner') <- applyDeclUpdate updates inner
        pure (updates', DeclAnn ann inner')
    DeclValue (FunctionBind name matches) ->
      let key = unqualifiedNameText name
       in do
            maybeUpdate <- takeFunctionMatches key (length matches) updates
            case maybeUpdate of
              Nothing -> pure (updates, decl)
              Just (updates', FunctionDeclUpdate maybeTy matches') ->
                pure (updates', annotateUpdatedValueDecl maybeTy (DeclValue (FunctionBind name matches')))
    DeclValue (PatternBind multiplicity pat _rhs) ->
      case takePatternRhs pat updates of
        Nothing -> pure (updates, decl)
        Just (updates', PatternDeclUpdate maybeTy rhs') ->
          pure (updates', annotateUpdatedValueDecl maybeTy (DeclValue (PatternBind multiplicity pat rhs')))
    DeclTypeSig names _ty -> do
      let (updates', diagnostics) = takeTypeSigDiagnostics names updates
      pure (updates', foldr (DeclAnn . mkAnnotation) decl diagnostics)
    _ -> pure (updates, decl)

annotateUpdatedValueDecl :: Maybe TcType -> Decl -> Decl
annotateUpdatedValueDecl maybeTy decl =
  case maybeTy of
    Nothing -> decl
    Just ty -> annotateTypedDecl (TcAnnotation ty [] [] []) decl

takeFunctionMatches :: Text -> Int -> DeclUpdates -> TcM (Maybe (DeclUpdates, FunctionDeclUpdate))
takeFunctionMatches name count updates =
  case Map.lookup name (declFunctionUpdates updates) of
    Nothing -> pure Nothing
    Just functionUpdates ->
      case functionUpdates of
        [] -> pure Nothing
        FunctionDeclUpdate maybeTy matches : restUpdates ->
          if length matches < count
            then abortTc ("internal type annotation error: not enough updated matches for " <> T.unpack name)
            else
              let (here, remainingMatches) = splitAt count matches
                  remainingUpdate =
                    [FunctionDeclUpdate maybeTy remainingMatches | not (null remainingMatches)]
                  remaining = remainingUpdate <> restUpdates
                  updates' =
                    updates
                      { declFunctionUpdates =
                          if null remaining
                            then Map.delete name (declFunctionUpdates updates)
                            else Map.insert name remaining (declFunctionUpdates updates)
                      }
               in pure (Just (updates', FunctionDeclUpdate maybeTy here))

takePatternRhs :: Pattern -> DeclUpdates -> Maybe (DeclUpdates, PatternDeclUpdate)
takePatternRhs pat updates =
  case patternBinderName pat of
    Just (_, name) ->
      case Map.lookup name (declPatternUpdates updates) of
        Just (rhsUpdate : rest) ->
          let updates' =
                updates
                  { declPatternUpdates =
                      if null rest
                        then Map.delete name (declPatternUpdates updates)
                        else Map.insert name rest (declPatternUpdates updates)
                  }
           in Just (updates', rhsUpdate)
        _ -> Nothing
    Nothing ->
      case declAnonymousPatternUpdates updates of
        [] -> Nothing
        rhsUpdate : rest ->
          Just (updates {declAnonymousPatternUpdates = rest}, rhsUpdate)

takeTypeSigDiagnostics :: [UnqualifiedName] -> DeclUpdates -> (DeclUpdates, [TcDiagnostic])
takeTypeSigDiagnostics names updates =
  let keys = map unqualifiedNameText names
      diagnostics = concatMap (\name -> Map.findWithDefault [] name (declTypeSigUpdates updates)) keys
      updates' = updates {declTypeSigUpdates = foldr Map.delete (declTypeSigUpdates updates) keys}
   in (updates', diagnostics)

nullDeclUpdates :: DeclUpdates -> Bool
nullDeclUpdates updates =
  Map.null (declFunctionUpdates updates)
    && Map.null (declPatternUpdates updates)
    && null (declAnonymousPatternUpdates updates)
    && Map.null (declTypeSigUpdates updates)

showDeclUpdates :: DeclUpdates -> String
showDeclUpdates updates =
  "function updates="
    <> show (Map.keys (declFunctionUpdates updates))
    <> ", pattern updates="
    <> show (Map.keys (declPatternUpdates updates), length (declAnonymousPatternUpdates updates))
    <> ", type signature updates="
    <> show (Map.keys (declTypeSigUpdates updates))

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
    EVar name -> [nameToText name]
    EAnn _ inner -> freeVarsExpr inner
    EIf cond trueBranch falseBranch ->
      freeVarsExpr cond ++ freeVarsExpr trueBranch ++ freeVarsExpr falseBranch
    ELambdaPats pats body ->
      freeVarsExpr body \\ concatMap patternBinders pats
    EInfix lhs op rhs ->
      nameToText op : freeVarsExpr lhs ++ freeVarsExpr rhs
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> nameToText op : freeVarsExpr inner
    ESectionR op inner -> nameToText op : freeVarsExpr inner
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

nameToText :: Name -> Text
nameToText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name

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
tcDeclGroup :: Map Text CheckedSig -> DeclGroup -> TcM TcDeclGroupResult
tcDeclGroup sigs (SingleDecl d) =
  case peelDeclAnn d of
    DeclValue (FunctionBind binder matches) -> do
      let name = unqualifiedNameText binder
          displayName = renderBinderName binder
      case Map.lookup name sigs of
        Just sig -> do
          (bindings, matches') <- tcFunctionWithSig displayName name sig matches
          pure (checkedBindings bindings (functionUpdate name (bindingUpdateType bindings) matches'))
        Nothing -> do
          (bindings, matches') <- tcFunctionInfer displayName name matches
          pure (checkedBindings bindings (functionUpdate name (bindingUpdateType bindings) matches'))
    DeclValue (PatternBind _ pat rhs)
      | Just (displayName, name) <- patternBinderName pat,
        Just sig <- Map.lookup name sigs -> do
          (bindings, matches') <- tcFunctionWithSig displayName name sig [zeroArgMatch (patternSpan pat `orSourceSpan` peelDeclSpan NoSourceSpan d) rhs]
          let rhs' =
                case matches' of
                  match' : _ -> matchRhs match'
                  [] -> rhs
          pure (checkedBindings bindings (patternUpdate name (bindingUpdateType bindings) rhs'))
    DeclValue (PatternBind _ pat rhs)
      | Just (displayName, name) <- patternBinderName pat -> do
          (bindings, matches') <- tcFunctionInfer displayName name [zeroArgMatch (patternSpan pat `orSourceSpan` peelDeclSpan NoSourceSpan d) rhs]
          let rhs' =
                case matches' of
                  match' : _ -> matchRhs match'
                  [] -> rhs
          pure (checkedBindings bindings (patternUpdate name (bindingUpdateType bindings) rhs'))
    DeclValue (PatternBind _ _ rhs) -> do
      (bindings, rhs') <- tcPatternRhs rhs
      pure (checkedBindings bindings (anonymousPatternUpdate rhs'))
    _ -> checkedBindings <$> tcDecl d <*> pure mempty
tcDeclGroup sigs (MergedFunctionBind _sp binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  case Map.lookup name sigs of
    Just sig -> do
      -- Use the declared type signature for checking.
      (bindings, matches') <- tcFunctionWithSig displayName name sig matches
      pure (checkedBindings bindings (functionUpdate name (bindingUpdateType bindings) matches'))
    Nothing -> do
      -- No signature: infer the type.
      (bindings, matches') <- tcFunctionInfer displayName name matches
      pure (checkedBindings bindings (functionUpdate name (bindingUpdateType bindings) matches'))

bindingUpdateType :: [TcBindingResult] -> Maybe TcType
bindingUpdateType bindings =
  case bindings of
    binding : _ -> Just (tbType binding)
    [] -> Nothing

-- | Type-check a function with a known type signature.
-- The signature's type variables are opened as rigid skolems so that
-- the body is checked against them. GADT patterns generate implication
-- constraints using the signature's skolems as given equalities.
tcFunctionWithSig :: Text -> Text -> CheckedSig -> [Match] -> TcM ([TcBindingResult], [Match])
tcFunctionWithSig displayName name sig matches = do
  let scheme = checkedSigScheme sig
  (matches', failed) <-
    withErrorTracking $ do
      extendTermEnvPermanent name (TcIdBinder name scheme Closed)
      -- Open the scheme with skolems (not metas) for checking.
      (sigPreds, sigTy) <- skolemizeQualified scheme
      let nArgs = case matches of
            (m : _) -> length (matchPats m)
            [] -> 0
          (argTys, resTy) = splitFunTy sigTy nArgs
      solveCircularWith (solveBodyConstraintsWithGivens sigPreds) $ \report -> do
        results <- mapM (tcMatchEquation report (Just (TypeSignatureOrigin (checkedSigName sig) (checkedSigSpan sig))) argTys resTy) matches
        let (matchesList, ctsList, implsList) = unzip3 results
        pure (matchesList, concat ctsList, concat implsList)
  if failed
    then pure ([], map stripSuccessfulMatchAnnotations matches')
    else do
      -- Report the declared scheme as the binding's type.
      let declaredTy = schemeToType scheme
      zonkedTy <- zonkType declaredTy
      pure ([TcBindingResult name displayName zonkedTy], matches')

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: Text -> Text -> [Match] -> TcM ([TcBindingResult], [Match])
tcFunctionInfer displayName name matches = do
  placeholderTy <- freshMetaTv
  beforeErrors <- getErrorCount
  extendTermEnvPermanent name (TcMonoIdBinder name placeholderTy)
  (bindings, matches') <-
    solveCircularWithPost
      ( \cts impls -> do
          void (solveWithImpls cts impls)
      )
      ( \report -> do
          (ty, matches', cts', impls') <- tcMatches report matches
          pure ((ty, matches'), cts', impls')
      )
      ( \(ty, matches') -> do
          failed <- (> beforeErrors) <$> getErrorCount
          if failed
            then pure ([], matches')
            else do
              scheme <- generalizeIgnoring [name] ty []
              commitGeneralizedMetas ty scheme
              let schemeTy = schemeToType scheme
              zonkedTy <- zonkType schemeTy
              extendTermEnvPermanent name (TcIdBinder name scheme Closed)
              pure ([TcBindingResult name displayName zonkedTy], matches')
      )
  failed <- (> beforeErrors) <$> getErrorCount
  if failed
    then pure ([], map stripSuccessfulMatchAnnotations matches')
    else pure (bindings, matches')

tcPatternRhs :: Rhs Expr -> TcM ([TcBindingResult], Rhs Expr)
tcPatternRhs rhs = do
  ((rhs', ty), failed) <-
    withErrorTracking $
      solveCircularWithImpls $ \report -> do
        (rhs', ty, cts) <- inferRhs report rhs
        pure ((rhs', ty), cts, [])
  if failed
    then pure ([], stripSuccessfulRhsAnnotations rhs')
    else do
      zonkedTy <- zonkType ty
      pure ([TcBindingResult "<pattern>" "<pattern>" zonkedTy], rhs')

stripSuccessfulMatchAnnotations :: Match -> Match
stripSuccessfulMatchAnnotations match =
  match
    { matchPats = map stripSuccessfulPatternAnnotations (matchPats match),
      matchRhs = stripSuccessfulRhsAnnotations (matchRhs match)
    }

stripSuccessfulRhsAnnotations :: Rhs Expr -> Rhs Expr
stripSuccessfulRhsAnnotations rhs =
  case rhs of
    UnguardedRhs anns expr maybeDecls ->
      UnguardedRhs anns (stripSuccessfulExprAnnotations expr) (map stripSuccessfulDeclAnnotations <$> maybeDecls)
    GuardedRhss anns guardedRhss maybeDecls ->
      GuardedRhss
        anns
        (map stripSuccessfulGuardedRhsAnnotations guardedRhss)
        (map stripSuccessfulDeclAnnotations <$> maybeDecls)

stripSuccessfulGuardedRhsAnnotations :: GuardedRhs Expr -> GuardedRhs Expr
stripSuccessfulGuardedRhsAnnotations guardedRhs =
  guardedRhs
    { guardedRhsGuards = map stripSuccessfulGuardQualifierAnnotations (guardedRhsGuards guardedRhs),
      guardedRhsBody = stripSuccessfulExprAnnotations (guardedRhsBody guardedRhs)
    }

stripSuccessfulDeclAnnotations :: Decl -> Decl
stripSuccessfulDeclAnnotations decl =
  case decl of
    DeclAnn ann inner
      | isSuccessfulTcAnnotation ann -> stripSuccessfulDeclAnnotations inner
      | otherwise -> DeclAnn ann (stripSuccessfulDeclAnnotations inner)
    DeclValue valueDecl -> DeclValue (stripSuccessfulValueDeclAnnotations valueDecl)
    _ -> decl

stripSuccessfulValueDeclAnnotations :: ValueDecl -> ValueDecl
stripSuccessfulValueDeclAnnotations valueDecl =
  case valueDecl of
    FunctionBind name matches ->
      FunctionBind (stripSuccessfulUnqualifiedNameAnnotations name) (map stripSuccessfulMatchAnnotations matches)
    PatternBind multiplicity pat rhs ->
      PatternBind multiplicity (stripSuccessfulPatternAnnotations pat) (stripSuccessfulRhsAnnotations rhs)

stripSuccessfulExprAnnotations :: Expr -> Expr
stripSuccessfulExprAnnotations expr =
  case expr of
    EAnn ann inner
      | isSuccessfulTcAnnotation ann -> stripSuccessfulExprAnnotations inner
      | otherwise -> EAnn ann (stripSuccessfulExprAnnotations inner)
    EVar name -> EVar (stripSuccessfulNameAnnotations name)
    EIf cond thenE elseE ->
      EIf
        (stripSuccessfulExprAnnotations cond)
        (stripSuccessfulExprAnnotations thenE)
        (stripSuccessfulExprAnnotations elseE)
    ELambdaPats pats body ->
      ELambdaPats (map stripSuccessfulPatternAnnotations pats) (stripSuccessfulExprAnnotations body)
    ELambdaCase alts ->
      ELambdaCase (map stripSuccessfulCaseAltAnnotations alts)
    ELambdaCases alts ->
      ELambdaCases (map stripSuccessfulLambdaCaseAltAnnotations alts)
    EInfix lhs op rhs ->
      EInfix
        (stripSuccessfulExprAnnotations lhs)
        (stripSuccessfulNameAnnotations op)
        (stripSuccessfulExprAnnotations rhs)
    ENegate inner -> ENegate (stripSuccessfulExprAnnotations inner)
    ELetDecls decls body ->
      ELetDecls (map stripSuccessfulDeclAnnotations decls) (stripSuccessfulExprAnnotations body)
    ECase scrut alts ->
      ECase (stripSuccessfulExprAnnotations scrut) (map stripSuccessfulCaseAltAnnotations alts)
    EListComp body quals ->
      EListComp (stripSuccessfulExprAnnotations body) (map stripSuccessfulCompStmtAnnotations quals)
    ETypeSig inner ty -> ETypeSig (stripSuccessfulExprAnnotations inner) ty
    EParen inner -> EParen (stripSuccessfulExprAnnotations inner)
    EList elems -> EList (map stripSuccessfulExprAnnotations elems)
    ETuple flavor elems -> ETuple flavor (map (fmap stripSuccessfulExprAnnotations) elems)
    EApp fun arg -> EApp (stripSuccessfulExprAnnotations fun) (stripSuccessfulExprAnnotations arg)
    _ -> expr

stripSuccessfulCaseAltAnnotations :: CaseAlt Expr -> CaseAlt Expr
stripSuccessfulCaseAltAnnotations (CaseAlt anns pat rhs) =
  CaseAlt anns (stripSuccessfulPatternAnnotations pat) (stripSuccessfulRhsAnnotations rhs)

stripSuccessfulLambdaCaseAltAnnotations :: LambdaCaseAlt -> LambdaCaseAlt
stripSuccessfulLambdaCaseAltAnnotations alt =
  alt
    { lambdaCaseAltPats = map stripSuccessfulPatternAnnotations (lambdaCaseAltPats alt),
      lambdaCaseAltRhs = stripSuccessfulRhsAnnotations (lambdaCaseAltRhs alt)
    }

stripSuccessfulGuardQualifierAnnotations :: GuardQualifier -> GuardQualifier
stripSuccessfulGuardQualifierAnnotations qual =
  case qual of
    GuardAnn ann inner -> GuardAnn ann (stripSuccessfulGuardQualifierAnnotations inner)
    GuardExpr guard -> GuardExpr (stripSuccessfulExprAnnotations guard)
    GuardPat pat guard -> GuardPat (stripSuccessfulPatternAnnotations pat) (stripSuccessfulExprAnnotations guard)
    GuardLet decls -> GuardLet (map stripSuccessfulDeclAnnotations decls)

stripSuccessfulCompStmtAnnotations :: CompStmt -> CompStmt
stripSuccessfulCompStmtAnnotations stmt =
  case stmt of
    CompAnn ann inner -> CompAnn ann (stripSuccessfulCompStmtAnnotations inner)
    CompGen pat src -> CompGen (stripSuccessfulPatternAnnotations pat) (stripSuccessfulExprAnnotations src)
    CompGuard guard -> CompGuard (stripSuccessfulExprAnnotations guard)
    CompLetDecls decls -> CompLetDecls (map stripSuccessfulDeclAnnotations decls)
    CompThen expr -> CompThen (stripSuccessfulExprAnnotations expr)
    CompThenBy expr byExpr -> CompThenBy (stripSuccessfulExprAnnotations expr) (stripSuccessfulExprAnnotations byExpr)
    CompGroupUsing expr -> CompGroupUsing (stripSuccessfulExprAnnotations expr)
    CompGroupByUsing byExpr usingExpr ->
      CompGroupByUsing (stripSuccessfulExprAnnotations byExpr) (stripSuccessfulExprAnnotations usingExpr)

stripSuccessfulPatternAnnotations :: Pattern -> Pattern
stripSuccessfulPatternAnnotations pat =
  case pat of
    PAnn ann inner
      | isSuccessfulTcAnnotation ann -> stripSuccessfulPatternAnnotations inner
      | otherwise -> PAnn ann (stripSuccessfulPatternAnnotations inner)
    PVar name -> PVar (stripSuccessfulUnqualifiedNameAnnotations name)
    PTuple flavor pats -> PTuple flavor (map stripSuccessfulPatternAnnotations pats)
    PUnboxedSum alt arity inner -> PUnboxedSum alt arity (stripSuccessfulPatternAnnotations inner)
    PList pats -> PList (map stripSuccessfulPatternAnnotations pats)
    PCon name typeArgs pats -> PCon (stripSuccessfulNameAnnotations name) typeArgs (map stripSuccessfulPatternAnnotations pats)
    PInfix lhs op rhs ->
      PInfix
        (stripSuccessfulPatternAnnotations lhs)
        (stripSuccessfulNameAnnotations op)
        (stripSuccessfulPatternAnnotations rhs)
    PView expr inner -> PView (stripSuccessfulExprAnnotations expr) (stripSuccessfulPatternAnnotations inner)
    PAs name inner -> PAs (stripSuccessfulUnqualifiedNameAnnotations name) (stripSuccessfulPatternAnnotations inner)
    PStrict inner -> PStrict (stripSuccessfulPatternAnnotations inner)
    PIrrefutable inner -> PIrrefutable (stripSuccessfulPatternAnnotations inner)
    PParen inner -> PParen (stripSuccessfulPatternAnnotations inner)
    PTypeSig inner ty -> PTypeSig (stripSuccessfulPatternAnnotations inner) ty
    _ -> pat

stripSuccessfulNameAnnotations :: Name -> Name
stripSuccessfulNameAnnotations name =
  name {nameAnns = filter (not . isSuccessfulTcAnnotation) (nameAnns name)}

stripSuccessfulUnqualifiedNameAnnotations :: UnqualifiedName -> UnqualifiedName
stripSuccessfulUnqualifiedNameAnnotations name =
  name {unqualifiedNameAnns = filter (not . isSuccessfulTcAnnotation) (unqualifiedNameAnns name)}

isSuccessfulTcAnnotation :: Annotation -> Bool
isSuccessfulTcAnnotation ann =
  isJust (fromAnnotation @TcAnnotation ann)
    || isJust (fromAnnotation @TcBindingAnnotation ann)

commitGeneralizedMetas :: TcType -> TypeScheme -> TcM ()
commitGeneralizedMetas ty (ForAll _ _ body) = do
  ty' <- zonkType ty
  mapM_ commit (metaTyVarPairs ty' body)
  where
    commit (meta, tv) = writeMetaTv meta (TcTyVar tv)

metaTyVarPairs :: TcType -> TcType -> [(Unique, TyVarId)]
metaTyVarPairs (TcMetaTv u) (TcTyVar tv) = [(u, tv)]
metaTyVarPairs (TcTyCon tc args) (TcTyCon targetTc targetArgs)
  | tc == targetTc,
    length args == length targetArgs =
      concat (zipWith metaTyVarPairs args targetArgs)
metaTyVarPairs (TcFunTy a b) (TcFunTy targetA targetB) =
  metaTyVarPairs a targetA <> metaTyVarPairs b targetB
metaTyVarPairs (TcAppTy f a) (TcAppTy targetF targetA) =
  metaTyVarPairs f targetF <> metaTyVarPairs a targetA
metaTyVarPairs (TcQualTy preds body) (TcQualTy targetPreds targetBody) =
  concat (zipWith metaTyVarPairsPred preds targetPreds) <> metaTyVarPairs body targetBody
metaTyVarPairs _ _ = []

metaTyVarPairsPred :: Pred -> Pred -> [(Unique, TyVarId)]
metaTyVarPairsPred (ClassPred className args) (ClassPred targetClass targetArgs)
  | className == targetClass,
    length args == length targetArgs =
      concat (zipWith metaTyVarPairs args targetArgs)
metaTyVarPairsPred (EqPred left right) (EqPred targetLeft targetRight) =
  metaTyVarPairs left targetLeft <> metaTyVarPairs right targetRight
metaTyVarPairsPred _ _ = []

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
  fst <$> tcFunctionInfer displayName name matches
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of
  -- Bare variable pattern (e.g. @x = 5@, @(.>.) = (++)@): type-check as a
  -- zero-argument function so that the binding gets generalized and registered
  -- in the environment.
  Just (displayName, name) -> do
    fst <$> tcFunctionInfer displayName name [zeroArgMatch (patternSpan pat) rhs]
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

zeroArgMatch :: SourceSpan -> Rhs Expr -> Match
zeroArgMatch sp rhs =
  Match
    { matchAnns = sourceSpanAnn sp,
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }

sourceSpanAnn :: SourceSpan -> [Annotation]
sourceSpanAnn NoSourceSpan = []
sourceSpanAnn sp = [mkAnnotation sp]

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sp _ = sp

patternSpan :: Pattern -> SourceSpan
patternSpan pat =
  case pat of
    PAnn ann inner ->
      fromMaybe (patternSpan inner) (fromAnnotation ann)
    PVar name -> sourceSpanFromAnns (unqualifiedNameAnns name)
    PParen inner -> patternSpan inner
    PAs name _ -> sourceSpanFromAnns (unqualifiedNameAnns name)
    PStrict inner -> patternSpan inner
    PIrrefutable inner -> patternSpan inner
    _ -> NoSourceSpan

typeSpan :: Type -> SourceSpan
typeSpan ty =
  case ty of
    TAnn ann inner ->
      fromMaybe (typeSpan inner) (fromAnnotation @SourceSpan ann)
    TParen inner -> typeSpan inner
    TForall _ inner -> typeSpan inner
    TContext _ inner -> typeSpan inner
    TKindSig inner _ -> typeSpan inner
    _ -> NoSourceSpan

rhsExprSpan :: Rhs Expr -> SourceSpan
rhsExprSpan rhs =
  case rhs of
    UnguardedRhs anns expr _ -> exprSpan expr `orSourceSpan` sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EAnn ann inner ->
      fromMaybe (exprSpan inner) (fromAnnotation @SourceSpan ann)
    EParen inner -> exprSpan inner
    ETypeSig inner _ -> exprSpan inner
    _ -> NoSourceSpan

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
solveCircularWithImpls :: (TcSolveReport -> TcM (a, [Ct], [Implication])) -> TcM a
solveCircularWithImpls =
  solveCircularWith $ \cts impls -> do
    void (solveWithImpls cts impls)

solveCircularWith :: ([Ct] -> [Implication] -> TcM ()) -> (TcSolveReport -> TcM (a, [Ct], [Implication])) -> TcM a
solveCircularWith solveGenerated generate =
  solveCircularWithPost solveGenerated generate pure

solveCircularWithPost :: ([Ct] -> [Implication] -> TcM ()) -> (TcSolveReport -> TcM (a, [Ct], [Implication])) -> (a -> TcM b) -> TcM b
solveCircularWithPost solveGenerated generate finish = do
  env <- ask
  startState <- lift get
  let report = solveReportFromState finalState
      finalResult =
        case runTcM env startState (generate report) of
          Left _abort ->
            Left "internal type annotation error: circular constraint generation failed"
          Right ((value, cts, impls), generatedState) ->
            case runTcM env generatedState (solveGenerated cts impls) of
              Left _abort ->
                Left "internal type annotation error: circular constraint solving failed"
              Right ((), solvedState) ->
                case runTcM env solvedState (finish value) of
                  Left _abort ->
                    Left "internal type annotation error: circular constraint finalization failed"
                  Right (finishedValue, finishedState) ->
                    Right (finishedValue, finishedState)
      finalState =
        case finalResult of
          Right (_, finishedState) -> finishedState
          Left _ -> startState
  case finalResult of
    Left message -> abortTc message
    Right (value, finishedState) -> do
      lift (put finishedState)
      pure value

tcMatches :: TcSolveReport -> [Match] -> TcM (TcType, [Match], [Ct], [Implication])
tcMatches _report [] = do
  ty <- freshMetaTv
  pure (ty, [], [], [])
tcMatches report matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      (rhs0', ty0, cts0) <- inferRhs report (matchRhs m0)
      restResults <- mapM (unifyMatchRhs report ty0) (drop 1 matches)
      let m0' = m0 {matchRhs = rhs0'}
          restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (ty0, m0' : restMatches, cts0 ++ restCts, [])
    else do
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      results <- mapM (tcMatchEquation report Nothing argTys resTy) matches
      let (matchesList, ctsList, implsList) = unzip3 results
          allCts = concat ctsList
          allImpls = concat implsList
          funTy = foldr TcFunTy resTy argTys
      pure (funTy, matchesList, allCts, allImpls)

tcMatchEquation :: TcSolveReport -> Maybe TypeOrigin -> [TcType] -> TcType -> Match -> TcM (Match, [Ct], [Implication])
tcMatchEquation report expectedOrigin argTys resTy match = do
  let pats = matchPats match
      sp = sourceSpanFromAnns (matchAnns match)
  patCheck <- checkPatternsWithGivens sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs report (matchRhs match))
  ev <- freshEvVar
  let rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      resCt =
        mkWantedEqCtAt
          TypeTrace
            { typeTraceType = rhsTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin rhsSp
            }
          TypeTrace
            { typeTraceType = resTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = fromMaybe (ConstraintTypeOrigin (AppOrigin sp)) expectedOrigin
            }
          ev
          (AppOrigin rhsSp)
          rhsSp
      rhs'' = attachRhsFailure report resCt rhs'
      pats' = zipWith (annotatePatternBindingWithReport report) argTys pats
      match' = match {matchPats = pats', matchRhs = rhs''}
      givenCts = pcGivenCts patCheck
      bodyWanteds = pcWantedCts patCheck ++ rhsCts ++ [resCt]
  if null givenCts
    then pure (match', bodyWanteds, [])
    else do
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
      pure (match', [], [impl])

unifyMatchRhs :: TcSolveReport -> TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs report expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhs report (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
      rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      eqCt =
        mkWantedEqCtAt
          TypeTrace
            { typeTraceType = rhsTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin rhsSp
            }
          TypeTrace
            { typeTraceType = expectedTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)
            }
          ev
          (AppOrigin rhsSp)
          rhsSp
      match' = match {matchRhs = attachRhsFailure report eqCt rhs'}
  pure (match', rhsCts ++ [eqCt])

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs Expr -> TcM TcType
tcRhs rhs = do
  (_rhs', ty, cts) <- inferRhs emptyTcSolveReport rhs
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
