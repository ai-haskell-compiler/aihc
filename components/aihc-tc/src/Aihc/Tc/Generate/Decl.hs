{-# LANGUAGE LambdaCase #-}
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
    nameText,
    peelClassDeclItemAnn,
    peelDeclAnn,
    tyVarBinderName,
  )
import Aihc.Tc.Annotations
  ( PendingTcAnnotation (..),
    TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    annotateDecl,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Env (InstanceInfo (..), TyConInfo (..))
import Aihc.Tc.Evidence (EvTerm, EvVar)
import Aihc.Tc.Generalize (generalizeIgnoring)
import Aihc.Tc.Generate.Bind (inferRhsWithLocals)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkSurfaceType, convertSurfaceType, defaultKindMetas, freeTypeVars, freshKindMeta, kindToTcType, makeParamEnv, sigToScheme, surfacePredToPred, tyConKindFromParams)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints, solveWithImpls)
import Aihc.Tc.Solve.Dict (solveDictWithGivens)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (foldM, zipWithM)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (mapAccumL, nub, (\\))
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

data UserSig = UserSig
  { userSigName :: !Text,
    userSigType :: !Type,
    userSigSpan :: !SourceSpan
  }
  deriving (Show)

data CheckedSig = CheckedSig
  { checkedSigName :: !Text,
    checkedSigScheme :: !TypeScheme,
    checkedSigSpan :: !SourceSpan
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
  let sourceGroups = zip [0 :: Int ..] (groupValueDecls (moduleDecls m))
      grouped = sortDeclGroups sourceGroups
  groupResults <- mapM (tcDeclGroup schemes) grouped
  let valueResults = concatMap tcGroupBindingResults groupResults
      checkedGroups =
        Map.fromList
          [ (tcGroupId result, decls)
          | result <- groupResults,
            Just decls <- [tcGroupAnnotatedDecls result]
          ]
      valueAnnotatedModule =
        m {moduleDecls = concatMap (renderCheckedGroup checkedGroups) sourceGroups}
  -- Phase 4: type-check instance method bodies. They are not top-level
  -- value bindings, but their occurrences still need the same instantiation
  -- and evidence records as ordinary expressions.
  instanceDecls <- mapM tcInstanceDeclBodies (moduleDecls valueAnnotatedModule)
  let pendingModule = valueAnnotatedModule {moduleDecls = instanceDecls}
  -- Only bindings that checked without errors are eligible for value
  -- annotations. Failed bindings remain in the recovery environment, but
  -- they must not be rendered as successful inferred types.
  annotatedModule <- annotateModuleTc (Set.fromList (map tbName valueResults)) pendingModule
  finalizeModuleTc annotatedModule

data TcDeclGroupResult = TcDeclGroupResult
  { tcGroupId :: !Int,
    tcGroupBindingResults :: ![TcBindingResult],
    tcGroupAnnotatedDecls :: !(Maybe [Decl])
  }

renderCheckedGroup :: Map Int [Decl] -> (Int, DeclGroup) -> [Decl]
renderCheckedGroup checkedGroups (groupId, group) =
  fromMaybe (renderDeclGroup group) (Map.lookup groupId checkedGroups)

finalizeModuleTc :: Module -> TcM Module
finalizeModuleTc modu = do
  decls <- traverse finalizeDeclTc (moduleDecls modu)
  pure (modu {moduleDecls = decls})

finalizeAnnotationTc :: Annotation -> TcM Annotation
finalizeAnnotationTc ann =
  case fromAnnotation @PendingTcAnnotation ann of
    Just pending -> mkAnnotation <$> annotationForPendingTc pending
    Nothing -> pure ann

annotationForPendingTc :: PendingTcAnnotation -> TcM TcAnnotation
annotationForPendingTc pending = do
  ty <- zonkType (pendingTcAnnType pending)
  typeArgs <- mapM zonkType (pendingTcAnnTypeArgs pending)
  evidenceTerms <- mapM evidenceForEvVar (pendingTcAnnEvidenceVars pending)
  termArgTypes <- mapM zonkType (pendingTcAnnTermArgTypes pending)
  pure (TcAnnotation ty typeArgs evidenceTerms termArgTypes)

finalizeDeclTc :: Decl -> TcM Decl
finalizeDeclTc decl =
  case decl of
    DeclAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeDeclTc inner
      pure (DeclAnn ann' inner')
    DeclValue valueDecl ->
      DeclValue <$> finalizeValueDeclTc valueDecl
    DeclData dataDecl -> do
      constructors <- traverse finalizeDataConDeclTc (dataDeclConstructors dataDecl)
      pure (DeclData (dataDecl {dataDeclConstructors = constructors}))
    DeclClass classDecl -> do
      items <- traverse finalizeClassDeclItemTc (classDeclItems classDecl)
      pure (DeclClass (classDecl {classDeclItems = items}))
    DeclInstance instanceDecl -> do
      items <- traverse finalizeInstanceDeclItemTc (instanceDeclItems instanceDecl)
      pure (DeclInstance (instanceDecl {instanceDeclItems = items}))
    DeclSplice expr ->
      DeclSplice <$> finalizeExprTc expr
    _ -> pure decl

finalizeDataConDeclTc :: DataConDecl -> TcM DataConDecl
finalizeDataConDeclTc dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeDataConDeclTc inner
      pure (DataConAnn ann' inner')
    _ -> pure dataConDecl

finalizeClassDeclItemTc :: ClassDeclItem -> TcM ClassDeclItem
finalizeClassDeclItemTc item =
  case item of
    ClassItemAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeClassDeclItemTc inner
      pure (ClassItemAnn ann' inner')
    _ -> pure item

finalizeInstanceDeclItemTc :: InstanceDeclItem -> TcM InstanceDeclItem
finalizeInstanceDeclItemTc item =
  case item of
    InstanceItemAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeInstanceDeclItemTc inner
      pure (InstanceItemAnn ann' inner')
    InstanceItemBind valueDecl ->
      InstanceItemBind <$> finalizeValueDeclTc valueDecl
    _ -> pure item

finalizeValueDeclTc :: ValueDecl -> TcM ValueDecl
finalizeValueDeclTc valueDecl =
  case valueDecl of
    FunctionBind name matches ->
      FunctionBind name <$> traverse finalizeMatchTc matches
    PatternBind mult pat rhs ->
      PatternBind mult <$> finalizePatternTc pat <*> finalizeRhsTc rhs

finalizeMatchTc :: Match -> TcM Match
finalizeMatchTc match = do
  pats <- traverse finalizePatternTc (matchPats match)
  rhs <- finalizeRhsTc (matchRhs match)
  pure (match {matchPats = pats, matchRhs = rhs})

finalizeRhsTc :: Rhs Expr -> TcM (Rhs Expr)
finalizeRhsTc rhs =
  case rhs of
    UnguardedRhs anns expr maybeDecls -> do
      anns' <- traverse finalizeAnnotationTc anns
      expr' <- finalizeExprTc expr
      decls' <- traverse (traverse finalizeDeclTc) maybeDecls
      pure (UnguardedRhs anns' expr' decls')
    GuardedRhss anns guards maybeDecls -> do
      anns' <- traverse finalizeAnnotationTc anns
      decls' <- traverse (traverse finalizeDeclTc) maybeDecls
      pure (GuardedRhss anns' guards decls')

finalizeExprTc :: Expr -> TcM Expr
finalizeExprTc expr =
  case expr of
    EAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeExprTc inner
      pure (EAnn ann' inner')
    EVar name ->
      EVar <$> finalizeNameTc name
    EIf cond thenE elseE ->
      EIf <$> finalizeExprTc cond <*> finalizeExprTc thenE <*> finalizeExprTc elseE
    ELambdaPats pats body ->
      ELambdaPats <$> traverse finalizePatternTc pats <*> finalizeExprTc body
    ELambdaCase alts ->
      ELambdaCase <$> traverse finalizeCaseAltTc alts
    ELambdaCases alts ->
      ELambdaCases <$> traverse finalizeLambdaCaseAltTc alts
    EInfix lhs op rhs ->
      EInfix <$> finalizeExprTc lhs <*> finalizeNameTc op <*> finalizeExprTc rhs
    ENegate inner ->
      ENegate <$> finalizeExprTc inner
    ELetDecls decls body ->
      ELetDecls <$> traverse finalizeDeclTc decls <*> finalizeExprTc body
    ECase scrut alts ->
      ECase <$> finalizeExprTc scrut <*> traverse finalizeCaseAltTc alts
    EListComp body stmts ->
      EListComp <$> finalizeExprTc body <*> traverse finalizeCompStmtTc stmts
    ETypeSig inner ty ->
      (`ETypeSig` ty) <$> finalizeExprTc inner
    EParen inner ->
      EParen <$> finalizeExprTc inner
    EList elems ->
      EList <$> traverse finalizeExprTc elems
    ETuple flavor elems ->
      ETuple flavor <$> traverse (traverse finalizeExprTc) elems
    EUnboxedSum alt arity inner ->
      EUnboxedSum alt arity <$> finalizeExprTc inner
    ETypeApp inner ty ->
      (`ETypeApp` ty) <$> finalizeExprTc inner
    EApp fun arg ->
      EApp <$> finalizeExprTc fun <*> finalizeExprTc arg
    EPragma pragma inner ->
      EPragma pragma <$> finalizeExprTc inner
    _ -> pure expr

finalizeCaseAltTc :: CaseAlt Expr -> TcM (CaseAlt Expr)
finalizeCaseAltTc (CaseAlt anns pat rhs) = do
  anns' <- traverse finalizeAnnotationTc anns
  pat' <- finalizePatternTc pat
  rhs' <- finalizeRhsTc rhs
  pure (CaseAlt anns' pat' rhs')

finalizeLambdaCaseAltTc :: LambdaCaseAlt -> TcM LambdaCaseAlt
finalizeLambdaCaseAltTc alt = do
  anns <- traverse finalizeAnnotationTc (lambdaCaseAltAnns alt)
  pats <- traverse finalizePatternTc (lambdaCaseAltPats alt)
  rhs <- finalizeRhsTc (lambdaCaseAltRhs alt)
  pure
    alt
      { lambdaCaseAltAnns = anns,
        lambdaCaseAltPats = pats,
        lambdaCaseAltRhs = rhs
      }

finalizeCompStmtTc :: CompStmt -> TcM CompStmt
finalizeCompStmtTc stmt =
  case stmt of
    CompAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizeCompStmtTc inner
      pure (CompAnn ann' inner')
    CompGen pat src ->
      CompGen <$> finalizePatternTc pat <*> finalizeExprTc src
    CompGuard guard ->
      CompGuard <$> finalizeExprTc guard
    CompLetDecls decls ->
      CompLetDecls <$> traverse finalizeDeclTc decls
    CompThen expr ->
      CompThen <$> finalizeExprTc expr
    CompThenBy f byExpr ->
      CompThenBy <$> finalizeExprTc f <*> finalizeExprTc byExpr
    CompGroupUsing expr ->
      CompGroupUsing <$> finalizeExprTc expr
    CompGroupByUsing byExpr usingExpr ->
      CompGroupByUsing <$> finalizeExprTc byExpr <*> finalizeExprTc usingExpr

finalizePatternTc :: Pattern -> TcM Pattern
finalizePatternTc pat =
  case pat of
    PAnn ann inner -> do
      ann' <- finalizeAnnotationTc ann
      inner' <- finalizePatternTc inner
      pure (PAnn ann' inner')
    PVar name ->
      PVar <$> finalizeUnqualifiedNameTc name
    PTuple flavor pats ->
      PTuple flavor <$> traverse finalizePatternTc pats
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity <$> finalizePatternTc inner
    PList pats ->
      PList <$> traverse finalizePatternTc pats
    PCon name tys pats ->
      PCon <$> finalizeNameTc name <*> pure tys <*> traverse finalizePatternTc pats
    PInfix lhs op rhs ->
      PInfix <$> finalizePatternTc lhs <*> finalizeNameTc op <*> finalizePatternTc rhs
    PView expr inner ->
      PView <$> finalizeExprTc expr <*> finalizePatternTc inner
    PAs name inner ->
      PAs <$> finalizeUnqualifiedNameTc name <*> finalizePatternTc inner
    PStrict inner ->
      PStrict <$> finalizePatternTc inner
    PIrrefutable inner ->
      PIrrefutable <$> finalizePatternTc inner
    PParen inner ->
      PParen <$> finalizePatternTc inner
    PRecord name fields wildcard ->
      pure (PRecord name fields wildcard)
    PTypeSig inner ty ->
      (`PTypeSig` ty) <$> finalizePatternTc inner
    PSplice expr ->
      PSplice <$> finalizeExprTc expr
    _ -> pure pat

finalizeNameTc :: Name -> TcM Name
finalizeNameTc name = do
  anns <- traverse finalizeAnnotationTc (nameAnns name)
  pure (name {nameAnns = anns})

finalizeUnqualifiedNameTc :: UnqualifiedName -> TcM UnqualifiedName
finalizeUnqualifiedNameTc name = do
  anns <- traverse finalizeAnnotationTc (unqualifiedNameAnns name)
  pure (name {unqualifiedNameAnns = anns})

annotateModuleTc :: Set.Set Text -> Module -> TcM Module
annotateModuleTc checkedValueNames m = do
  let classMethods = collectClassMethodNames (moduleDecls m)
  decls <- mapM (annotateDeclTc classMethods checkedValueNames) (moduleDecls m)
  pure (m {moduleDecls = decls})

annotateDeclTc :: Map Text [Text] -> Set.Set Text -> Decl -> TcM Decl
annotateDeclTc classMethods checkedValueNames decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann <$> annotateDeclTc classMethods checkedValueNames inner
    DeclValue valueDecl
      | valueDeclWasChecked checkedValueNames valueDecl -> do
          (ty, valueDecl') <- annotateValueDeclTc valueDecl
          pure (annotateDeclAt (valueDeclSpan valueDecl) (TcAnnotation ty [] [] []) (DeclValue valueDecl'))
      | otherwise -> pure decl
    DeclData dataDecl -> annotateDataDeclTc dataDecl
    DeclForeign foreignDecl
      | isForeignImport foreignDecl -> annotateForeignDeclTc foreignDecl
    DeclClass classDecl -> annotateClassDeclTc classDecl
    DeclInstance instanceDecl -> annotateInstanceDeclTc classMethods instanceDecl
    _ -> pure decl

valueDeclWasChecked :: Set.Set Text -> ValueDecl -> Bool
valueDeclWasChecked checkedValueNames valueDecl =
  any (`Set.member` checkedValueNames) (valueDeclBinderNames valueDecl)

valueDeclBinderNames :: ValueDecl -> [Text]
valueDeclBinderNames valueDecl =
  case valueDecl of
    FunctionBind binder _ -> [unqualifiedNameText binder]
    PatternBind _ pat _ -> patternBinders pat

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

annotateDataDeclTc :: DataDecl -> TcM Decl
annotateDataDeclTc dataDecl = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
  ty <- tyConBindingType tyName
  constructors <- mapM annotateDataConDeclTc (dataDeclConstructors dataDecl)
  pure (annotateDeclAt (unqualifiedNameSpan (binderHeadName (dataDeclHead dataDecl))) (TcAnnotation ty [] [] []) (DeclData (dataDecl {dataDeclConstructors = constructors})))

annotateDataConDeclTc :: DataConDecl -> TcM DataConDecl
annotateDataConDeclTc dataConDecl = do
  case dataConBindingNames dataConDecl of
    [] -> pure dataConDecl
    (name, _) : _ -> do
      ty <- dataConBindingType name
      pure (DataConAnn (mkAnnotation (TcAnnotation ty [] [] [])) dataConDecl)

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
  pure (annotateDeclAt (unqualifiedNameSpan (foreignName foreignDecl)) (TcAnnotation ty [] [] []) (DeclForeign foreignDecl))

annotateDeclAt :: SourceSpan -> TcAnnotation -> Decl -> Decl
annotateDeclAt NoSourceSpan tcAnn decl =
  annotateDecl tcAnn decl
annotateDeclAt sp tcAnn decl =
  DeclAnn (mkAnnotation sp) (annotateDecl tcAnn decl)

valueDeclSpan :: ValueDecl -> SourceSpan
valueDeclSpan valueDecl =
  case valueDecl of
    FunctionBind name _ -> unqualifiedNameSpan name
    PatternBind _ pat _ -> patternSpan pat

unqualifiedNameSpan :: UnqualifiedName -> SourceSpan
unqualifiedNameSpan =
  sourceSpanFromAnns . unqualifiedNameAnns

tyConBindingType :: Text -> TcM TcType
tyConBindingType name = do
  mInfo <- lookupTyCon name
  case mInfo of
    Just info -> kindToTcType <$> defaultKindMetas (tciKind info)
    Nothing -> missingTypeInfo ("type constructor " <> T.unpack name)

annotateValueDeclTc :: ValueDecl -> TcM (TcType, ValueDecl)
annotateValueDeclTc valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      bindingTy <- bindingType (unqualifiedNameText name)
      pure (bindingTy, FunctionBind name matches)
    PatternBind anns pat rhs ->
      case patternBinderName pat of
        Just (_, name) -> do
          bindingTy <- bindingType name
          pure (bindingTy, PatternBind anns pat rhs)
        Nothing -> do
          ty <- missingTypeInfo ("top-level pattern binding " <> show pat)
          pure (ty, valueDecl)

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
      items <- mapM (annotateInstanceItemTc headTys) (instanceDeclItems instanceDecl)
      pure (DeclAnn (mkAnnotation instAnn) (DeclInstance (instanceDecl {instanceDeclItems = items})))

annotateInstanceItemTc :: [TcType] -> InstanceDeclItem -> TcM InstanceDeclItem
annotateInstanceItemTc headTys item =
  case item of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> annotateInstanceItemTc headTys inner
    InstanceItemBind (FunctionBind name matches) -> do
      let methodName = unqualifiedNameText name
      methodTy <- methodExpectedType headTys (unqualifiedNameText name)
      pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) (InstanceItemBind (FunctionBind name matches)))
    InstanceItemBind (PatternBind anns pat rhs) ->
      case patternBinderName pat of
        Just (_, methodName) -> do
          methodTy <- methodExpectedType headTys methodName
          pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) (InstanceItemBind (PatternBind anns pat rhs)))
        Nothing -> pure item
    _ -> pure item

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
      results <- mapM (tcMatchEquation Nothing argTys resTy) matches
      solveInstanceBodyConstraints givens [(cts, impls) | (_match, cts, impls) <- results]
      pure (InstanceItemBind (FunctionBind name [match | (match, _cts, _impls) <- results]))
    InstanceItemBind (PatternBind _ pat rhs) ->
      case patternBinderName pat of
        Just (_, methodName) -> do
          methodTy <- methodExpectedType headTys methodName
          results <- mapM (tcMatchEquation Nothing [] methodTy) [zeroArgMatch (patternSpan pat) rhs]
          solveInstanceBodyConstraints givens [(cts, impls) | (_match, cts, impls) <- results]
          case results of
            [(match, _cts, _impls)] ->
              pure (replaceInstancePatternBindRhs (matchRhs match) item)
            _ -> pure item
        Nothing -> pure item
    _ -> pure item

matchArity :: [Match] -> Int
matchArity (match : _) = length (matchPats match)
matchArity [] = 0

solveInstanceBodyConstraints :: [Pred] -> [([Ct], [Implication])] -> TcM ()
solveInstanceBodyConstraints givens results = do
  let (ctsList, implsList) = unzip results
      cts = concat ctsList
      impls = concat implsList
  solveBodyConstraintsWithGivens givens cts impls

solveBodyConstraintsWithGivens :: [Pred] -> [Ct] -> [Implication] -> TcM ()
solveBodyConstraintsWithGivens givens cts impls = do
  _ <- solveWithImpls cts impls
  mapM_ solveClassCt cts
  where
    solveClassCt ct@Ct {ctPred = ClassPred {}} = do
      _ <- solveDictWithGivens givens ct
      pure ()
    solveClassCt _ = pure ()

evidenceForEvVar :: EvVar -> TcM EvTerm
evidenceForEvVar ev = do
  maybeEvidence <- lookupEvidence ev
  case maybeEvidence of
    Just evidence -> pure evidence
    Nothing ->
      abortTc ("internal type annotation error: missing evidence for " <> show ev)

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
  scheme <- sigToScheme (userSigType userSig)
  pure
    CheckedSig
      { checkedSigName = userSigName userSig,
        checkedSigScheme = scheme,
        checkedSigSpan = userSigSpan userSig
      }

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
  | MergedFunctionBind SourceSpan UnqualifiedName [Decl] [Match]

-- | Group consecutive FunctionBind declarations with the same name.
groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) = case extractFunctionBind d of
  Just (sp, name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        groupDecls = d : sameNameDecls
        allMatches = matches ++ concatMap (maybe [] (\(_, _, ms) -> ms) . extractFunctionBind) sameNameDecls
     in MergedFunctionBind sp name groupDecls allMatches : groupValueDecls rest
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
sortDeclGroups :: [(Int, DeclGroup)] -> [(Int, DeclGroup)]
sortDeclGroups groups =
  concatMap flattenScc (stronglyConnComp nodes)
  where
    allBinders = Set.fromList (concatMap (declGroupBinders . snd) groups)
    nodes =
      [ (numberedGroup, groupKey groupId group, Set.toList (Set.intersection allBinders (Set.fromList (freeVarsGroup group))))
      | numberedGroup@(groupId, group) <- groups
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
    MergedFunctionBind _sp binder _decls _matches -> [unqualifiedNameText binder]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind binder _) -> [unqualifiedNameText binder]
        DeclValue (PatternBind _ pat _) -> maybe [] ((: []) . snd) (patternBinderName pat)
        _ -> []

freeVarsGroup :: DeclGroup -> [Text]
freeVarsGroup group =
  case group of
    MergedFunctionBind _sp binder _decls matches ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText binder]
    SingleDecl decl -> freeVarsDecl decl

renderDeclGroup :: DeclGroup -> [Decl]
renderDeclGroup group =
  case group of
    SingleDecl decl -> [decl]
    MergedFunctionBind _ _ decls _ -> decls

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

replacePatternBindRhs :: Rhs Expr -> Decl -> Decl
replacePatternBindRhs rhs decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replacePatternBindRhs rhs inner)
    DeclValue (PatternBind mult pat _) -> DeclValue (PatternBind mult pat rhs)
    _ -> decl

replaceInstancePatternBindRhs :: Rhs Expr -> InstanceDeclItem -> InstanceDeclItem
replaceInstancePatternBindRhs rhs item =
  case item of
    InstanceItemAnn ann inner -> InstanceItemAnn ann (replaceInstancePatternBindRhs rhs inner)
    InstanceItemBind (PatternBind mult pat _) -> InstanceItemBind (PatternBind mult pat rhs)
    _ -> item

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
tcDeclGroup :: Map Text CheckedSig -> (Int, DeclGroup) -> TcM TcDeclGroupResult
tcDeclGroup sigs (groupId, group) =
  case group of
    SingleDecl d -> tcSingleDeclGroup sigs groupId d
    MergedFunctionBind _sp binder decls matches -> tcMergedFunctionGroup sigs groupId binder decls matches

tcSingleDeclGroup :: Map Text CheckedSig -> Int -> Decl -> TcM TcDeclGroupResult
tcSingleDeclGroup sigs groupId d =
  case peelDeclAnn d of
    DeclValue (PatternBind _ pat rhs)
      | Just (displayName, name) <- patternBinderName pat,
        Just sig <- Map.lookup name sigs ->
          do
            (maybeMatches, bindings) <- tcFunctionWithSig displayName name sig [zeroArgMatch (patternSpan pat `orSourceSpan` peelDeclSpan NoSourceSpan d) rhs]
            let annotatedDecls = fmap (\case [match] -> [replacePatternBindRhs (matchRhs match) d]; _ -> [d]) maybeMatches
            pure (TcDeclGroupResult groupId bindings annotatedDecls)
    DeclValue (PatternBind _ pat rhs)
      | Just (displayName, name) <- patternBinderName pat -> do
          (maybeMatches, bindings) <- tcFunctionInfer displayName name [zeroArgMatch (patternSpan pat) rhs]
          let annotatedDecls = fmap (\case [match] -> [replacePatternBindRhs (matchRhs match) d]; _ -> [d]) maybeMatches
          pure (TcDeclGroupResult groupId bindings annotatedDecls)
      | otherwise -> do
          ((rhs', ty), failed) <- withErrorTracking (tcRhs rhs)
          if failed
            then pure (TcDeclGroupResult groupId [] Nothing)
            else do
              zonkedTy <- zonkType ty
              let decl' = replacePatternBindRhs rhs' d
              pure (TcDeclGroupResult groupId [TcBindingResult "<pattern>" "<pattern>" zonkedTy] (Just [decl']))
    _ -> do
      bindings <- tcDecl d
      pure (TcDeclGroupResult groupId bindings Nothing)

tcMergedFunctionGroup :: Map Text CheckedSig -> Int -> UnqualifiedName -> [Decl] -> [Match] -> TcM TcDeclGroupResult
tcMergedFunctionGroup sigs groupId binder decls matches = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  (maybeMatches, bindings) <- case Map.lookup name sigs of
    Just sig -> do
      -- Use the declared type signature for checking.
      tcFunctionWithSig displayName name sig matches
    Nothing -> do
      -- No signature: infer the type.
      tcFunctionInfer displayName name matches
  let annotatedDecls = fmap (`replaceFunctionDeclMatches` decls) maybeMatches
  pure (TcDeclGroupResult groupId bindings annotatedDecls)

-- | Type-check a function with a known type signature.
-- The signature's type variables are opened as rigid skolems so that
-- the body is checked against them. GADT patterns generate implication
-- constraints using the signature's skolems as given equalities.
tcFunctionWithSig :: Text -> Text -> CheckedSig -> [Match] -> TcM (Maybe [Match], [TcBindingResult])
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
      -- Check each equation against the signature types.
      results <- mapM (tcMatchEquation (Just (TypeSignatureOrigin (checkedSigName sig) (checkedSigSpan sig))) argTys resTy) matches
      let (_matches', ctsList, implsList) = unzip3 results
          allCts = concat ctsList
          allImpls = concat implsList
      solveBodyConstraintsWithGivens sigPreds allCts allImpls
      pure _matches'
  if failed
    then pure (Nothing, [])
    else do
      -- Report the declared scheme as the binding's type.
      let declaredTy = schemeToType scheme
      zonkedTy <- zonkType declaredTy
      pure (Just matches', [TcBindingResult name displayName zonkedTy])

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: Text -> Text -> [Match] -> TcM (Maybe [Match], [TcBindingResult])
tcFunctionInfer displayName name matches = do
  placeholderTy <- freshMetaTv
  ((matches', ty, _, _), failed) <-
    withErrorTracking $ do
      extendTermEnvPermanent name (TcMonoIdBinder name placeholderTy)
      result@(_, _, cts', impls') <- tcMatches matches
      _ <- solveWithImpls cts' impls'
      pure result
  if failed
    then pure (Nothing, [])
    else do
      scheme <- generalizeIgnoring [name] ty []
      commitGeneralizedMetas ty scheme
      let schemeTy = schemeToType scheme
      zonkedTy <- zonkType schemeTy
      extendTermEnvPermanent name (TcIdBinder name scheme Closed)
      pure (Just matches', [TcBindingResult name displayName zonkedTy])

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
  snd <$> tcFunctionInfer displayName name matches
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of
  -- Bare variable pattern (e.g. @x = 5@, @(.>.) = (++)@): type-check as a
  -- zero-argument function so that the binding gets generalized and registered
  -- in the environment.
  Just (displayName, name) -> do
    snd <$> tcFunctionInfer displayName name [zeroArgMatch (patternSpan pat) rhs]
  -- Non-trivial pattern binding: infer the RHS type without generalization.
  Nothing -> do
    (_rhs', ty) <- tcRhs rhs
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
tcMatches :: [Match] -> TcM ([Match], TcType, [Ct], [Implication])
tcMatches [] = do
  ty <- freshMetaTv
  pure ([], ty, [], [])
tcMatches matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      -- No patterns: just infer the RHS of the first match.
      (rhs0, ty0, cts0) <- inferRhsExpr (matchRhs m0)
      restResults <- mapM (unifyMatchRhs ty0) (drop 1 matches)
      let firstMatch = m0 {matchRhs = rhs0}
          restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (firstMatch : restMatches, ty0, cts0 ++ restCts, [])
    else do
      -- Create fresh meta-variables for the argument types and result type.
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      -- Process each equation.
      results <- mapM (tcMatchEquation Nothing argTys resTy) matches
      let (matches', ctsList, implsList) = unzip3 results
          allCts = concat ctsList
          allImpls = concat implsList
          funTy = foldr TcFunTy resTy argTys
      pure (matches', funTy, allCts, allImpls)

-- | Type-check a single match equation against expected arg/result types.
-- Returns flat wanted constraints and implication constraints.
tcMatchEquation :: Maybe TypeOrigin -> [TcType] -> TcType -> Match -> TcM (Match, [Ct], [Implication])
tcMatchEquation expectedOrigin argTys resTy match = do
  let pats = matchPats match
      sp = sourceSpanFromAnns (matchAnns match)
  patCheck <- checkPatternsWithGivens sp (zip pats argTys)
  -- Infer the RHS under the extended environment.
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsExpr (matchRhs match))
  -- RHS type must match the expected result type.
  ev <- freshEvVar
  let rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      resCt =
        mkWantedEqCt
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
  let givenCts = pcGivenCts patCheck
      bodyWanteds = pcWantedCts patCheck ++ rhsCts ++ [resCt]
  if null givenCts
    then -- No GADT givens: emit everything as flat wanteds.
      pure (match {matchRhs = rhs'}, bodyWanteds, [])
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
      pure (match {matchRhs = rhs'}, [], [impl])

-- | Unify an additional match equation's RHS with the expected type.
unifyMatchRhs :: TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsExpr (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
      rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      eqCt =
        mkWantedEqCt
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
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

-- | Infer the type of a right-hand side expression.
inferRhsExpr :: Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhsExpr = inferRhsWithLocals inferExpr

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs Expr -> TcM (Rhs Expr, TcType)
tcRhs rhs = do
  (rhs', ty, cts) <- inferRhsWithLocals inferExpr rhs
  _ <- solveConstraints cts
  pure (rhs', ty)

-- | Render an unqualified name for display.
-- Operators (NameVarSym, NameConSym) are wrapped in parentheses.
renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname
