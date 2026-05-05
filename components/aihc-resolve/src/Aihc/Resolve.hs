{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Resolve
  ( pattern DeclResolution,
    pattern EResolution,
    pattern ImportResolution,
    pattern PResolution,
    pattern TResolution,
    resolve,
    resolveWithDeps,
    extractInterface,
    Scope (..),
    ModuleExports,
    collectModuleExports,
    ResolveError (..),
    ResolveResult (..),
    ResolutionNamespace (..),
    ResolvedName (..),
    ResolutionAnnotation (..),
    renderResolveResult,
    renderAnnotatedResolveResult,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ArrowKind (..),
    BangType (..),
    BinderHead,
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    FieldDecl (..),
    ForallTelescope (..),
    GadtBody (..),
    GuardQualifier (..),
    GuardedRhs (..),
    IEBundledMember (..),
    ImportDecl (..),
    ImportItem (..),
    ImportLevel (..),
    ImportSpec (..),
    LambdaCaseAlt (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    Pattern (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    TyVarBinder (..),
    Type (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    mkAnnotation,
    mkQualifiedName,
    mkUnqualifiedName,
    moduleName,
    peelGuardQualifierAnn,
    peelPatternAnn,
    recordFieldName,
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Data.Data (Data, cast, gmapQ, showConstr, toConstr)
import Data.List (find, mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

-- | Use a 'SourceSpan' stored in a dynamic annotation as the innermost ambient span.
pushSpanFromAnn :: SourceSpan -> Annotation -> SourceSpan
pushSpanFromAnn cur ann = fromMaybe cur (fromAnnotation @SourceSpan ann)

-- | Prefer a concrete span on a node; fall back to the ambient span from annotations.
effectiveResolutionSpan :: SourceSpan -> SourceSpan -> SourceSpan
effectiveResolutionSpan ambient localSpan =
  case localSpan of
    NoSourceSpan -> ambient
    _ -> localSpan

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

-- | Resolver-owned span tracking for nodes that now store source spans only in
-- annotations.
peelDeclSpan :: SourceSpan -> Decl -> (SourceSpan, Decl)
peelDeclSpan ambient (DeclAnn ann inner) = peelDeclSpan (pushSpanFromAnn ambient ann) inner
peelDeclSpan ambient decl = (ambient, decl)

peelPatternSpan :: SourceSpan -> Pattern -> SourceSpan
peelPatternSpan ambient (PAnn ann inner) = peelPatternSpan (pushSpanFromAnn ambient ann) inner
peelPatternSpan ambient _ = ambient

peelGuardQualifierSpan :: SourceSpan -> GuardQualifier -> SourceSpan
peelGuardQualifierSpan ambient (GuardAnn ann inner) = peelGuardQualifierSpan (pushSpanFromAnn ambient ann) inner
peelGuardQualifierSpan ambient _ = ambient

peelDataConSpan :: SourceSpan -> DataConDecl -> SourceSpan
peelDataConSpan ambient (DataConAnn ann inner) = peelDataConSpan (pushSpanFromAnn ambient ann) inner
peelDataConSpan ambient _ = ambient

peelImportItemSpan :: SourceSpan -> ImportItem -> SourceSpan
peelImportItemSpan ambient (ImportAnn ann inner) = peelImportItemSpan (pushSpanFromAnn ambient ann) inner
peelImportItemSpan ambient _ = ambient

rhsSpan :: Rhs body -> SourceSpan
rhsSpan rhs =
  case rhs of
    UnguardedRhs anns _ _ -> sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

unhandledSyntaxAnnotation :: (Data a) => ResolutionNamespace -> SourceSpan -> a -> ResolutionAnnotation
unhandledSyntaxAnnotation namespace span' node =
  ResolutionAnnotation
    span'
    (T.pack (showConstr (toConstr node)))
    namespace
    (ResolvedError "unhandled syntax")

annotateUnhandledDecl :: SourceSpan -> Decl -> Decl
annotateUnhandledDecl span' decl =
  annotateDecl (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' decl) decl

annotateUnhandledClassDeclItem :: SourceSpan -> ClassDeclItem -> ClassDeclItem
annotateUnhandledClassDeclItem span' item =
  ClassItemAnn (mkAnnotation (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' item)) item

annotateUnhandledExpr :: SourceSpan -> Expr -> Expr
annotateUnhandledExpr span' expr =
  annotateExpr (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' expr) expr

annotateUnhandledPattern :: SourceSpan -> Pattern -> Pattern
annotateUnhandledPattern span' pat =
  annotatePattern (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' pat) pat

annotateUnhandledType :: SourceSpan -> Type -> Type
annotateUnhandledType span' ty =
  annotateType (unhandledSyntaxAnnotation ResolutionNamespaceType span' ty) ty

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeConstructors :: Map.Map Text [Text],
    scopeRecordFields :: Map.Map Text [Text],
    scopeMethods :: Map.Map Text [Text],
    scopeQualifiedModules :: Map.Map Text Scope
  }

type ModuleExports = Map.Map Text Scope

data ResolveEnv = ResolveEnv
  { envScope :: !Scope,
    envSpan :: !SourceSpan
  }

data ResolveState = ResolveState
  { stateNextLocal :: !Int,
    stateGeneratedAnnotations :: [ResolutionAnnotation]
  }

newtype ResolveM a = ResolveM
  { unResolveM :: ResolveEnv -> ResolveState -> (a, ResolveState)
  }

instance Functor ResolveM where
  fmap f action =
    ResolveM $ \env state ->
      let (result, state') = unResolveM action env state
       in (f result, state')

instance Applicative ResolveM where
  pure result = ResolveM $ \_ state -> (result, state)

  fun <*> arg =
    ResolveM $ \env state ->
      let (f, state') = unResolveM fun env state
          (result, state'') = unResolveM arg env state'
       in (f result, state'')

instance Monad ResolveM where
  action >>= next =
    ResolveM $ \env state ->
      let (result, state') = unResolveM action env state
       in unResolveM (next result) env state'

runResolveM :: Scope -> Int -> ResolveM a -> (Int, [ResolutionAnnotation], a)
runResolveM scope nextLocal action =
  let initialEnv = ResolveEnv {envScope = scope, envSpan = NoSourceSpan}
      initialState = ResolveState {stateNextLocal = nextLocal, stateGeneratedAnnotations = []}
      (result, finalState) = unResolveM action initialEnv initialState
   in (stateNextLocal finalState, reverse (stateGeneratedAnnotations finalState), result)

asks :: (ResolveEnv -> a) -> ResolveM a
asks f = ResolveM $ \env state -> (f env, state)

gets :: (ResolveState -> a) -> ResolveM a
gets f = ResolveM $ \_ state -> (f state, state)

modify' :: (ResolveState -> ResolveState) -> ResolveM ()
modify' f =
  ResolveM $ \_ state ->
    let state' = f state
     in state' `seq` ((), state')

local :: (ResolveEnv -> ResolveEnv) -> ResolveM a -> ResolveM a
local f action =
  ResolveM $ \env state -> unResolveM action (f env) state

currentScope :: ResolveM Scope
currentScope = asks envScope

currentSpan :: ResolveM SourceSpan
currentSpan = asks envSpan

withScope :: Scope -> ResolveM a -> ResolveM a
withScope scope = local (\env -> env {envScope = scope})

extendScope :: Scope -> ResolveM a -> ResolveM a
extendScope localScope = local (\env -> env {envScope = localScope `unionScope` envScope env})

withAmbientSpan :: SourceSpan -> ResolveM a -> ResolveM a
withAmbientSpan span' = local (\env -> env {envSpan = span'})

withEffectiveSpan :: SourceSpan -> ResolveM a -> ResolveM a
withEffectiveSpan localSpan action = do
  ambient <- currentSpan
  withAmbientSpan (effectiveResolutionSpan ambient localSpan) action

withPushedSpan :: Annotation -> ResolveM a -> ResolveM a
withPushedSpan ann action = do
  ambient <- currentSpan
  withAmbientSpan (pushSpanFromAnn ambient ann) action

freshLocal :: UnqualifiedName -> ResolveM ResolvedName
freshLocal name = do
  currentId <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = currentId + 1})
  pure (ResolvedLocal currentId name)

withLocalSupply :: Int -> ResolveM a -> ResolveM a
withLocalSupply nextLocal action = do
  savedNextLocal <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = nextLocal})
  result <- action
  modify' (\state -> state {stateNextLocal = savedNextLocal})
  pure result

emitAnnotation :: ResolutionAnnotation -> ResolveM ()
emitAnnotation annotation =
  modify' (\state -> state {stateGeneratedAnnotations = annotation : stateGeneratedAnnotations state})

emitAnnotations :: [ResolutionAnnotation] -> ResolveM ()
emitAnnotations = mapM_ emitAnnotation

resolve :: [Module] -> ResolveResult
resolve = resolveWithDeps Map.empty

collectResolveErrors :: (Data a) => a -> [ResolveError]
collectResolveErrors node =
  ownResolveErrors node <> concat (gmapQ collectResolveErrors node)

ownResolveErrors :: (Data a) => a -> [ResolveError]
ownResolveErrors node =
  declResolutionErrors (cast node)
    <> classDeclItemResolutionErrors (cast node)
    <> importResolutionErrors (cast node)
    <> patternResolutionErrors (cast node)
    <> typeResolutionErrors (cast node)
    <> exprResolutionErrors (cast node)

declResolutionErrors :: Maybe Decl -> [ResolveError]
declResolutionErrors maybeDecl =
  case maybeDecl of
    Just (DeclResolution resolution) -> maybeToList (annotationResolveError resolution)
    _ -> []

classDeclItemResolutionErrors :: Maybe ClassDeclItem -> [ResolveError]
classDeclItemResolutionErrors maybeItem =
  case maybeItem of
    Just (ClassItemAnn ann _) -> maybeToList (fromAnnotation ann >>= annotationResolveError)
    _ -> []

importResolutionErrors :: Maybe ImportDecl -> [ResolveError]
importResolutionErrors maybeImport =
  case maybeImport of
    Just importDecl -> mapMaybe annotationResolveError (mapMaybe fromAnnotation (importDeclAnns importDecl))
    _ -> []

patternResolutionErrors :: Maybe Pattern -> [ResolveError]
patternResolutionErrors maybePattern =
  case maybePattern of
    Just (PResolution resolution) -> maybeToList (annotationResolveError resolution)
    _ -> []

typeResolutionErrors :: Maybe Type -> [ResolveError]
typeResolutionErrors maybeType =
  case maybeType of
    Just (TResolution resolution) -> maybeToList (annotationResolveError resolution)
    _ -> []

exprResolutionErrors :: Maybe Expr -> [ResolveError]
exprResolutionErrors maybeExpr =
  case maybeExpr of
    Just (EResolution resolution) -> maybeToList (annotationResolveError resolution)
    _ -> []

annotationResolveError :: ResolutionAnnotation -> Maybe ResolveError
annotationResolveError resolution =
  case resolutionTarget resolution of
    ResolvedError msg ->
      Just
        ResolveResolutionError
          { resolveErrorSpan = resolutionSpan resolution,
            resolveErrorName = resolutionName resolution,
            resolveErrorNamespace = resolutionNamespace resolution,
            resolveErrorMessage = msg
          }
    ResolvedBuiltin _ -> Nothing
    _ -> Nothing

resolveWithDeps :: ModuleExports -> [Module] -> ResolveResult
resolveWithDeps depExports modules =
  ResolveResult
    { resolvedModules = modules',
      resolvedAnnotations = extraAnnotations,
      resolveErrors = collectResolveErrors modules' <> concatMap (mapMaybe annotationResolveError . snd) extraAnnotations
    }
  where
    step currentNextLocal modu =
      let (nextLocal', annotations, modu') = resolveModule exports currentNextLocal modu
       in (nextLocal', (annotations, modu'))
    (_, resolved) = mapAccumL step 0 modules
    modules' = map snd resolved
    extraAnnotations = map (\(annotations, modu) -> (moduleKey modu, annotations)) resolved
    ownExports = collectModuleExports modules
    exports = ownExports `Map.union` depExports

extractInterface :: ResolveResult -> ModuleExports
extractInterface = collectModuleExports . resolvedModules

resolveModule :: ModuleExports -> Int -> Module -> (Int, [ResolutionAnnotation], Module)
resolveModule exports nextLocal modu =
  let imports' = resolveModuleImports exports (moduleImports modu)
      modu' = modu {moduleImports = imports'}
      scope = moduleScope exports modu'
      (nextLocal', annotations, decls') = runResolveM scope nextLocal (resolveTopLevelDecls Map.empty (moduleDecls modu))
   in (nextLocal', annotations, modu' {moduleDecls = decls'})

resolveModuleImports :: ModuleExports -> [ImportDecl] -> [ImportDecl]
resolveModuleImports exports =
  map resolveModuleImport
  where
    resolveModuleImport importDecl
      | Just originScope <- Map.lookup (importDeclModule importDecl) exports =
          annotateImportErrors (missingImportItemAnnotations originScope importDecl) importDecl
      | otherwise = annotateImport (missingModuleImportAnnotation importDecl) importDecl

missingModuleImportAnnotation :: ImportDecl -> ResolutionAnnotation
missingModuleImportAnnotation importDecl =
  let importedModule = importDeclModule importDecl
   in ResolutionAnnotation
        (importModuleNameSpan importDecl)
        importedModule
        ResolutionNamespaceModule
        (ResolvedError "not found")

missingImportItemAnnotations :: Scope -> ImportDecl -> [ResolutionAnnotation]
missingImportItemAnnotations originScope importDecl =
  case importDeclSpec importDecl of
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      mapMaybe (missingImportItemAnnotation originScope) importSpecItems
    _ -> []

missingImportItemAnnotation :: Scope -> ImportItem -> Maybe ResolutionAnnotation
missingImportItemAnnotation originScope item =
  go item
  where
    go current =
      case current of
        ImportAnn _ sub -> go sub
        ImportItemVar _ itemName ->
          missingImportedName item ResolutionNamespaceTerm itemName (scopeTerms originScope)
        ImportItemAbs _ itemName ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
        ImportItemAll _ itemName ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
        ImportItemWith _ itemName members ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
            <|> missingImportMemberAnnotation originScope item members
        ImportItemAllWith _ itemName _ members ->
          missingImportedName item ResolutionNamespaceType itemName (scopeTypes originScope)
            <|> missingImportMemberAnnotation originScope item members

missingImportMemberAnnotation :: Scope -> ImportItem -> [IEBundledMember] -> Maybe ResolutionAnnotation
missingImportMemberAnnotation originScope item members =
  missingMemberAnnotation <$> find missingMember members
  where
    missingMember member = nameText (ieBundledMemberName member) `notElem` exportedMembers
    exportedMembers =
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let parentName = renderUnqualifiedName itemName
           in Map.findWithDefault [] parentName (scopeConstructors originScope)
                <> Map.findWithDefault [] parentName (scopeMethods originScope)
    missingMemberAnnotation member =
      let memberName = nameText (ieBundledMemberName member)
       in ResolutionAnnotation
            (importMemberNameSpan (peelImportItemSpan NoSourceSpan item) memberName)
            memberName
            ResolutionNamespaceTerm
            (ResolvedError "not exported")

missingImportedName :: ImportItem -> ResolutionNamespace -> UnqualifiedName -> Map.Map Text ResolvedName -> Maybe ResolutionAnnotation
missingImportedName item namespace itemName candidates
  | Map.member rendered candidates = Nothing
  | otherwise =
      Just
        ( ResolutionAnnotation
            (spanStartNameSpan (peelImportItemSpan NoSourceSpan item) rendered)
            rendered
            namespace
            (ResolvedError "not exported")
        )
  where
    rendered = renderUnqualifiedName itemName

importMemberNameSpan :: SourceSpan -> Text -> SourceSpan
importMemberNameSpan itemSpan memberName =
  case itemSpan of
    SourceSpan sourceName startLine startCol endLine endCol startOffset endOffset ->
      let width = T.length memberName
       in SourceSpan
            sourceName
            startLine
            (max startCol (endCol - width - 1))
            endLine
            endCol
            startOffset
            endOffset
    NoSourceSpan -> NoSourceSpan

resolveTopLevelDecls :: Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveTopLevelDecls _ [] = pure []
resolveTopLevelDecls signatureScopes (decl : rest) = do
  (signatureScopes', decl') <- resolveTopLevelDecl signatureScopes decl
  decls' <- resolveTopLevelDecls signatureScopes' rest
  pure (decl' : decls')

resolveTopLevelDecl :: Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveTopLevelDecl signatureScopes decl = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  (signatureScopes', decl') <- withScope scoped (resolveDeclWithSignatureScope signatureScopes decl)
  emitAnnotations (topLevelDeclAnnotations decl scope)
  let decl'' = maybe decl' (`annotateDecl` decl') (topLevelBinderAnnotation decl scope)
  pure (signatureScopes', decl'')

resolveDeclWithSignatureScope :: Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveDeclWithSignatureScope signatureScopes decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann $ do
        (signatureScopes', inner') <- resolveDeclWithSignatureScope signatureScopes inner
        pure (signatureScopes', DeclAnn ann inner')
    DeclTypeSig names ty -> do
      (binderScope, ty') <- resolveTypeSignature ty
      let signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
      pure (signatureScopes', DeclTypeSig names ty')
    _ -> do
      decl' <- resolveDecl decl
      let signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
      pure (signatureScopes', decl')

resolveDecl :: Decl -> ResolveM Decl
resolveDecl (DeclAnn ann inner) =
  withPushedSpan ann (resolveDecl inner)
resolveDecl decl =
  resolveDeclCore decl

resolveDeclCore :: Decl -> ResolveM Decl
resolveDeclCore decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann (resolveDeclCore inner)
    DeclValue valueDecl ->
      DeclValue <$> resolveValueDecl valueDecl
    DeclTypeSig names ty -> do
      ty' <- resolveType ty
      pure (DeclTypeSig names ty')
    DeclStandaloneKindSig {} ->
      annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeData dataDecl ->
      DeclTypeData <$> resolveDataDecl dataDecl
    DeclData dataDecl ->
      DeclData <$> resolveDataDecl dataDecl
    DeclTypeSyn {} ->
      annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclSplice expr -> DeclSplice <$> resolveExpr expr
    DeclNewtype newtypeDecl -> do
      kind' <- traverse resolveType (newtypeDeclKind newtypeDecl)
      constructor' <- traverse resolveDataConDecl (newtypeDeclConstructor newtypeDecl)
      pure (DeclNewtype (newtypeDecl {newtypeDeclKind = kind', newtypeDeclConstructor = constructor'}))
    DeclClass classDecl ->
      DeclClass <$> resolveClassDecl classDecl
    DeclDefault tys ->
      DeclDefault <$> mapM resolveType tys
    DeclFixity {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclRoleAnnotation {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPragma {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPatSyn {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPatSynSig {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclInstance {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclStandaloneDeriving {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclForeign {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeFamilyDecl {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclDataFamilyDecl {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeFamilyInst {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclDataFamilyInst {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl

resolveValueDecl :: ValueDecl -> ResolveM ValueDecl
resolveValueDecl valueDecl =
  case valueDecl of
    FunctionBind name matches ->
      FunctionBind name <$> mapM resolveMatch matches
    PatternBind multTag pat rhs ->
      PatternBind multTag <$> resolvePatternSyntax pat <*> resolveRhs rhs

resolveClassDecl :: ClassDecl -> ResolveM ClassDecl
resolveClassDecl classDecl = do
  context' <- traverse (mapM resolveType) (classDeclContext classDecl)
  items' <- mapM resolveClassDeclItem (classDeclItems classDecl)
  pure
    classDecl
      { classDeclContext = context',
        classDeclItems = items'
      }

resolveClassDeclItem :: ClassDeclItem -> ResolveM ClassDeclItem
resolveClassDeclItem classDeclItem =
  case classDeclItem of
    ClassItemAnn ann inner -> ClassItemAnn ann <$> withPushedSpan ann (resolveClassDeclItem inner)
    ClassItemTypeSig names ty -> ClassItemTypeSig names <$> resolveType ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name <$> resolveType ty
    ClassItemDefault valueDecl -> ClassItemDefault <$> withLocalSupply 0 (resolveValueDecl valueDecl)
    ClassItemFixity {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemPragma {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemTypeFamilyDecl {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemDataFamilyDecl {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemDefaultTypeInst {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem

resolveMatch :: Match -> ResolveM Match
resolveMatch match =
  withEffectiveSpan (sourceSpanFromAnns (matchAnns match)) $ do
    (patScope, pats') <- bindPatterns (matchPats match)
    rhsHere <- effectiveResolutionSpan <$> currentSpan <*> pure (rhsSpan (matchRhs match))
    rhs' <- extendScope patScope (withAmbientSpan rhsHere (resolveRhs (matchRhs match)))
    pure match {matchPats = pats', matchRhs = rhs'}

resolveRhs :: Rhs Expr -> ResolveM (Rhs Expr)
resolveRhs rhs =
  case rhs of
    UnguardedRhs anns expr mDecls ->
      withEffectiveSpan (sourceSpanFromAnns anns) $ do
        -- Pre-allocate where-clause binders so the body can reference them.
        (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
        expr' <- extendScope localScope (resolveExpr expr)
        mDecls' <- case mDecls of
          Nothing -> pure Nothing
          Just decls -> Just <$> extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (UnguardedRhs anns expr' mDecls')
    GuardedRhss anns guardedRhss mDecls ->
      withEffectiveSpan (sourceSpanFromAnns anns) $ do
        -- Pre-allocate where-clause binders so guards can reference them.
        (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
        guardedRhss' <- extendScope localScope (mapM resolveGuardedRhs guardedRhss)
        mDecls' <- case mDecls of
          Nothing -> pure Nothing
          Just decls -> Just <$> extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (GuardedRhss anns guardedRhss' mDecls')

resolveGuardedRhs :: GuardedRhs Expr -> ResolveM (GuardedRhs Expr)
resolveGuardedRhs guardedRhs =
  withEffectiveSpan (sourceSpanFromAnns (guardedRhsAnns guardedRhs)) $ do
    (scope', guards') <- resolveGuardQualifiers (guardedRhsGuards guardedRhs)
    body' <- withScope scope' (resolveExpr (guardedRhsBody guardedRhs))
    pure guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'}

resolveGuardQualifiers :: [GuardQualifier] -> ResolveM (Scope, [GuardQualifier])
resolveGuardQualifiers qualifiers = do
  scope <- currentScope
  go scope qualifiers
  where
    go scope qualifiers' =
      withScope scope $
        case qualifiers' of
          [] -> pure (scope, [])
          qualifier : rest -> do
            (scope', qualifier') <- resolveGuardQualifier qualifier
            (scope'', rest') <- go scope' rest
            pure (scope'', qualifier' : rest')

resolveGuardQualifier :: GuardQualifier -> ResolveM (Scope, GuardQualifier)
resolveGuardQualifier qualifier =
  withEffectiveSpan (peelGuardQualifierSpan NoSourceSpan qualifier) $ do
    scope <- currentScope
    let qualifierSpan = peelGuardQualifierSpan NoSourceSpan qualifier
        wrap = GuardAnn (mkAnnotation qualifierSpan)
    case peelGuardQualifierAnn qualifier of
      GuardExpr expr -> do
        expr' <- resolveExpr expr
        pure (scope, wrap (GuardExpr expr'))
      GuardPat pat expr -> do
        expr' <- resolveExpr expr
        (patScope, pat') <- bindPattern pat
        pure (unionScope patScope scope, wrap (GuardPat pat' expr'))
      GuardLet decls -> do
        (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
        decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (unionScope localScope scope, wrap (GuardLet decls'))
      GuardAnn _ _ -> pure (scope, qualifier)

resolveExpr :: Expr -> ResolveM Expr
resolveExpr expr =
  case expr of
    EAnn ann inner ->
      withPushedSpan ann (resolveExpr inner)
    EVar name -> do
      sp <- currentSpan
      scope <- currentScope
      pure
        ( annotateExpr
            (ResolutionAnnotation sp (nameText name) ResolutionNamespaceTerm (resolveTermName scope name))
            (EVar name)
        )
    ETypeSyntax form ty -> ETypeSyntax form <$> resolveType ty
    EInt {} -> pure expr
    EFloat {} -> pure expr
    EChar {} -> pure expr
    ECharHash {} -> pure expr
    EString {} -> pure expr
    EStringHash {} -> pure expr
    EOverloadedLabel {} -> pure expr
    EIf cond trueBranch falseBranch ->
      EIf <$> resolveExpr cond <*> resolveExpr trueBranch <*> resolveExpr falseBranch
    EMultiWayIf guardedRhss ->
      EMultiWayIf <$> mapM resolveGuardedRhs guardedRhss
    ELambdaPats pats body -> do
      (patScope, pats') <- bindPatterns pats
      body' <- extendScope patScope (resolveExpr body)
      pure (ELambdaPats pats' body')
    ELambdaCase alts ->
      ELambdaCase <$> mapM resolveCaseAlt alts
    ELambdaCases alts ->
      ELambdaCases <$> mapM resolveLambdaCaseAlt alts
    EInfix left op right ->
      EInfix <$> resolveExpr left <*> pure op <*> resolveExpr right
    ENegate inner ->
      ENegate <$> resolveExpr inner
    ESectionL inner op ->
      ESectionL <$> resolveExpr inner <*> pure op
    ESectionR op inner ->
      ESectionR op <$> resolveExpr inner
    ELetDecls decls body -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      body' <- extendScope localScope (resolveExpr body)
      pure (ELetDecls decls' body')
    ECase scrutinee alts ->
      ECase <$> resolveExpr scrutinee <*> mapM resolveCaseAlt alts
    EArithSeq arithSeq ->
      EArithSeq <$> resolveArithSeq arithSeq
    ERecordCon name fields wildcard ->
      ERecordCon name <$> resolveRecordFields fields <*> pure wildcard
    ERecordUpd record fields ->
      ERecordUpd <$> resolveExpr record <*> resolveRecordFields fields
    EGetField record name ->
      EGetField <$> resolveExpr record <*> pure name
    EGetFieldProjection {} -> pure expr
    ETypeSig inner ty ->
      ETypeSig <$> resolveExpr inner <*> resolveType ty
    EParen inner ->
      EParen <$> resolveExpr inner
    EList items ->
      EList <$> mapM resolveExpr items
    ETuple flavor items ->
      ETuple flavor <$> mapM resolveMaybeExpr items
    EUnboxedSum alt arity inner ->
      EUnboxedSum alt arity <$> resolveExpr inner
    ETypeApp fun ty ->
      ETypeApp <$> resolveExpr fun <*> resolveType ty
    EApp fun arg ->
      EApp <$> resolveExpr fun <*> resolveExpr arg
    ETHSplice inner ->
      ETHSplice <$> resolveExpr inner
    ETHTypedSplice inner ->
      ETHTypedSplice <$> resolveExpr inner
    EPragma pragma inner ->
      EPragma pragma <$> resolveExpr inner
    EDo stmts flavor -> do
      (_, stmts') <- resolveDoStmts stmts
      pure (EDo stmts' flavor)
    EQuasiQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    EListComp {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    EListCompParallel {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHExpQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHTypedQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHDeclQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHTypeQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHPatQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHNameQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    ETHTypeNameQuote {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr
    EProc {} -> annotateUnhandledExpr <$> currentSpan <*> pure expr

resolveMaybeExpr :: Maybe Expr -> ResolveM (Maybe Expr)
resolveMaybeExpr = traverse resolveExpr

resolveCaseAlt :: CaseAlt Expr -> ResolveM (CaseAlt Expr)
resolveCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (caseAltAnns alt)) $ do
    (patScope, pat') <- bindPattern (caseAltPattern alt)
    rhs' <- extendScope patScope (resolveRhs (caseAltRhs alt))
    pure alt {caseAltPattern = pat', caseAltRhs = rhs'}

resolveLambdaCaseAlt :: LambdaCaseAlt -> ResolveM LambdaCaseAlt
resolveLambdaCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (lambdaCaseAltAnns alt)) $ do
    (patScope, pats') <- bindPatterns (lambdaCaseAltPats alt)
    rhs' <- extendScope patScope (resolveRhs (lambdaCaseAltRhs alt))
    pure alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = rhs'}

resolveRecordFields :: [RecordField Expr] -> ResolveM [RecordField Expr]
resolveRecordFields =
  mapM
    ( \field -> do
        value' <- resolveExpr (recordFieldValue field)
        pure field {recordFieldValue = value'}
    )

resolveDoStmts :: [DoStmt Expr] -> ResolveM (Scope, [DoStmt Expr])
resolveDoStmts stmts = do
  scope <- currentScope
  go scope stmts
  where
    go scope stmts' =
      withScope scope $
        case stmts' of
          [] -> pure (scope, [])
          stmt : rest -> do
            (scope', stmt') <- resolveDoStmt stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveDoStmt :: DoStmt Expr -> ResolveM (Scope, DoStmt Expr)
resolveDoStmt stmt =
  case stmt of
    DoAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveDoStmt inner)
      pure (scope', DoAnn ann inner')
    DoExpr body -> do
      scope <- currentScope
      body' <- resolveExpr body
      pure (scope, DoExpr body')
    DoBind pat body -> do
      scope <- currentScope
      body' <- resolveExpr body
      (patScope, pat') <- bindPattern pat
      pure (unionScope patScope scope, DoBind pat' body')
    DoLetDecls decls -> do
      scope <- currentScope
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, DoLetDecls decls')
    DoRecStmt stmts -> do
      scope <- currentScope
      (_, stmts') <- resolveDoStmts stmts
      pure (scope, DoRecStmt stmts')

resolveArithSeq :: ArithSeq -> ResolveM ArithSeq
resolveArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner ->
      ArithSeqAnn ann <$> withPushedSpan ann (resolveArithSeq inner)
    ArithSeqFrom from ->
      ArithSeqFrom <$> resolveExpr from
    ArithSeqFromThen from then' ->
      ArithSeqFromThen <$> resolveExpr from <*> resolveExpr then'
    ArithSeqFromTo from to ->
      ArithSeqFromTo <$> resolveExpr from <*> resolveExpr to
    ArithSeqFromThenTo from then' to ->
      ArithSeqFromThenTo <$> resolveExpr from <*> resolveExpr then' <*> resolveExpr to

resolveBoundDecl :: Map.Map Text ResolutionAnnotation -> Decl -> ResolveM Decl
resolveBoundDecl binderAnnotations decl = do
  decl' <- resolveDecl decl
  pure (maybe decl' (`annotateDecl` decl') (declBinderAnnotation decl binderAnnotations))

resolveBoundDecls :: Map.Map Text ResolutionAnnotation -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBoundDecls _ _ [] = pure []
resolveBoundDecls binderAnnotations signatureScopes (decl : rest) = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  (signatureScopes', decl') <- withScope scoped (resolveBoundDeclWithSignatureScope binderAnnotations signatureScopes decl)
  decls' <- resolveBoundDecls binderAnnotations signatureScopes' rest
  pure (decl' : decls')

resolveBoundDeclWithSignatureScope :: Map.Map Text ResolutionAnnotation -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveBoundDeclWithSignatureScope binderAnnotations signatureScopes decl =
  case decl of
    DeclTypeSig names ty -> do
      (binderScope, ty') <- resolveTypeSignature ty
      let signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
          resolvedDecl = DeclTypeSig names ty'
          annotatedDecl = maybe resolvedDecl (`annotateDecl` resolvedDecl) (declBinderAnnotation decl binderAnnotations)
      pure (signatureScopes', annotatedDecl)
    _ -> do
      decl' <- resolveBoundDecl binderAnnotations decl
      let signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
      pure (signatureScopes', decl')

declSignatureScope :: Decl -> Map.Map Text Scope -> Maybe Scope
declSignatureScope decl signatureScopes =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) signatureScopes
    Nothing -> Nothing

bindPatterns :: [Pattern] -> ResolveM (Scope, [Pattern])
bindPatterns pats = do
  (scopes, pats') <- mapAndUnzipM bindPattern pats
  pure (foldr unionScope emptyScope scopes, pats')

bindPattern :: Pattern -> ResolveM (Scope, Pattern)
bindPattern pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann (bindPattern inner)
    PVar name -> do
      sp <- currentSpan
      resolvedName <- freshLocal name
      let key = renderUnqualifiedName name
          annotation = ResolutionAnnotation sp (renderUnqualifiedName name) ResolutionNamespaceTerm resolvedName
      pure (termScope key resolvedName, annotatePattern annotation (PVar name))
    PTypeBinder binder -> do
      let binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
      resolvedName <- freshLocal binderName
      binder' <- traverseTyVarBinderKind binder
      let binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) resolvedName) Map.empty Map.empty Map.empty Map.empty
      pure (binderScope, PTypeBinder binder')
    PTypeSyntax form ty -> do
      ty' <- resolveType ty
      pure (emptyScope, PTypeSyntax form ty')
    PWildcard -> pure (emptyScope, pat)
    PLit {} -> pure (emptyScope, pat)
    PTuple flavor pats -> do
      (scope, pats') <- bindPatterns pats
      pure (scope, PTuple flavor pats')
    PUnboxedSum alt arity inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PUnboxedSum alt arity inner')
    PList pats -> do
      (scope, pats') <- bindPatterns pats
      pure (scope, PList pats')
    PCon name typeArgs pats -> do
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, PCon name typeArgs' pats')
    PInfix left name right -> do
      (leftScope, left') <- bindPattern left
      (rightScope, right') <- bindPattern right
      pure (unionScope rightScope leftScope, PInfix left' name right')
    PView expr inner -> do
      expr' <- resolveExpr expr
      (scope, inner') <- bindPattern inner
      pure (scope, PView expr' inner')
    PAs alias inner -> do
      here <- currentSpan
      let aliasKey = renderUnqualifiedName alias
      aliasResolved <- freshLocal alias
      let aliasAnnotation =
            ResolutionAnnotation (spanStartNameSpan here aliasKey) aliasKey ResolutionNamespaceTerm aliasResolved
          aliasScope = termScope aliasKey aliasResolved
      (innerScope, inner') <- bindPattern inner
      pure (unionScope innerScope aliasScope, annotatePattern aliasAnnotation (PAs alias inner'))
    PStrict inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PStrict inner')
    PIrrefutable inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PIrrefutable inner')
    PParen inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, PParen inner')
    PRecord name fields wildcard -> do
      (fieldScopes, fields') <-
        mapAndUnzipM
          ( \field -> do
              (fieldScope, fieldPat') <- bindPattern (recordFieldValue field)
              pure (fieldScope, field {recordFieldValue = fieldPat'})
          )
          fields
      wildcardEntries <- bindRecordWildcardFields name fields wildcard
      let wildcardScope = Scope (Map.fromList wildcardEntries) Map.empty Map.empty Map.empty Map.empty Map.empty
      pure (foldr unionScope wildcardScope fieldScopes, PRecord name fields' wildcard)
    PTypeSig inner ty -> do
      (scope, inner') <- bindPattern inner
      ty' <- resolveType ty
      pure (scope, PTypeSig inner' ty')
    PNegLit {} -> pure (emptyScope, pat)
    PSplice expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, PSplice expr')
    PQuasiQuote {} -> do
      sp <- currentSpan
      pure (emptyScope, annotateUnhandledPattern sp pat)
  where
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

termScope :: Text -> ResolvedName -> Scope
termScope key resolvedName =
  Scope (Map.singleton key resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty

resolvePatternSyntax :: Pattern -> ResolveM Pattern
resolvePatternSyntax pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann (resolvePatternSyntax inner)
    PVar {} -> pure pat
    PTypeBinder binder -> do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure (PTypeBinder (binder {tyVarBinderKind = kind'}))
    PTypeSyntax form ty ->
      PTypeSyntax form <$> resolveType ty
    PWildcard -> pure pat
    PLit {} -> pure pat
    PQuasiQuote {} -> annotateUnhandledPattern <$> currentSpan <*> pure pat
    PTuple flavor pats ->
      PTuple flavor <$> mapM resolvePatternSyntax pats
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity <$> resolvePatternSyntax inner
    PList pats ->
      PList <$> mapM resolvePatternSyntax pats
    PCon name typeArgs pats ->
      PCon name <$> mapM resolveType typeArgs <*> mapM resolvePatternSyntax pats
    PInfix left name right ->
      PInfix <$> resolvePatternSyntax left <*> pure name <*> resolvePatternSyntax right
    PView expr inner ->
      PView <$> withLocalSupply 0 (resolveExpr expr) <*> resolvePatternSyntax inner
    PAs alias inner ->
      PAs alias <$> resolvePatternSyntax inner
    PStrict inner ->
      PStrict <$> resolvePatternSyntax inner
    PIrrefutable inner ->
      PIrrefutable <$> resolvePatternSyntax inner
    PNegLit {} -> pure pat
    PParen inner ->
      PParen <$> resolvePatternSyntax inner
    PRecord name fields wildcard ->
      PRecord name
        <$> mapM
          ( \field -> do
              value' <- resolvePatternSyntax (recordFieldValue field)
              pure field {recordFieldValue = value'}
          )
          fields
        <*> pure wildcard
    PTypeSig inner ty ->
      PTypeSig <$> resolvePatternSyntax inner <*> resolveType ty
    PSplice expr ->
      PSplice <$> withLocalSupply 0 (resolveExpr expr)

bindRecordWildcardFields :: Name -> [RecordField Pattern] -> Bool -> ResolveM [(Text, ResolvedName)]
bindRecordWildcardFields conName fields wildcard
  | not wildcard = pure []
  | otherwise =
      mapM bindField =<< wildcardFields
  where
    wildcardFields = do
      scope <- currentScope
      let explicitFields = map (nameText . recordFieldName) fields
      pure
        ( filter (`notElem` explicitFields) $
            Map.findWithDefault [] (nameText conName) (scopeRecordFields scope)
        )
    bindField fieldName = do
      let binder = mkUnqualifiedName NameVarId fieldName
      resolvedName <- freshLocal binder
      pure (fieldName, resolvedName)

resolveDataDecl :: DataDecl -> ResolveM DataDecl
resolveDataDecl dataDecl = do
  context' <- mapM resolveType (dataDeclContext dataDecl)
  kind' <- traverse resolveType (dataDeclKind dataDecl)
  constructors' <- mapM resolveDataConDecl (dataDeclConstructors dataDecl)
  pure
    dataDecl
      { dataDeclContext = context',
        dataDeclKind = kind',
        dataDeclConstructors = constructors'
      }

resolveDataConDecl :: DataConDecl -> ResolveM DataConDecl
resolveDataConDecl dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> DataConAnn ann <$> withPushedSpan ann (resolveDataConDecl inner)
    PrefixCon forallVars context name bangTypes ->
      PrefixCon forallVars <$> mapM resolveType context <*> pure name <*> mapM resolveBangType bangTypes
    InfixCon forallVars context lhs name rhs ->
      InfixCon forallVars <$> mapM resolveType context <*> resolveBangType lhs <*> pure name <*> resolveBangType rhs
    RecordCon forallVars context name fields ->
      RecordCon forallVars <$> mapM resolveType context <*> pure name <*> mapM resolveFieldDecl fields
    GadtCon forallVars context names body ->
      GadtCon forallVars <$> mapM resolveType context <*> pure names <*> resolveGadtBody body
    TupleCon forallVars context flavor fields ->
      TupleCon forallVars <$> mapM resolveType context <*> pure flavor <*> mapM resolveBangType fields
    UnboxedSumCon forallVars context pos arity field ->
      UnboxedSumCon forallVars <$> mapM resolveType context <*> pure pos <*> pure arity <*> resolveBangType field
    ListCon forallVars context ->
      ListCon forallVars <$> mapM resolveType context
  where
    resolveBangType bt = do
      ty' <- resolveType (bangType bt)
      pure bt {bangType = ty'}
    resolveFieldDecl fieldDecl = do
      fieldType' <- resolveBangType (fieldType fieldDecl)
      pure fieldDecl {fieldType = fieldType'}

resolveGadtBody :: GadtBody -> ResolveM GadtBody
resolveGadtBody body =
  case body of
    GadtPrefixBody bangTypes ty ->
      GadtPrefixBody <$> mapM resolveBangTypePair bangTypes <*> resolveType ty
    GadtRecordBody fields ty ->
      GadtRecordBody <$> mapM resolveFieldDecl fields <*> resolveType ty
  where
    resolveBangTypePair (bt, arrowKind) = do
      bt' <- resolveBangType bt
      pure (bt', arrowKind)
    resolveBangType bt = do
      ty' <- resolveType (bangType bt)
      pure bt {bangType = ty'}
    resolveFieldDecl fieldDecl = do
      fieldType' <- resolveBangType (fieldType fieldDecl)
      pure fieldDecl {fieldType = fieldType'}

resolveType :: Type -> ResolveM Type
resolveType ty =
  case ty of
    TAnn ann inner -> withPushedSpan ann (resolveType inner)
    TVar name -> do
      sp <- currentSpan
      scope <- currentScope
      let resolvedTyVar = TVar name
      pure (maybe resolvedTyVar (`annotateType` resolvedTyVar) (resolveScopedTypeVarAnnotation scope sp name))
    TCon name promoted -> do
      sp <- currentSpan
      scope <- currentScope
      pure
        ( annotateType
            (ResolutionAnnotation sp (nameText name) ResolutionNamespaceType (resolveTypeName scope name))
            (TCon name promoted)
        )
    TBuiltinCon {} -> pure ty
    TImplicitParam name inner ->
      TImplicitParam name <$> resolveType inner
    TTypeLit {} -> pure ty
    TStar -> pure ty
    TForall telescope inner -> do
      (binderScope, binders') <- withLocalSupply 0 (bindTyVarBinders (forallTelescopeBinders telescope))
      inner' <- extendScope binderScope (resolveType inner)
      pure (TForall (telescope {forallTelescopeBinders = binders'}) inner')
    TApp left right ->
      TApp <$> resolveType left <*> resolveType right
    TTypeApp left right ->
      TTypeApp <$> resolveType left <*> resolveType right
    TInfix left name promoted right ->
      TInfix <$> resolveType left <*> pure name <*> pure promoted <*> resolveType right
    TFun arrowKind left right ->
      TFun <$> resolveArrowKind arrowKind <*> resolveType left <*> resolveType right
    TTuple flavor promoted items ->
      TTuple flavor promoted <$> mapM resolveType items
    TUnboxedSum items ->
      TUnboxedSum <$> mapM resolveType items
    TList promoted items ->
      TList promoted <$> mapM resolveType items
    TParen inner ->
      TParen <$> resolveType inner
    TKindSig inner kind ->
      TKindSig <$> resolveType inner <*> resolveType kind
    TContext constraints inner ->
      TContext <$> mapM resolveType constraints <*> resolveType inner
    TSplice expr ->
      TSplice <$> withLocalSupply 0 (resolveExpr expr)
    TWildcard -> pure ty
    TQuasiQuote {} -> annotateUnhandledType <$> currentSpan <*> pure ty

resolveArrowKind :: ArrowKind -> ResolveM ArrowKind
resolveArrowKind arrowKind =
  case arrowKind of
    ArrowUnrestricted -> pure arrowKind
    ArrowLinear -> pure arrowKind
    ArrowExplicit ty -> ArrowExplicit <$> resolveType ty

resolveScopedTypeVarAnnotation :: Scope -> SourceSpan -> UnqualifiedName -> Maybe ResolutionAnnotation
resolveScopedTypeVarAnnotation scope span' name =
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
   in case resolved of
        ResolvedError _ -> Nothing
        _ -> Just (ResolutionAnnotation span' rendered ResolutionNamespaceType resolved)

resolveTypeSignature :: Type -> ResolveM (Scope, Type)
resolveTypeSignature ty =
  case ty of
    -- Type signatures may carry span-only 'TAnn' wrappers (see 'typeAnnSpan'); peel
    -- them so we still allocate scoped type variables and advance 'nextLocal'.
    TAnn ann sub -> withPushedSpan ann (resolveTypeSignature sub)
    TForall telescope inner -> do
      (binderScope, binders') <- bindTyVarBinders (forallTelescopeBinders telescope)
      inner' <- extendScope binderScope (resolveType inner)
      pure (binderScope, TForall (telescope {forallTelescopeBinders = binders'}) inner')
    _ -> do
      ty' <- resolveType ty
      pure (emptyScope, ty')

bindTyVarBinders :: [TyVarBinder] -> ResolveM (Scope, [TyVarBinder])
bindTyVarBinders =
  foldM step (emptyScope, [])
  where
    step (boundScope, acc) binder = do
      binder' <- extendScope boundScope (traverseTyVarBinderKind binder)
      let binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
      resolvedName <- freshLocal binderName
      let boundScope' = insertType (tyVarBinderName binder) resolvedName boundScope
      pure (boundScope', acc <> [binder'])
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

allocateLocalDeclBinders :: [Decl] -> ResolveM (Map.Map Text ResolutionAnnotation, Scope)
allocateLocalDeclBinders =
  foldM step (Map.empty, emptyScope)
  where
    step acc decl = foldM addBinder acc (declBinderCandidates decl)
    addBinder (annotations, scope) (span', name) = do
      resolvedName <- freshLocal name
      let key = renderUnqualifiedName name
          annotation = ResolutionAnnotation span' key ResolutionNamespaceTerm resolvedName
      pure (Map.insert key annotation annotations, insertTerm key resolvedName scope)

-- | Collect all term binders introduced by a declaration (handles tuple patterns etc.)
declBinderCandidates :: Decl -> [(SourceSpan, UnqualifiedName)]
declBinderCandidates decl =
  let (outerSp, innerDecl) = peelDeclSpan NoSourceSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = effectiveResolutionSpan outerSp NoSourceSpan
               in [(spanStartNameSpan loc (renderUnqualifiedName name), name)]
            PatternBind _ pat _ ->
              let loc = effectiveResolutionSpan outerSp (peelPatternSpan NoSourceSpan pat)
               in collectPatVarBinders loc pat
        DeclTypeSig [name] _ ->
          [(spanStartNameSpan outerSp (renderUnqualifiedName name), name)]
        _ -> []

collectPatVarBinders :: SourceSpan -> Pattern -> [(SourceSpan, UnqualifiedName)]
collectPatVarBinders ambient pat =
  case peelPatternAnn pat of
    PVar name -> [(spanStartNameSpan ambient (renderUnqualifiedName name), name)]
    PTuple _ pats -> concatMap (collectPatVarBinders ambient) pats
    PList pats -> concatMap (collectPatVarBinders ambient) pats
    PParen inner -> collectPatVarBinders ambient inner
    PAs alias inner ->
      (spanStartNameSpan ambient (renderUnqualifiedName alias), alias)
        : collectPatVarBinders ambient inner
    PStrict inner -> collectPatVarBinders ambient inner
    PIrrefutable inner -> collectPatVarBinders ambient inner
    PRecord _ fields _ -> concatMap (collectPatVarBinders ambient . recordFieldValue) fields
    PInfix left _ right ->
      collectPatVarBinders ambient left <> collectPatVarBinders ambient right
    PCon _ _ pats -> concatMap (collectPatVarBinders ambient) pats
    _ -> []

declBinderAnnotation :: Decl -> Map.Map Text ResolutionAnnotation -> Maybe ResolutionAnnotation
declBinderAnnotation decl binderAnnotations =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) binderAnnotations
    Nothing -> Nothing

topLevelBinderAnnotation :: Decl -> Scope -> Maybe ResolutionAnnotation
topLevelBinderAnnotation decl scope =
  case declBinderCandidate decl of
    Just (span', name) ->
      let rendered = renderUnqualifiedName name
       in Just (ResolutionAnnotation span' rendered ResolutionNamespaceTerm (lookupTerm rendered scope))
    Nothing -> Nothing

declBinderCandidate :: Decl -> Maybe (SourceSpan, UnqualifiedName)
declBinderCandidate decl =
  let (outerSp, innerDecl) = peelDeclSpan NoSourceSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = effectiveResolutionSpan outerSp NoSourceSpan
               in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
            PatternBind _ pat _ ->
              case peelPatternAnn pat of
                PVar name ->
                  let loc =
                        effectiveResolutionSpan
                          (effectiveResolutionSpan outerSp NoSourceSpan)
                          (peelPatternSpan NoSourceSpan pat)
                   in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
                _ -> Nothing
        DeclTypeSig [name] _ ->
          Just (spanStartNameSpan outerSp (renderUnqualifiedName name), name)
        _ -> Nothing

topLevelDeclAnnotations :: Decl -> Scope -> [ResolutionAnnotation]
topLevelDeclAnnotations decl scope =
  case peelDeclSpan NoSourceSpan decl of
    (declSpan, DeclClass classDecl) -> [classAnnotation scope declSpan classDecl]
    (declSpan, DeclTypeData dataDecl) -> dataDeclAnnotations declSpan "type data " dataDecl
    (declSpan, DeclData dataDecl) -> dataDeclAnnotations declSpan "data " dataDecl
    (declSpan, DeclNewtype newtypeDecl) ->
      let span' = declSpan
          typeName = binderHeadName (newtypeDeclHead newtypeDecl)
          typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan "newtype " span' (renderUnqualifiedName typeName))
              (renderUnqualifiedName typeName)
              ResolutionNamespaceType
              (resolveTopLevelType scope typeName)
          constructorAnnotations =
            maybe [] (\ctor -> [dataConAnnotation scope ctor]) (newtypeDeclConstructor newtypeDecl)
       in typeAnnotation : constructorAnnotations
    _ -> []
  where
    dataDeclAnnotations declSpan keyword dataDecl =
      let span' = declSpan
          typeName = binderHeadName (dataDeclHead dataDecl)
          typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan keyword span' (renderUnqualifiedName typeName))
              (renderUnqualifiedName typeName)
              ResolutionNamespaceType
              (resolveTopLevelType scope typeName)
       in typeAnnotation : map (dataConAnnotation scope) (dataDeclConstructors dataDecl)

classAnnotation :: Scope -> SourceSpan -> ClassDecl -> ResolutionAnnotation
classAnnotation scope declSpan classDecl =
  let className = binderHeadName (classDeclHead classDecl)
      span' = declSpan
   in ResolutionAnnotation
        (declKeywordNameSpan "class " span' (renderUnqualifiedName className))
        (renderUnqualifiedName className)
        ResolutionNamespaceType
        (resolveTopLevelType scope className)

resolveTopLevelType :: Scope -> UnqualifiedName -> ResolvedName
resolveTopLevelType scope name = lookupType (renderUnqualifiedName name) scope

resolveTopLevelTerm :: Scope -> UnqualifiedName -> ResolvedName
resolveTopLevelTerm scope name = lookupTerm (renderUnqualifiedName name) scope

dataConAnnotation :: Scope -> DataConDecl -> ResolutionAnnotation
dataConAnnotation scope dataConDecl =
  let span' = peelDataConSpan NoSourceSpan dataConDecl
      go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> topLevelNameAnnotation scope span' name
          RecordCon _ _ name _ -> topLevelNameAnnotation scope span' name
          InfixCon _ _ _ name _ -> topLevelNameAnnotation scope span' name
          GadtCon _ _ names _ ->
            case names of
              name : _ -> topLevelNameAnnotation scope span' name
              [] -> ResolutionAnnotation NoSourceSpan "" ResolutionNamespaceTerm (ResolvedError "missing GADT constructor name")
          TupleCon _ _ flavor fields ->
            topLevelNameAnnotation scope span' (tupleConName flavor (length fields))
          UnboxedSumCon _ _ pos arity _ ->
            topLevelNameAnnotation scope span' (unboxedSumConName pos arity)
          ListCon {} ->
            topLevelNameAnnotation scope span' listConName
   in go dataConDecl

topLevelNameAnnotation :: Scope -> SourceSpan -> UnqualifiedName -> ResolutionAnnotation
topLevelNameAnnotation scope span' name =
  ResolutionAnnotation
    (spanStartNameSpan span' (renderUnqualifiedName name))
    (renderUnqualifiedName name)
    ResolutionNamespaceTerm
    (resolveTopLevelTerm scope name)

collectModuleExports :: [Module] -> ModuleExports
collectModuleExports modules =
  Map.fromList
    [ (moduleKey modu, topLevelScope modu)
    | modu <- modules
    ]

topLevelScope :: Module -> Scope
topLevelScope modu =
  foldl' addDecl emptyScope (moduleDecls modu)
  where
    moduleKeyText = moduleKey modu
    qualify = ResolvedTopLevel . (`mkQualifiedName` Just moduleKeyText)
    addDecl scope decl =
      let DeclExports termNames typeNames constructors recordFields methods = declExportedNames decl
          scope' = foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
          scope'' = foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames
          scope''' = scope'' {scopeConstructors = constructors `Map.union` scopeConstructors scope''}
          scope'''' = scope''' {scopeRecordFields = recordFields `Map.union` scopeRecordFields scope'''}
       in scope'''' {scopeMethods = methods `Map.union` scopeMethods scope''''}

data DeclExports = DeclExports [UnqualifiedName] [UnqualifiedName] (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text])

declExportedNames :: Decl -> DeclExports
declExportedNames decl =
  case decl of
    DeclAnn _ inner -> declExportedNames inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name _ -> DeclExports [name] [] Map.empty Map.empty Map.empty
        PatternBind _ pat _ ->
          DeclExports (map snd (collectPatVarBinders NoSourceSpan pat)) [] Map.empty Map.empty Map.empty
    DeclTypeSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty
    DeclClass classDecl ->
      let className = binderHeadName (classDeclHead classDecl)
          methodNames = classDeclMethodNames (classDeclItems classDecl)
       in DeclExports
            methodNames
            [className]
            Map.empty
            Map.empty
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName methodNames))
    DeclTypeData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      let typeName = binderHeadName (newtypeDeclHead newtypeDecl)
          termNames = maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl)
          constructorNames = maybe [] dataConDeclConstructorNames (newtypeDeclConstructor newtypeDecl)
       in DeclExports termNames [typeName] (constructorMap typeName constructorNames) (maybe Map.empty (recordFieldMap . (: [])) (newtypeDeclConstructor newtypeDecl)) Map.empty
    DeclTypeSyn typeSynDecl -> DeclExports [] [binderHeadName (typeSynHead typeSynDecl)] Map.empty Map.empty Map.empty
    _ -> DeclExports [] [] Map.empty Map.empty Map.empty

dataDeclExports :: BinderHead UnqualifiedName -> [DataConDecl] -> DeclExports
dataDeclExports headBinder constructors =
  let typeName = binderHeadName headBinder
   in DeclExports
        (dataDeclConstructorNames constructors)
        [typeName]
        (constructorMap typeName (concatMap dataConDeclConstructorNames constructors))
        (recordFieldMap constructors)
        Map.empty

constructorMap :: UnqualifiedName -> [UnqualifiedName] -> Map.Map Text [Text]
constructorMap typeName constructors =
  Map.singleton (renderUnqualifiedName typeName) (map renderUnqualifiedName constructors)

recordFieldMap :: [DataConDecl] -> Map.Map Text [Text]
recordFieldMap constructors =
  Map.fromList
    [ (renderUnqualifiedName conName, concatMap (map renderUnqualifiedName . fieldNames) fields)
    | (conName, fields) <- concatMap dataConDeclRecordFields constructors
    ]

classDeclMethodNames :: [ClassDeclItem] -> [UnqualifiedName]
classDeclMethodNames = concatMap go
  where
    go (ClassItemAnn _ inner) = go inner
    go (ClassItemTypeSig names _) = names
    go (ClassItemDefaultSig name _) = [name]
    go _ = []

dataDeclConstructorNames :: [DataConDecl] -> [UnqualifiedName]
dataDeclConstructorNames = concatMap dataConDeclNames

dataConDeclNames :: DataConDecl -> [UnqualifiedName]
dataConDeclNames dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> [name]
          InfixCon _ _ _ name _ -> [name]
          RecordCon _ _ name fields -> name : concatMap fieldNames fields
          GadtCon _ _ names (GadtRecordBody fields _) -> names <> concatMap fieldNames fields
          GadtCon _ _ names _ -> names
          TupleCon _ _ flavor fields -> [tupleConName flavor (length fields)]
          UnboxedSumCon _ _ pos arity _ -> [unboxedSumConName pos arity]
          ListCon {} -> [listConName]
   in go dataConDecl

dataConDeclConstructorNames :: DataConDecl -> [UnqualifiedName]
dataConDeclConstructorNames dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> [name]
          InfixCon _ _ _ name _ -> [name]
          RecordCon _ _ name _ -> [name]
          GadtCon _ _ names _ -> names
          TupleCon _ _ flavor fields -> [tupleConName flavor (length fields)]
          UnboxedSumCon _ _ pos arity _ -> [unboxedSumConName pos arity]
          ListCon {} -> [listConName]
   in go dataConDecl

dataConDeclRecordFields :: DataConDecl -> [(UnqualifiedName, [FieldDecl])]
dataConDeclRecordFields dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          RecordCon _ _ name fields -> [(name, fields)]
          GadtCon _ _ names (GadtRecordBody fields _) -> [(name, fields) | name <- names]
          _ -> []
   in go dataConDecl

tupleConName :: TupleFlavor -> Int -> UnqualifiedName
tupleConName flavor arity =
  mkUnqualifiedName NameConSym $ case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

unboxedSumConName :: Int -> Int -> UnqualifiedName
unboxedSumConName pos arity =
  mkUnqualifiedName NameConSym ("(#" <> bars (pos - 1) <> "_" <> bars (arity - pos) <> "#)")

listConName :: UnqualifiedName
listConName = mkUnqualifiedName NameConSym "[]"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = T.replicate (n - 1) ","

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = T.replicate n "|"

moduleScope :: ModuleExports -> Module -> Scope
moduleScope exports modu =
  ownScope `unionScope` importedScope exports modu `unionScope` implicitPrelude `unionScope` builtinScope
  where
    ownScope = Map.findWithDefault emptyScope (moduleKey modu) exports
    preludeScope = Map.findWithDefault emptyScope "Prelude" exports
    -- Implicit Prelude: names available unqualified AND as Prelude.xxx
    implicitPrelude = preludeScope {scopeQualifiedModules = Map.singleton "Prelude" preludeScope}

importedScope :: ModuleExports -> Module -> Scope
importedScope exports modu =
  foldl' addImport emptyScope (moduleImports modu)
  where
    addImport acc importDecl
      | importDeclQualified importDecl || importDeclQualifiedPost importDecl =
          insertQualifiedModule qualifier imported acc
      | otherwise =
          let qualifiedAcc = insertQualifiedModule qualifier imported acc
           in unionScope qualifiedAcc imported
      where
        originModule = importDeclModule importDecl
        qualifier = fromMaybe originModule (importDeclAs importDecl)
        imported = filterImportSpec (importDeclSpec importDecl) (Map.findWithDefault emptyScope originModule exports)

filterImportSpec :: Maybe ImportSpec -> Scope -> Scope
filterImportSpec maybeSpec scope =
  case maybeSpec of
    Nothing -> scope
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      let allowedTypes = allowedTypeNames importSpecItems
          allowedTerms = allowedTermNames scope importSpecItems
       in Scope
            { scopeTerms =
                Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeTerms scope),
              scopeTypes = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeTypes scope),
              scopeConstructors = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeConstructors scope),
              scopeRecordFields = Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeRecordFields scope),
              scopeMethods = Map.filterWithKey (\n _ -> n `elem` allowedTypes) (scopeMethods scope),
              scopeQualifiedModules = scopeQualifiedModules scope
            }
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      filterScopeByNames (`notElem` (allowedTypeNames importSpecItems <> allowedTermNames scope importSpecItems)) scope

allowedTypeNames :: [ImportItem] -> [Text]
allowedTypeNames = mapMaybe (fmap renderUnqualifiedName . importItemTypeName)

allowedTermNames :: Scope -> [ImportItem] -> [Text]
allowedTermNames scope = concatMap (allowedTermNamesForItem scope)

allowedTermNamesForItem :: Scope -> ImportItem -> [Text]
allowedTermNamesForItem scope item =
  case item of
    ImportAnn _ sub -> allowedTermNamesForItem scope sub
    ImportItemVar _ itemName -> [renderUnqualifiedName itemName]
    ImportItemAll _ itemName -> allBundledMembers itemName
    ImportItemWith _ _ members -> map bundledMemberName members
    ImportItemAllWith _ itemName _ members -> map bundledMemberName members <> allBundledMembers itemName
    ImportItemAbs {} -> []
  where
    bundledMemberName = nameText . ieBundledMemberName
    allBundledMembers itemName =
      let parentName = renderUnqualifiedName itemName
          constructors = Map.lookup parentName (scopeConstructors scope)
          methods = Map.lookup parentName (scopeMethods scope)
       in fromMaybe [parentName] (constructors <> methods)

importItemTypeName :: ImportItem -> Maybe UnqualifiedName
importItemTypeName item =
  case item of
    ImportAnn _ sub -> importItemTypeName sub
    ImportItemVar {} -> Nothing
    ImportItemAbs _ itemName -> Just itemName
    ImportItemAll _ itemName -> Just itemName
    ImportItemWith _ itemName _ -> Just itemName
    ImportItemAllWith _ itemName _ _ -> Just itemName

resolveTermName :: Scope -> Name -> ResolvedName
resolveTermName scope name =
  case nameQualifier name of
    Just qualifier ->
      resolveQualifiedName scope lookupTerm qualifier name
    Nothing ->
      lookupTerm (nameText name) scope

resolveTypeName :: Scope -> Name -> ResolvedName
resolveTypeName scope name =
  case nameQualifier name of
    Just qualifier ->
      resolveQualifiedName scope lookupType qualifier name
    Nothing ->
      lookupType (nameText name) scope

resolveQualifiedName :: Scope -> (Text -> Scope -> ResolvedName) -> Text -> Name -> ResolvedName
resolveQualifiedName scope lookupName qualifier name =
  case Map.lookup qualifier (scopeQualifiedModules scope) of
    Nothing -> ResolvedError ("unknown qualified import: " <> T.unpack qualifier)
    Just qualifiedScope ->
      case lookupName (nameText name) qualifiedScope of
        ResolvedTopLevel resolved -> ResolvedTopLevel resolved
        other -> other

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Scope containing all wired-in Haskell built-ins that have no defining
-- source module but act like regular names during name resolution.
--
-- Term namespace: constructors for special syntax that cannot be expressed
-- as ordinary Haskell declarations (list cons @(:)@, the empty list @[]@,
-- tuple constructors, unboxed-tuple/sum constructors).  Normal Prelude
-- constructors like @True@, @False@, @Just@, @Nothing@, @Left@, @Right@ are
-- /not/ included here — they are defined in @base@ and reach a module via the
-- implicit Prelude import.
--
-- Type namespace: primitive and special types that are not defined in any
-- parsed source module and cannot be imported from @base@ in the ordinary
-- way (unboxed primitive types, @TYPE@, @RuntimeRep@, the function arrow,
-- @Constraint@, and the list type constructor @[]@).
--
-- This scope is merged into every module's scope unconditionally (lowest
-- priority — user-defined and imported names shadow it).
builtinScope :: Scope
builtinScope =
  Scope
    { scopeTerms = Map.fromList (map mkBuiltinTerm builtinTermNames),
      scopeTypes = Map.fromList (map mkBuiltinType builtinTypeNames),
      scopeConstructors = Map.empty,
      scopeRecordFields = Map.empty,
      scopeMethods = Map.empty,
      scopeQualifiedModules = Map.empty
    }
  where
    mkBuiltinTerm n = (n, ResolvedBuiltin n)
    mkBuiltinType n = (n, ResolvedBuiltin n)

-- | Wired-in term-namespace names: special syntax constructors that have no
-- defining source declaration.  Normal Prelude constructors (@True@, @False@,
-- @Just@, etc.) are intentionally excluded — they live in @base@ and arrive
-- via the implicit Prelude import.
--
-- Note: names here must match exactly what the parser emits as the 'Name'
-- text inside 'EVar'.  For example, the cons operator appears as @":"@ (not
-- @"(:)"@), because the surrounding parens are stripped by the parser.
builtinTermNames :: [T.Text]
builtinTermNames =
  [ -- Cons operator — the only list constructor that surfaces as EVar
    ":"
  ]

-- | Wired-in type-namespace names: primitive types that are not defined in
-- any parsed source module.  Prelude types like @Int@, @Bool@, @Maybe@, etc.
-- are defined in @base@ and are intentionally excluded.
--
-- Note: names here must match exactly what the parser emits as the 'Name'
-- text inside 'TCon'.  For example, the function arrow appears as @"->"@
-- (not @"(->)"@).
builtinTypeNames :: [T.Text]
builtinTypeNames =
  [ -- Function arrow (appears as TCon when used in kind signatures)
    "->",
    -- Constraint kind
    "Constraint",
    -- Unboxed primitive types (GHC.Prim / GHC.Types)
    "Int#",
    "Word#",
    "Char#",
    "Float#",
    "Double#",
    "Addr#",
    "ByteArray#",
    "MutableByteArray#",
    "RealWorld",
    "State#",
    "TYPE",
    "RuntimeRep",
    "LiftedRep",
    "UnliftedRep"
  ]

unionScope :: Scope -> Scope -> Scope
unionScope left right =
  Scope
    { scopeTerms = scopeTerms left `Map.union` scopeTerms right,
      scopeTypes = scopeTypes left `Map.union` scopeTypes right,
      scopeConstructors = scopeConstructors left `Map.union` scopeConstructors right,
      scopeRecordFields = scopeRecordFields left `Map.union` scopeRecordFields right,
      scopeMethods = scopeMethods left `Map.union` scopeMethods right,
      scopeQualifiedModules = scopeQualifiedModules left `Map.union` scopeQualifiedModules right
    }

insertTerm :: Text -> ResolvedName -> Scope -> Scope
insertTerm name resolved scope = scope {scopeTerms = Map.insert name resolved (scopeTerms scope)}

insertType :: Text -> ResolvedName -> Scope -> Scope
insertType name resolved scope = scope {scopeTypes = Map.insert name resolved (scopeTypes scope)}

insertQualifiedModule :: Text -> Scope -> Scope -> Scope
insertQualifiedModule qualifier imported scope =
  scope {scopeQualifiedModules = Map.insert qualifier imported (scopeQualifiedModules scope)}

lookupTerm :: Text -> Scope -> ResolvedName
lookupTerm name scope =
  Map.findWithDefault
    (ResolvedError "unbound")
    name
    (scopeTerms scope)

lookupType :: Text -> Scope -> ResolvedName
lookupType name scope =
  Map.findWithDefault
    (ResolvedError "unbound")
    name
    (scopeTypes scope)

filterScopeByNames :: (Text -> Bool) -> Scope -> Scope
filterScopeByNames keep scope =
  Scope
    { scopeTerms = Map.filterWithKey (\name _ -> keep name) (scopeTerms scope),
      scopeTypes = Map.filterWithKey (\name _ -> keep name) (scopeTypes scope),
      scopeConstructors = Map.filterWithKey (\name _ -> keep name) (scopeConstructors scope),
      scopeRecordFields = Map.filterWithKey (\name _ -> keep name) (scopeRecordFields scope),
      scopeMethods = Map.filterWithKey (\name _ -> keep name) (scopeMethods scope),
      scopeQualifiedModules = scopeQualifiedModules scope
    }

spanStartNameSpan :: SourceSpan -> Text -> SourceSpan
spanStartNameSpan span' name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let width = T.length name
       in SourceSpan
            sourceName
            startLine
            startCol
            startLine
            (startCol + width)
            startOffset
            (min endOffset (startOffset + width))
    NoSourceSpan -> NoSourceSpan

annotateDecl :: ResolutionAnnotation -> Decl -> Decl
annotateDecl annotation = DeclAnn (mkAnnotation annotation)

annotateExpr :: ResolutionAnnotation -> Expr -> Expr
annotateExpr annotation = EAnn (mkAnnotation annotation)

annotatePattern :: ResolutionAnnotation -> Pattern -> Pattern
annotatePattern annotation = PAnn (mkAnnotation annotation)

annotateType :: ResolutionAnnotation -> Type -> Type
annotateType annotation = TAnn (mkAnnotation annotation)

annotateImport :: ResolutionAnnotation -> ImportDecl -> ImportDecl
annotateImport annotation importDecl =
  importDecl {importDeclAnns = mkAnnotation annotation : importDeclAnns importDecl}

annotateImportErrors :: [ResolutionAnnotation] -> ImportDecl -> ImportDecl
annotateImportErrors annotations importDecl =
  importDecl {importDeclAnns = map mkAnnotation annotations <> importDeclAnns importDecl}

importModuleNameSpan :: ImportDecl -> SourceSpan
importModuleNameSpan importDecl =
  shiftSpanStartNameSpan (sourceSpanFromAnns (importDeclAnns importDecl)) prefixWidth (importDeclModule importDecl)
  where
    prefixWidth =
      T.length "import "
        + safeWidth
        + sourcePragmaWidth
        + preQualifiedWidth
        + levelWidth
        + packageWidth
    safeWidth
      | importDeclSafe importDecl = T.length "safe "
      | otherwise = 0
    sourcePragmaWidth =
      case importDeclSourcePragma importDecl of
        Just _ -> T.length "{-# SOURCE #-} "
        Nothing -> 0
    preQualifiedWidth
      | importDeclQualified importDecl && not (importDeclQualifiedPost importDecl) = T.length "qualified "
      | otherwise = 0
    levelWidth =
      case importDeclLevel importDecl of
        Just ImportLevelQuote -> T.length "quote "
        Just ImportLevelSplice -> T.length "splice "
        Nothing -> 0
    packageWidth =
      case importDeclPackage importDecl of
        Just packageName -> T.length packageName + T.length "\"\" "
        Nothing -> 0

shiftSpanStartNameSpan :: SourceSpan -> Int -> Text -> SourceSpan
shiftSpanStartNameSpan span' offset name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let shiftedStartOffset = startOffset + offset
          shifted =
            SourceSpan
              sourceName
              startLine
              (startCol + offset)
              startLine
              (startCol + offset)
              shiftedStartOffset
              endOffset
       in spanStartNameSpan shifted name
    NoSourceSpan -> NoSourceSpan

declKeywordNameSpan :: Text -> SourceSpan -> Text -> SourceSpan
declKeywordNameSpan keyword span' name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let keywordWidth = T.length keyword
          shifted =
            SourceSpan
              sourceName
              startLine
              (startCol + keywordWidth)
              startLine
              (startCol + keywordWidth)
              (startOffset + keywordWidth)
              endOffset
       in spanStartNameSpan shifted name
    NoSourceSpan -> NoSourceSpan
