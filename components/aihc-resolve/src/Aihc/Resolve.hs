{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Resolve
  ( pattern DeclResolution,
    pattern EResolution,
    pattern ImportResolution,
    pattern PResolution,
    pattern TResolution,
    resolve,
    resolveWithDeps,
    extractInterface,
    OperatorFixity (..),
    Scope (..),
    ModuleExports,
    collectModuleExports,
    ResolveError (..),
    ResolveResult (..),
    ResolutionNamespace (..),
    ResolvedName (..),
    ResolutionAnnotation (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ArrowKind (..),
    BangType (..),
    BinderHead (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    FieldDecl (..),
    FixityAssoc (..),
    ForallTelescope (..),
    ForeignDecl (..),
    GadtBody (..),
    GuardQualifier (..),
    GuardedRhs (..),
    IEBundledMember (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
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
    TyVarBinder (..),
    Type (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelGuardQualifierAnn,
    peelPatternAnn,
    recordFieldName,
    recordFieldValue,
    renderUnqualifiedName,
    unqualifiedNameAnns,
  )
import Aihc.Resolve.Monad
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Data.Data (Data, cast, gmapQ)
import Data.List (find, mapAccumL)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)

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
    <> importItemResolutionErrors (cast node)
    <> nameResolutionErrors (cast node)
    <> unqualifiedNameResolutionErrors (cast node)
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

importItemResolutionErrors :: Maybe ImportItem -> [ResolveError]
importItemResolutionErrors maybeItem =
  case maybeItem of
    Just (ImportAnn ann _) -> maybeToList (fromAnnotation ann >>= annotationResolveError)
    _ -> []

nameResolutionErrors :: Maybe Name -> [ResolveError]
nameResolutionErrors maybeName =
  case maybeName of
    Just name -> mapMaybe annotationResolveError (mapMaybe fromAnnotation (nameAnns name))
    _ -> []

unqualifiedNameResolutionErrors :: Maybe UnqualifiedName -> [ResolveError]
unqualifiedNameResolutionErrors maybeName =
  case maybeName of
    Just name -> mapMaybe annotationResolveError (mapMaybe fromAnnotation (unqualifiedNameAnns name))
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
      resolveErrors = collectResolveErrors modules'
    }
  where
    step currentNextLocal modu =
      let (nextLocal', modu') = resolveModule exports currentNextLocal modu
       in (nextLocal', modu')
    (_, resolved) = mapAccumL step 0 modules
    modules' = resolved
    ownExports = collectModuleExports modules
    exports = ownExports `Map.union` depExports

extractInterface :: ResolveResult -> ModuleExports
extractInterface = collectModuleExports . resolvedModules

resolveModule :: ModuleExports -> Int -> Module -> (Int, Module)
resolveModule exports nextLocal modu =
  let imports' = resolveModuleImports exports (moduleImports modu)
      modu' = modu {moduleImports = imports'}
      scope = moduleScope exports modu'
      (nextLocal', decls') = runResolveM scope nextLocal (resolveTopLevelDecls Map.empty (moduleDecls modu))
   in (nextLocal', modu' {moduleDecls = decls'})

resolveModuleImports :: ModuleExports -> [ImportDecl] -> [ImportDecl]
resolveModuleImports exports =
  map resolveModuleImport
  where
    resolveModuleImport importDecl
      | Just originScope <- Map.lookup (importDeclModule importDecl) exports =
          annotateMissingImportItems originScope importDecl
      | otherwise = annotateImport (missingModuleImportAnnotation importDecl) importDecl

missingModuleImportAnnotation :: ImportDecl -> ResolutionAnnotation
missingModuleImportAnnotation importDecl =
  let importedModule = importDeclModule importDecl
   in ResolutionAnnotation
        (importModuleNameSpan importDecl)
        importedModule
        ResolutionNamespaceModule
        (ResolvedError "not found")

annotateMissingImportItems :: Scope -> ImportDecl -> ImportDecl
annotateMissingImportItems originScope importDecl =
  case importDeclSpec importDecl of
    Just importSpec@ImportSpec {importSpecHiding = False, importSpecItems} ->
      importDecl {importDeclSpec = Just importSpec {importSpecItems = map annotateItem importSpecItems}}
    _ -> importDecl
  where
    annotateItem item =
      case missingImportItemAnnotation originScope item of
        Nothing -> item
        Just annotation -> annotateImportItemError annotation item

annotateImportItemError :: ResolutionAnnotation -> ImportItem -> ImportItem
annotateImportItemError annotation item =
  -- Keep the diagnostic span as the carrier span for annotated-source overlays.
  ImportAnn (mkAnnotation annotation) (ImportAnn (mkAnnotation (resolutionSpan annotation)) item)

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

type TermDefinition = UnqualifiedName -> Maybe ResolvedName

resolveTopLevelDecls :: Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveTopLevelDecls signatureScopes decls = do
  scope <- currentScope
  resolveBindingGroup (topLevelTermDefinition scope) signatureScopes decls

resolveBindingGroup :: TermDefinition -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBindingGroup _ _ [] = pure []
resolveBindingGroup termDefinition signatureScopes (decl : rest) = do
  (signatureScopes', decl') <- resolveBindingDecl termDefinition signatureScopes decl
  decls' <- resolveBindingGroup termDefinition signatureScopes' rest
  pure (decl' : decls')

resolveBindingDecl :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveBindingDecl termDefinition signatureScopes decl = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  withScope scoped (resolveDeclWithSignatureScope termDefinition signatureScopes decl)

resolveDeclWithSignatureScope :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveDeclWithSignatureScope termDefinition signatureScopes decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann $ do
        (signatureScopes', inner') <- resolveDeclWithSignatureScope termDefinition signatureScopes inner
        pure (signatureScopes', DeclAnn ann inner')
    DeclTypeSig names ty -> do
      sp <- currentSpan
      (binderScope, ty') <- resolveTypeSignature ty
      let names' = map (resolveTermDefinitionAt sp termDefinition) names
      let signatureScopes' =
            List.foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
      pure (signatureScopes', DeclTypeSig names' ty')
    _ -> do
      decl' <- resolveDecl termDefinition decl
      let signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
      pure (signatureScopes', decl')

resolveDecl :: TermDefinition -> Decl -> ResolveM Decl
resolveDecl termDefinition (DeclAnn ann inner) =
  withPushedSpan ann (resolveDecl termDefinition inner)
resolveDecl termDefinition decl =
  resolveDeclCore termDefinition decl

resolveDeclCore :: TermDefinition -> Decl -> ResolveM Decl
resolveDeclCore termDefinition decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann (resolveDeclCore termDefinition inner)
    DeclValue valueDecl ->
      DeclValue <$> resolveValueDecl termDefinition valueDecl
    DeclTypeSig names ty -> do
      ty' <- resolveType ty
      pure (DeclTypeSig names ty')
    DeclStandaloneKindSig {} ->
      annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeData dataDecl ->
      DeclTypeData <$> resolveDataDecl "type data " dataDecl
    DeclData dataDecl ->
      DeclData <$> resolveDataDecl "data " dataDecl
    DeclTypeSyn typeSynDecl ->
      DeclTypeSyn <$> resolveTypeSynDecl typeSynDecl
    DeclSplice expr -> DeclSplice <$> resolveExpr expr
    DeclNewtype newtypeDecl ->
      DeclNewtype <$> resolveNewtypeDecl newtypeDecl
    DeclClass classDecl ->
      DeclClass <$> resolveClassDecl classDecl
    DeclDefault tys ->
      DeclDefault <$> mapM resolveType tys
    DeclFixity {} -> pure decl
    DeclForeign foreignDecl ->
      DeclForeign <$> resolveForeignDecl termDefinition foreignDecl
    DeclRoleAnnotation {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPragma {} -> pure decl
    DeclPatSyn {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclPatSynSig {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclInstance instanceDecl ->
      DeclInstance <$> resolveInstanceDecl instanceDecl
    DeclStandaloneDeriving {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeFamilyDecl {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclDataFamilyDecl {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclTypeFamilyInst {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl
    DeclDataFamilyInst {} -> annotateUnhandledDecl <$> currentSpan <*> pure decl

resolveValueDecl :: TermDefinition -> ValueDecl -> ResolveM ValueDecl
resolveValueDecl termDefinition valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      sp <- currentSpan
      let name' = resolveTermDefinitionAt sp termDefinition name
      FunctionBind name' <$> mapM resolveMatch matches
    PatternBind multTag pat rhs ->
      PatternBind multTag <$> resolvePatternDefinition termDefinition pat <*> resolveRhs rhs

resolveForeignDecl :: TermDefinition -> ForeignDecl -> ResolveM ForeignDecl
resolveForeignDecl termDefinition foreignDecl = do
  sp <- currentSpan
  let name' = resolveTermDefinitionAt sp termDefinition (foreignName foreignDecl)
  ty' <- resolveType (foreignType foreignDecl)
  pure foreignDecl {foreignName = name', foreignType = ty'}

resolveClassDecl :: ClassDecl -> ResolveM ClassDecl
resolveClassDecl classDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "class " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case classDeclHead classDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  context' <- traverse (mapM resolveType) (classDeclContext classDecl)
  items' <- mapM resolveClassDeclItem (classDeclItems classDecl)
  pure
    classDecl
      { classDeclHead = head',
        classDeclContext = context',
        classDeclItems = items'
      }

resolveClassDeclItem :: ClassDeclItem -> ResolveM ClassDeclItem
resolveClassDeclItem classDeclItem =
  case classDeclItem of
    ClassItemAnn ann inner -> ClassItemAnn ann <$> withPushedSpan ann (resolveClassDeclItem inner)
    ClassItemTypeSig names ty -> ClassItemTypeSig names <$> resolveType ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name <$> resolveType ty
    ClassItemDefault valueDecl -> do
      scope <- currentScope
      ClassItemDefault <$> withLocalSupply 0 (resolveValueDecl (topLevelTermDefinition scope) valueDecl)
    ClassItemFixity {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemPragma {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemTypeFamilyDecl {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemDataFamilyDecl {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem
    ClassItemDefaultTypeInst {} -> annotateUnhandledClassDeclItem <$> currentSpan <*> pure classDeclItem

resolveInstanceDecl :: InstanceDecl -> ResolveM InstanceDecl
resolveInstanceDecl instanceDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (instanceDeclForall instanceDecl)
  (context', head', items') <-
    extendScope forallScope $
      (,,)
        <$> mapM resolveType (instanceDeclContext instanceDecl)
        <*> resolveType (instanceDeclHead instanceDecl)
        <*> mapM resolveInstanceDeclItem (instanceDeclItems instanceDecl)
  pure
    instanceDecl
      { instanceDeclForall = forallBinders',
        instanceDeclContext = context',
        instanceDeclHead = head',
        instanceDeclItems = items'
      }

resolveInstanceDeclItem :: InstanceDeclItem -> ResolveM InstanceDeclItem
resolveInstanceDeclItem instanceDeclItem =
  case instanceDeclItem of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> withPushedSpan ann (resolveInstanceDeclItem inner)
    InstanceItemBind valueDecl -> do
      scope <- currentScope
      InstanceItemBind <$> withLocalSupply 0 (resolveValueDecl (topLevelTermDefinition scope) valueDecl)
    InstanceItemTypeSig names ty -> InstanceItemTypeSig names <$> resolveType ty
    InstanceItemFixity {} -> pure instanceDeclItem
    InstanceItemTypeFamilyInst {} -> annotateUnhandledInstanceDeclItem <$> currentSpan <*> pure instanceDeclItem
    InstanceItemDataFamilyInst {} -> annotateUnhandledInstanceDeclItem <$> currentSpan <*> pure instanceDeclItem
    InstanceItemPragma {} -> annotateUnhandledInstanceDeclItem <$> currentSpan <*> pure instanceDeclItem

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
      EAnn ann <$> withPushedSpan ann (resolveExpr inner)
    EVar name ->
      EVar <$> resolveTermUse name
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
    EInfix {} ->
      resolveInfixExpr expr
    ENegate inner ->
      ENegate <$> resolveExpr inner
    ESectionL inner op ->
      ESectionL <$> resolveExpr inner <*> resolveTermUseAtName op
    ESectionR op inner ->
      ESectionR <$> resolveTermUseAtName op <*> resolveExpr inner
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
    EListComp body stmts -> do
      (scope, stmts') <- resolveCompStmts stmts
      body' <- withScope scope (resolveExpr body)
      pure (EListComp body' stmts')
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

resolveCompStmts :: [CompStmt] -> ResolveM (Scope, [CompStmt])
resolveCompStmts stmts = do
  scope <- currentScope
  go scope stmts
  where
    go scope stmts' =
      withScope scope $
        case stmts' of
          [] -> pure (scope, [])
          stmt : rest -> do
            (scope', stmt') <- resolveCompStmt scope stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveCompStmt :: Scope -> CompStmt -> ResolveM (Scope, CompStmt)
resolveCompStmt scope stmt =
  case stmt of
    CompAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveCompStmt scope inner)
      pure (scope', CompAnn ann inner')
    CompGen pat src -> do
      src' <- resolveExpr src
      (patScope, pat') <- bindPattern pat
      pure (unionScope patScope scope, CompGen pat' src')
    CompGuard guard -> do
      guard' <- resolveExpr guard
      pure (scope, CompGuard guard')
    CompLetDecls decls -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, CompLetDecls decls')
    CompThen expr -> do
      expr' <- resolveExpr expr
      pure (scope, CompThen expr')
    CompThenBy f byExpr -> do
      f' <- resolveExpr f
      byExpr' <- resolveExpr byExpr
      pure (scope, CompThenBy f' byExpr')
    CompGroupUsing expr -> do
      expr' <- resolveExpr expr
      pure (scope, CompGroupUsing expr')
    CompGroupByUsing byExpr usingExpr -> do
      byExpr' <- resolveExpr byExpr
      usingExpr' <- resolveExpr usingExpr
      pure (scope, CompGroupByUsing byExpr' usingExpr')

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

resolveBoundDecls :: Map.Map Text ResolvedName -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBoundDecls binderTargets =
  resolveBindingGroup (\name -> Map.lookup (renderUnqualifiedName name) binderTargets)

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
          name' = resolveUnqualifiedNameTo sp ResolutionNamespaceTerm resolvedName name
      pure (termScope key resolvedName, PVar name')
    PTypeBinder binder -> do
      let binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
      resolvedName <- freshLocal binderName
      binder' <- traverseTyVarBinderKind binder
      let binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty
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
      name' <- resolveTermUseAtName name
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, PCon name' typeArgs' pats')
    PInfix left name right -> do
      name' <- resolveTermUseAtName name
      (leftScope, left') <- bindPattern left
      (rightScope, right') <- bindPattern right
      pure (unionScope rightScope leftScope, PInfix left' name' right')
    PView expr inner -> do
      expr' <- resolveExpr expr
      (scope, inner') <- bindPattern inner
      pure (scope, PView expr' inner')
    PAs alias inner -> do
      here <- currentSpan
      let aliasKey = renderUnqualifiedName alias
      aliasResolved <- freshLocal alias
      let alias' = resolveUnqualifiedNameTo (spanStartNameSpan here aliasKey) ResolutionNamespaceTerm aliasResolved alias
          aliasScope = termScope aliasKey aliasResolved
      (innerScope, inner') <- bindPattern inner
      pure (unionScope innerScope aliasScope, PAs alias' inner')
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
      name' <- resolveTermUseAtName name
      (fieldScopes, fields') <-
        mapAndUnzipM
          ( \field -> do
              (fieldScope, fieldPat') <- bindPattern (recordFieldValue field)
              pure (fieldScope, field {recordFieldValue = fieldPat'})
          )
          fields
      wildcardEntries <- bindRecordWildcardFields name fields wildcard
      let wildcardScope = Scope (Map.fromList wildcardEntries) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      pure (foldr unionScope wildcardScope fieldScopes, PRecord name' fields' wildcard)
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
  Scope (Map.singleton key resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

resolvePatternDefinition :: TermDefinition -> Pattern -> ResolveM Pattern
resolvePatternDefinition termDefinition pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann (resolvePatternDefinition termDefinition inner)
    PVar name -> do
      sp <- currentSpan
      pure (PVar (resolveTermDefinitionAt sp termDefinition name))
    PTypeBinder binder -> do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure (PTypeBinder (binder {tyVarBinderKind = kind'}))
    PTypeSyntax form ty ->
      PTypeSyntax form <$> resolveType ty
    PWildcard -> pure pat
    PLit {} -> pure pat
    PQuasiQuote {} -> annotateUnhandledPattern <$> currentSpan <*> pure pat
    PTuple flavor pats ->
      PTuple flavor <$> mapM (resolvePatternDefinition termDefinition) pats
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity <$> resolvePatternDefinition termDefinition inner
    PList pats ->
      PList <$> mapM (resolvePatternDefinition termDefinition) pats
    PCon name typeArgs pats ->
      PCon <$> resolveTermUseAtName name <*> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PInfix left name right ->
      PInfix <$> resolvePatternDefinition termDefinition left <*> resolveTermUseAtName name <*> resolvePatternDefinition termDefinition right
    PView expr inner ->
      PView <$> withLocalSupply 0 (resolveExpr expr) <*> resolvePatternDefinition termDefinition inner
    PAs alias inner -> do
      sp <- currentSpan
      PAs (resolveTermDefinitionAt sp termDefinition alias) <$> resolvePatternDefinition termDefinition inner
    PStrict inner ->
      PStrict <$> resolvePatternDefinition termDefinition inner
    PIrrefutable inner ->
      PIrrefutable <$> resolvePatternDefinition termDefinition inner
    PNegLit {} -> pure pat
    PParen inner ->
      PParen <$> resolvePatternDefinition termDefinition inner
    PRecord name fields wildcard ->
      PRecord
        <$> resolveTermUseAtName name
        <*> mapM
          ( \field -> do
              value' <- resolvePatternDefinition termDefinition (recordFieldValue field)
              pure field {recordFieldValue = value'}
          )
          fields
        <*> pure wildcard
    PTypeSig inner ty ->
      PTypeSig <$> resolvePatternDefinition termDefinition inner <*> resolveType ty
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

resolveDataDecl :: Text -> DataDecl -> ResolveM DataDecl
resolveDataDecl keyword dataDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan keyword declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case dataDeclHead dataDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  context' <- mapM resolveType (dataDeclContext dataDecl)
  kind' <- traverse resolveType (dataDeclKind dataDecl)
  constructors' <- mapM resolveDataConDecl (dataDeclConstructors dataDecl)
  pure
    dataDecl
      { dataDeclHead = head',
        dataDeclContext = context',
        dataDeclKind = kind',
        dataDeclConstructors = map (resolveDataConDefinitions scope) constructors'
      }

resolveTypeSynDecl :: TypeSynDecl -> ResolveM TypeSynDecl
resolveTypeSynDecl typeSynDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "type " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case typeSynHead typeSynDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  body' <- resolveType (typeSynBody typeSynDecl)
  pure typeSynDecl {typeSynHead = head', typeSynBody = body'}

resolveNewtypeDecl :: NewtypeDecl -> ResolveM NewtypeDecl
resolveNewtypeDecl newtypeDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "newtype " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
      head' =
        case newtypeDeclHead newtypeDecl of
          PrefixBinderHead name params -> PrefixBinderHead (resolveHeadName name) params
          InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (resolveHeadName name) rhs params
  kind' <- traverse resolveType (newtypeDeclKind newtypeDecl)
  constructor' <- traverse resolveDataConDecl (newtypeDeclConstructor newtypeDecl)
  pure
    newtypeDecl
      { newtypeDeclHead = head',
        newtypeDeclKind = kind',
        newtypeDeclConstructor = resolveDataConDefinitions scope <$> constructor'
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
    TVar name ->
      TVar <$> resolveScopedTypeVariableUse name
    TCon name promoted ->
      TCon <$> resolveTypeUse name <*> pure promoted
    TBuiltinCon {} -> pure ty
    TImplicitParam name inner ->
      TImplicitParam name <$> resolveType inner
    TTypeLit {} -> pure ty
    TStar {} -> pure ty
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

allocateLocalDeclBinders :: [Decl] -> ResolveM (Map.Map Text ResolvedName, Scope)
allocateLocalDeclBinders =
  foldM step (Map.empty, emptyScope)
  where
    step acc decl = foldM addBinder acc (declBinderCandidates decl)
    addBinder (targets, scope) (_, name) = do
      resolvedName <- freshLocal name
      let key = renderUnqualifiedName name
      pure (Map.insert key resolvedName targets, insertTerm key resolvedName scope)

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

topLevelTermDefinition :: Scope -> TermDefinition
topLevelTermDefinition scope name =
  Just (lookupTerm (renderUnqualifiedName name) scope)

resolveTermDefinitionAt :: SourceSpan -> TermDefinition -> UnqualifiedName -> UnqualifiedName
resolveTermDefinitionAt span' termDefinition name =
  case termDefinition name of
    Just resolved ->
      resolveUnqualifiedNameTo (spanStartNameSpan span' (renderUnqualifiedName name)) ResolutionNamespaceTerm resolved name
    Nothing -> name

resolveUnqualifiedNameTo :: SourceSpan -> ResolutionNamespace -> ResolvedName -> UnqualifiedName -> UnqualifiedName
resolveUnqualifiedNameTo span' namespace resolved name =
  name
    { unqualifiedNameAnns =
        mkAnnotation (ResolutionAnnotation span' (renderUnqualifiedName name) namespace resolved)
          : unqualifiedNameAnns name
    }

resolveNameTo :: SourceSpan -> ResolutionNamespace -> ResolvedName -> Name -> Name
resolveNameTo span' namespace resolved name =
  name
    { nameAnns =
        mkAnnotation (ResolutionAnnotation span' (nameText name) namespace resolved)
          : nameAnns name
    }

resolveTermUse :: Name -> ResolveM Name
resolveTermUse name = do
  sp <- currentSpan
  scope <- currentScope
  pure (resolveNameTo sp ResolutionNamespaceTerm (resolveTermName scope name) name)

resolveTermUseAtName :: Name -> ResolveM Name
resolveTermUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  pure (resolveNameTo (spanStartNameSpan sp (nameText name)) ResolutionNamespaceTerm (resolveTermName scope name) name)

data ResolvedInfixOp = ResolvedInfixOp
  { resolvedInfixIndex :: !Int,
    resolvedInfixName :: !Name,
    resolvedInfixFixity :: !OperatorFixity
  }

resolveInfixExpr :: Expr -> ResolveM Expr
resolveInfixExpr expr = do
  let (operands, names) = flattenInfixExpr expr
  operands' <- mapM resolveExpr operands
  names' <- mapM resolveTermUseAtName names
  let fallbackExpr = buildLeftInfixExpr expr operands' names'
  reassociateResolvedInfixExpr operands' names' fallbackExpr

reassociateResolvedInfixExpr :: [Expr] -> [Name] -> Expr -> ResolveM Expr
reassociateResolvedInfixExpr operands names fallbackExpr = do
  scope <- currentScope
  sp <- currentSpan
  let ops =
        [ ResolvedInfixOp index name (resolveFixityName scope name)
        | (index, name) <- zip [0 :: Int ..] names
        ]
  case ambiguousInfixOp ops of
    Just op ->
      pure (buildLeftInfixExpr fallbackExpr operands (replaceAt (resolvedInfixIndex op) (ambiguousFixityName sp op) names))
    Nothing ->
      pure (rebuildInfixExpr fallbackExpr operands ops)

ambiguousFixityName :: SourceSpan -> ResolvedInfixOp -> Name
ambiguousFixityName ambient op =
  name
    { nameAnns =
        mkAnnotation
          ( ResolutionAnnotation
              (effectiveResolutionSpan (spanStartNameSpan ambient (nameText name)) (sourceSpanFromAnns (nameAnns name)))
              (nameText name)
              ResolutionNamespaceTerm
              (ResolvedError "ambiguous fixity")
          )
          : filter (not . isResolutionAnnotation) (nameAnns name)
    }
  where
    name = resolvedInfixName op

isResolutionAnnotation :: Annotation -> Bool
isResolutionAnnotation ann =
  not (null (maybeToList (fromAnnotation ann :: Maybe ResolutionAnnotation)))

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index replacement =
  zipWith (\i value -> if i == index then replacement else value) [0 :: Int ..]

buildLeftInfixExpr :: Expr -> [Expr] -> [Name] -> Expr
buildLeftInfixExpr fallbackExpr [] _ = fallbackExpr
buildLeftInfixExpr _ (operand : operands) ops =
  List.foldl' (\left (op, right) -> EInfix left op right) operand (zip ops operands)

flattenInfixExpr :: Expr -> ([Expr], [Name])
flattenInfixExpr expr =
  case expr of
    EInfix left op right ->
      let (operands, ops) = flattenInfixExpr left
       in (operands <> [right], ops <> [op])
    _ -> ([expr], [])

ambiguousInfixOp :: [ResolvedInfixOp] -> Maybe ResolvedInfixOp
ambiguousInfixOp ops =
  listToMaybe
    [ right
    | (leftIndex, left) <- indexed,
      let leftPrec = infixPrecedence left,
      (rightIndex, right) <- drop (leftIndex + 1) indexed,
      infixPrecedence right == leftPrec,
      all ((> leftPrec) . infixPrecedence) [between | (index, between) <- indexed, index > leftIndex, index < rightIndex],
      incompatibleSamePrecedence left right
    ]
  where
    indexed = zip [0 :: Int ..] ops

incompatibleSamePrecedence :: ResolvedInfixOp -> ResolvedInfixOp -> Bool
incompatibleSamePrecedence left right =
  infixAssoc left /= infixAssoc right || infixAssoc left == Infix || infixAssoc right == Infix

infixAssoc :: ResolvedInfixOp -> FixityAssoc
infixAssoc = operatorFixityAssoc . resolvedInfixFixity

infixPrecedence :: ResolvedInfixOp -> Int
infixPrecedence = operatorFixityPrecedence . resolvedInfixFixity

rebuildInfixExpr :: Expr -> [Expr] -> [ResolvedInfixOp] -> Expr
rebuildInfixExpr fallbackExpr [] _ = fallbackExpr
rebuildInfixExpr _ (operand : operands) ops =
  let (expr, _, _) = parseInfixExpr 0 operand operands ops
   in expr

parseInfixExpr :: Int -> Expr -> [Expr] -> [ResolvedInfixOp] -> (Expr, [Expr], [ResolvedInfixOp])
parseInfixExpr minPrec lhs operands ops =
  case ops of
    op : restOps
      | infixPrecedence op >= minPrec,
        rhsOperand : restOperands <- operands ->
          let nextMinPrec =
                case infixAssoc op of
                  InfixR -> infixPrecedence op
                  Infix -> infixPrecedence op + 1
                  InfixL -> infixPrecedence op + 1
              (rhs, operands', ops') = parseInfixExpr nextMinPrec rhsOperand restOperands restOps
           in parseInfixExpr minPrec (EInfix lhs (resolvedInfixName op) rhs) operands' ops'
    _ -> (lhs, operands, ops)

resolveTypeUse :: Name -> ResolveM Name
resolveTypeUse name = do
  sp <- currentSpan
  scope <- currentScope
  pure (resolveNameTo sp ResolutionNamespaceType (resolveTypeName scope name) name)

resolveScopedTypeVariableUse :: UnqualifiedName -> ResolveM UnqualifiedName
resolveScopedTypeVariableUse name = do
  sp <- currentSpan
  scope <- currentScope
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
  pure $
    case resolved of
      ResolvedError _ -> name
      _ -> resolveUnqualifiedNameTo sp ResolutionNamespaceType resolved name

resolveDataConDefinitions :: Scope -> DataConDecl -> DataConDecl
resolveDataConDefinitions scope =
  go NoSourceSpan
  where
    go ambient current =
      case current of
        DataConAnn ann inner -> DataConAnn ann (go (pushSpanFromAnn ambient ann) inner)
        PrefixCon forallVars context name bangTypes ->
          PrefixCon forallVars context (resolveConstructor ambient name) bangTypes
        RecordCon forallVars context name fields ->
          RecordCon forallVars context (resolveConstructor ambient name) fields
        InfixCon forallVars context lhs name rhs ->
          InfixCon forallVars context lhs (resolveConstructor ambient name) rhs
        GadtCon forallVars context names body ->
          GadtCon forallVars context (map (resolveConstructor ambient) names) body
        TupleCon {} -> current
        UnboxedSumCon {} -> current
        ListCon {} -> current

    resolveConstructor span' name =
      let rendered = renderUnqualifiedName name
       in resolveUnqualifiedNameTo
            (spanStartNameSpan span' rendered)
            ResolutionNamespaceTerm
            (lookupTerm rendered scope)
            name
