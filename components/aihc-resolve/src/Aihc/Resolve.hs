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
  ( ArithSeq (..),
    ArrowKind (..),
    BangType (..),
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
    TyVarBinder (..),
    Type (..),
    UnqualifiedName,
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelGuardQualifierAnn,
    peelPatternAnn,
    recordFieldName,
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Monad
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Data.Data (Data, cast, gmapQ)
import Data.List (find, mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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

type BindingAnnotation = Scope -> Decl -> Maybe ResolutionAnnotation

resolveTopLevelDecls :: Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveTopLevelDecls _ [] = pure []
resolveTopLevelDecls signatureScopes (decl : rest) = do
  scope <- currentScope
  emitAnnotations (topLevelIntroducedNameAnnotations scope decl)
  (signatureScopes', decl') <- resolveBindingDecl (flip topLevelBinderAnnotation) signatureScopes decl
  decls' <- resolveTopLevelDecls signatureScopes' rest
  pure (decl' : decls')

resolveBindingGroup :: BindingAnnotation -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBindingGroup _ _ [] = pure []
resolveBindingGroup bindingAnnotation signatureScopes (decl : rest) = do
  (signatureScopes', decl') <- resolveBindingDecl bindingAnnotation signatureScopes decl
  decls' <- resolveBindingGroup bindingAnnotation signatureScopes' rest
  pure (decl' : decls')

resolveBindingDecl :: BindingAnnotation -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, Decl)
resolveBindingDecl bindingAnnotation signatureScopes decl = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  (signatureScopes', decl') <- withScope scoped (resolveDeclWithSignatureScope signatureScopes decl)
  let decl'' = maybe decl' (`annotateDecl` decl') (bindingAnnotation scope decl)
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

resolveBoundDecls :: Map.Map Text ResolutionAnnotation -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBoundDecls binderAnnotations =
  resolveBindingGroup (\_ decl -> declBinderAnnotation decl binderAnnotations)

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

topLevelIntroducedNameAnnotations :: Scope -> Decl -> [ResolutionAnnotation]
topLevelIntroducedNameAnnotations scope decl =
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
