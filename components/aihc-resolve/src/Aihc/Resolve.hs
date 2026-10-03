{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Resolve
  ( -- * Resolving a unit
    resolveUnit,
    ResolvedUnit (..),
    ResolvedModule (..),
    ResolveFailure (..),
    ResolveError (..),
    ModuleUnit (..),
    modulesInPackage,
    Package (..),
    PackageId (..),
    unnamedPackage,

    -- * Identities
    Entity (..),
    GlobalName (..),
    LocalId (..),
    globalTerm,
    ResolutionNamespace (..),

    -- * Annotations
    ResolutionAnnotation (..),
    Identifier (..),
    displayIdentifier,
    annotationResolution,
    resolutionOf,
    nameResolution,
    termResolution,
    typeResolution,
    binderResolution,
    binderEntity,
    nameEntity,
    nameOrigin,
    pattern DeclResolution,
    pattern EResolution,
    pattern PResolution,
    pattern TResolution,

    -- * Exports between units
    ModuleExports,
    ModuleKey (..),
    Exports,
    ExportEntry (..),
    exportsEntries,
    exportsFromEntries,
    exportedTerms,
    exportedTypes,
    OperatorFixity (..),
    moduleExportsFromList,
    moduleExportKeys,
    lookupModuleExport,
    filterModuleExports,
    collectModuleExports,
    collectModuleExportsWithDeps,
    exportedLocalNames,

    -- * Builtins
    Builtins,
    builtins,
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
    DataFamilyDecl (..),
    DataFamilyInst (..),
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    DoStmt (..),
    Expr (..),
    Extension (..),
    FieldDecl (..),
    FloatType (..),
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
    Literal (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    NumericType (..),
    PatSynArgs (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    Pragma (..),
    PragmaType (..),
    RecordField (..),
    Rhs (..),
    RoleAnnotation (..),
    RuleBinder (..),
    RuleDecl (..),
    SourceSpan,
    StandaloneDerivingDecl (..),
    TyVarBinder (..),
    Type (..),
    TypeFamilyDecl (..),
    TypeFamilyEq (..),
    TypeFamilyInst (..),
    TypeFamilyResultSig (..),
    TypePromotion (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelGuardQualifierAnn,
    peelLiteralAnn,
    peelPatternAnn,
    recordFieldName,
    recordFieldValue,
    renderUnqualifiedName,
    unqualifiedNameAnns,
    unqualifiedNameText,
  )
import Aihc.Resolve.Infix
import Aihc.Resolve.Monad
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Data.List (find, mapAccumL)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T

-- | Resolve one compilation unit against the scopes its modules can see:
-- what the unit's own modules export, on top of what its dependencies
-- export.
--
-- The caller builds that map, with 'collectModuleExportsWithDeps' over the
-- unit and @<>@ with the dependencies, and keeps it: the builtin scope is
-- read out of the same map, and the units above this one import the half
-- that the unit itself exports. Building the export scopes is most of what
-- resolving a unit costs, so it happens once.
--
-- The builtins supply the terms that desugared syntax applies, such as
-- @fromInteger@, and the types of primitive literals. They also supply the
-- list constructor @:@ and the equality type @~@, which every module sees
-- without an import. The resolver does not look up these names in a module
-- of its own choice.
--
-- A unit resolves as a whole: one name that does not resolve makes the
-- unit a 'ResolveFailure', so a 'ResolvedUnit' has no unresolved name.
resolveUnit :: Builtins -> ModuleExports -> [ModuleUnit] -> Either ResolveFailure ResolvedUnit
resolveUnit (Builtins builtinScope) exports packageModules
  | null errors = Right (ResolvedUnit modules)
  | otherwise = Left (ResolveFailure errors modules)
  where
    step currentNextLocal unit =
      resolveModule builtinScope (moduleUnitPackage unit) exports (moduleUnitExtensions unit) currentNextLocal (moduleUnitAst unit)
    (_, resolved) = mapAccumL step 0 packageModules
    errors = concatMap (\(moduleErrors, _, _) -> moduleErrors) resolved
    modules =
      zipWith
        (\unit (_, visibleTerms, modu) -> ResolvedModule unit {moduleUnitAst = modu} visibleTerms)
        packageModules
        resolved

resolveModule :: Scope -> Package -> ModuleExports -> [Extension] -> Int -> Module -> (Int, ([ResolveError], [GlobalName], Module))
resolveModule builtinScope package exports extensions nextLocal modu =
  let (imports', importErrors) = resolveModuleImports package exports (moduleImports modu)
      modu' = modu {moduleImports = imports'}
      scope = moduleScope builtinScope package exports extensions modu'
      (nextLocal', declErrors, decls') =
        runResolveM
          scope
          (moduleInfo builtinScope extensions modu')
          nextLocal
          (resolveBindingGroup (topLevelTermDefinition scope) Map.empty (moduleDecls modu))
      visibleTerms =
        [ global
        | visibleScope <- scope : Map.elems (scopeQualifiedModules scope),
          EntityGlobal global <- Map.elems (scopeTerms visibleScope)
        ]
   in ( nextLocal',
        ( importErrors <> declErrors,
          visibleTerms,
          modu' {moduleDecls = decls'}
        )
      )

-- | What the resolver needs to know about the module it is resolving. The
-- extensions come from the driver, which folded the language edition, the
-- package's default extensions and the module's pragmas into them.
moduleInfo :: Scope -> [Extension] -> Module -> ModuleInfo
moduleInfo builtinScope extensions modu =
  ModuleInfo
    { moduleInfoExtensions = extensions,
      moduleInfoExplicitPreludeImport =
        any ((== "Prelude") . importDeclModule) (moduleImports modu),
      moduleInfoBuiltinScope = builtinScope
    }

-- | Resolve the imports of a module, and give what they failed to name.
--
-- Import resolution runs before the module's own scope exists, so it is
-- outside 'ResolveM' and hands its errors back rather than recording them.
resolveModuleImports :: Package -> ModuleExports -> [ImportDecl] -> ([ImportDecl], [ResolveError])
resolveModuleImports package exports imports =
  (map fst resolved, concatMap snd resolved)
  where
    resolved = map resolveModuleImport imports

    resolveModuleImport importDecl
      | [originScope] <- matches =
          annotateMissingImportItems originScope importDecl
      | null matches = missingModule "not found" importDecl
      | otherwise = missingModule "ambiguous" importDecl
      where
        matches = matchingModuleScopes package (importDeclPackage importDecl) (importDeclModule importDecl) exports

    missingModule message importDecl =
      let resolveError = missingModuleImport message importDecl
       in (annotateImport resolveError importDecl, [resolveError])

missingModuleImport :: String -> ImportDecl -> ResolveError
missingModuleImport message importDecl =
  resolveErrorAt
    (importModuleNameSpan importDecl)
    (IdentifierNamed (importDeclModule importDecl))
    ResolutionNamespaceModule
    message

annotateMissingImportItems :: Scope -> ImportDecl -> (ImportDecl, [ResolveError])
annotateMissingImportItems originScope importDecl =
  case importDeclSpec importDecl of
    Just importSpec@ImportSpec {importSpecHiding = False, importSpecItems} ->
      let annotated = map annotateItem importSpecItems
       in ( importDecl {importDeclSpec = Just importSpec {importSpecItems = map fst annotated}},
            mapMaybe snd annotated
          )
    _ -> (importDecl, [])
  where
    annotateItem item =
      case missingImportItemAnnotation originScope item of
        Nothing -> (item, Nothing)
        Just annotation -> (annotateImportItemError annotation item, Just annotation)

annotateImportItemError :: ResolveError -> ImportItem -> ImportItem
annotateImportItemError resolveError item =
  -- Keep the diagnostic span as the carrier span for annotated-source overlays.
  ImportAnn (mkAnnotation resolveError) (maybe id (ImportAnn . mkAnnotation) (resolveErrorSpan resolveError) item)

missingImportItemAnnotation :: Scope -> ImportItem -> Maybe ResolveError
missingImportItemAnnotation originScope item =
  go item
  where
    go current =
      case current of
        ImportAnn _ sub -> go sub
        ImportItemVar _ itemName ->
          missingImportedName item ResolutionNamespaceTerm itemName (scopeTerms originScope)
        ImportItemAbs (Just namespace) itemName
          | isTermNamespace namespace ->
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

missingImportMemberAnnotation :: Scope -> ImportItem -> [IEBundledMember] -> Maybe ResolveError
missingImportMemberAnnotation originScope item members =
  missingMemberAnnotation <$> find missingMember members
  where
    missingMember member = nameText (ieBundledMemberName member) `notElem` exportedMembers
    exportedMembers =
      case importItemTypeName item of
        Nothing -> []
        Just itemName -> allTypeMembers (renderUnqualifiedName itemName) originScope
    missingMemberAnnotation member =
      let memberName = nameText (ieBundledMemberName member)
       in resolveErrorAt
            (importMemberNameSpan (peelImportItemSpan item) memberName)
            (IdentifierNamed memberName)
            ResolutionNamespaceTerm
            "not exported"

missingImportedName :: ImportItem -> ResolutionNamespace -> UnqualifiedName -> Map.Map Text Entity -> Maybe ResolveError
missingImportedName item namespace itemName candidates
  | Map.member rendered candidates = Nothing
  | otherwise =
      Just
        ( resolveErrorAt
            (spanStartNameSpan (peelImportItemSpan item) rendered)
            (IdentifierNamed rendered)
            namespace
            "not exported"
        )
  where
    rendered = renderUnqualifiedName itemName

type TermDefinition = UnqualifiedName -> Maybe Resolution

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
      names' <- mapM (resolveTermDefinitionAt sp termDefinition) names
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

-- | The annotation for syntax the resolver has no case for. The name is
-- the constructor of the form it met.
unhandledSyntax :: ResolutionNamespace -> Text -> ResolveM Annotation
unhandledSyntax namespace constructor = do
  sp <- currentSpan
  resolution sp (IdentifierNamed constructor) namespace (Unresolved "unhandled syntax")

resolveDeclCore :: TermDefinition -> Decl -> ResolveM Decl
resolveDeclCore termDefinition decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann (resolveDeclCore termDefinition inner)
    DeclValue valueDecl ->
      DeclValue <$> resolveValueDecl termDefinition valueDecl
    DeclImplicitParam name expr mDecls -> do
      -- An implicit-parameter binding does not bind a term name.
      -- The type checker connects each use to its binding.
      (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
      expr' <- extendScope localScope (resolveExpr expr)
      mDecls' <- traverse (extendScope localScope . resolveBoundDecls binderAnnotations Map.empty) mDecls
      pure (DeclImplicitParam name expr' mDecls')
    DeclTypeSig names ty -> do
      ty' <- resolveType ty
      pure (DeclTypeSig names ty')
    DeclStandaloneKindSig name kind -> do
      scope <- currentScope
      sp <- currentSpan
      let rendered = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      DeclStandaloneKindSig name' <$> resolveType kind
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
    DeclRoleAnnotation roleAnnotation -> do
      scope <- currentScope
      sp <- currentSpan
      let name = roleAnnotationName roleAnnotation
          rendered = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      pure (DeclRoleAnnotation roleAnnotation {roleAnnotationName = name'})
    DeclPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure decl
      | otherwise -> DeclAnn <$> unhandledSyntax ResolutionNamespaceTerm "DeclPragma" <*> pure decl
    DeclRules rules ->
      DeclRules <$> mapM resolveRuleDecl rules
    DeclPatSyn patSyn -> do
      sp <- currentSpan
      (patSyn', unboundArgs) <- resolvePatSynDecl termDefinition patSyn
      unboundAnns <- mapM (unboundPatSynArgAnnotation sp) unboundArgs
      pure (List.foldl' (flip DeclAnn) (DeclPatSyn patSyn') unboundAnns)
    DeclPatSynSig names ty -> do
      sp <- currentSpan
      (_, ty') <- resolveTypeSignature ty
      names' <- mapM (resolveTermDefinitionAt (declKeywordNameSpan "pattern " sp "") termDefinition) names
      pure (DeclPatSynSig names' ty')
    DeclInstance instanceDecl ->
      DeclInstance <$> resolveInstanceDecl instanceDecl
    DeclStandaloneDeriving derivingDecl ->
      DeclStandaloneDeriving <$> resolveStandaloneDerivingDecl derivingDecl
    DeclTypeFamilyDecl familyDecl ->
      DeclTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    DeclDataFamilyDecl dataFamilyDecl ->
      DeclDataFamilyDecl <$> resolveDataFamilyDecl "data family " dataFamilyDecl
    DeclTypeFamilyInst familyInst ->
      DeclTypeFamilyInst <$> resolveTypeFamilyInst familyInst
    DeclDataFamilyInst dataFamilyInst ->
      DeclDataFamilyInst <$> resolveDataFamilyInst dataFamilyInst

-- | Resolve one rewrite rule of a @RULES@ pragma.
--
-- The type variables of a leading @forall@ scope over the types of the
-- pattern variables and over both sides, and the pattern variables scope
-- over both sides: inside a @RULES@ pragma, ScopedTypeVariables is always
-- on. A type variable that no @forall@ binds is left for the type checker
-- to bind, as in a signature. The left-hand side must be a top-level
-- variable applied to arguments; anything else is an error on the rule.
resolveRuleDecl :: RuleDecl -> ResolveM RuleDecl
resolveRuleDecl rule =
  withEffectiveSpan (sourceSpanFromAnns (ruleAnns rule)) $
    withResetLocalSupply $ do
      (typeScope, typeBinders') <- bindTyVarBinders (ruleTypeBinders rule)
      extendScope typeScope $ do
        (binderScope, binders') <- bindRuleBinders (ruleBinders rule)
        extendScope binderScope $ do
          lhs' <- resolveExpr (ruleLhs rule)
          rhs' <- resolveExpr (ruleRhs rule)
          anns' <-
            if ruleLhsHeadIsTopLevel lhs'
              then pure (ruleAnns rule)
              else do
                sp <- currentSpan
                errorAnn <-
                  resolution
                    sp
                    (IdentifierNamed (ruleName rule))
                    ResolutionNamespaceTerm
                    (Unresolved "the left-hand side of a rule must be a top-level variable applied to arguments")
                pure (errorAnn : ruleAnns rule)
          pure
            rule
              { ruleAnns = anns',
                ruleTypeBinders = typeBinders',
                ruleBinders = binders',
                ruleLhs = lhs',
                ruleRhs = rhs'
              }

-- | Bind the pattern variables of a rule, each with a fresh local name.
-- The type of a binder is resolved in the scope of the rule's type
-- variables, which the caller has put in scope.
bindRuleBinders :: [RuleBinder] -> ResolveM (Scope, [RuleBinder])
bindRuleBinders =
  foldM step (emptyScope, [])
  where
    step (bound, acc) binder =
      withEffectiveSpan (sourceSpanFromAnns (ruleBinderAnns binder)) $ do
        ty' <- traverse resolveType (ruleBinderType binder)
        sp <- currentSpan
        let name = ruleBinderName binder
            key = renderUnqualifiedName name
        entity <- freshLocal
        name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Resolved entity) name
        pure (insertTerm key entity bound, acc <> [binder {ruleBinderName = name', ruleBinderType = ty'}])

-- | Whether the head of a resolved rule left-hand side is a top-level
-- variable: the function of an application chain, or the operator of an
-- infix expression.
ruleLhsHeadIsTopLevel :: Expr -> Bool
ruleLhsHeadIsTopLevel expr =
  case expr of
    EAnn _ inner -> ruleLhsHeadIsTopLevel inner
    EParen inner -> ruleLhsHeadIsTopLevel inner
    EApp fun _ -> ruleLhsHeadIsTopLevel fun
    ETypeApp fun _ -> ruleLhsHeadIsTopLevel fun
    EInfix _ op _ -> isTopLevelValue op
    EVar name -> isTopLevelValue name
    _ -> False
  where
    isTopLevelValue name =
      case [resolutionTarget ann | Just ann <- map fromAnnotation (nameAnns name)] of
        EntityGlobal {} : _ -> nameType name == NameVarId || nameType name == NameVarSym
        _ -> False

-- | Pragmas that only give optimisation or documentation hints.
-- The resolver accepts them and does not resolve their contents.
ignoredPragma :: PragmaType -> Bool
ignoredPragma pragma =
  case pragma of
    PragmaInline kind _
      | kind == "INLINE"
          || kind == "INLINABLE"
          || kind == "INLINEABLE"
          || kind == "NOINLINE"
          || kind == "NOINLINEABLE"
          || kind == "NOINLINABLE"
          || kind == "CONLIKE" ->
          True
    PragmaDeprecated _ -> True
    PragmaWarning _ -> True
    -- GHC ignores a file-header pragma between declarations.
    PragmaLanguage _ -> True
    PragmaUnknown rawText ->
      -- A phase such as @INLINE[1]@ is part of the first word.
      case T.words (T.toUpper (T.drop 3 rawText)) of
        keyword : _ -> T.takeWhile (/= '[') keyword `elem` ignoredPragmaKeywords
        [] -> False
    _ -> False

-- | Keywords of hint pragmas that do not change name resolution. The
-- file-header keywords are here because GHC ignores such a pragma between
-- declarations.
ignoredPragmaKeywords :: [Text]
ignoredPragmaKeywords =
  [ "INLINE",
    "INLINABLE",
    "INLINEABLE",
    "NOINLINE",
    "NOINLINABLE",
    "NOINLINEABLE",
    "CONLIKE",
    "RULES",
    "SPECIALISE",
    "SPECIALIZE",
    "SPECIALISE_INLINE",
    "SPECIALIZE_INLINE",
    "MINIMAL",
    "COMPLETE",
    "ANN",
    "OPAQUE",
    "DEPRECATED",
    "WARNING",
    "CFILES",
    "LANGUAGE",
    "OPTIONS",
    "OPTIONS_GHC",
    "OPTIONS_HADDOCK",
    "INCLUDE"
  ]

resolveValueDecl :: TermDefinition -> ValueDecl -> ResolveM ValueDecl
resolveValueDecl termDefinition valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      sp <- currentSpan
      name' <- resolveTermDefinitionAt sp termDefinition name
      FunctionBind name' <$> mapM resolveMatch matches
    PatternBind multTag pat rhs ->
      PatternBind multTag <$> resolvePatternDefinition termDefinition pat <*> resolveRhs rhs

-- | Resolve a pattern synonym declaration. The right-hand side pattern
-- binds the argument variables. The result gives the arguments that the
-- pattern does not bind.
resolvePatSynDecl :: TermDefinition -> PatSynDecl -> ResolveM (PatSynDecl, [Text])
resolvePatSynDecl termDefinition patSyn = do
  sp <- currentSpan
  let name = patSynDeclName patSyn
      nameSpan = patSynNameSpan sp patSyn
  name' <-
    case termDefinition name of
      Just resolved -> resolveUnqualifiedNameTo nameSpan ResolutionNamespaceTerm resolved name
      Nothing -> pure name
  (patScope, pat') <- bindPattern (patSynDeclPat patSyn)
  dir' <-
    case patSynDeclDir patSyn of
      PatSynExplicitBidirectional matches -> PatSynExplicitBidirectional <$> mapM resolveMatch matches
      dir -> pure dir
  let unboundArgs = [arg | arg <- patSynArgNames (patSynDeclArgs patSyn), not (Map.member arg (scopeTerms patScope))]
  pure (patSyn {patSynDeclName = name', patSynDeclPat = pat', patSynDeclDir = dir'}, unboundArgs)

patSynArgNames :: PatSynArgs -> [Text]
patSynArgNames args =
  case args of
    PatSynPrefixArgs names -> names
    PatSynInfixArgs left right -> [left, right]
    PatSynRecordArgs fields -> fields

-- | The span of the pattern synonym name. An infix name follows its left
-- argument.
patSynNameSpan :: Maybe SourceSpan -> PatSynDecl -> Maybe SourceSpan
patSynNameSpan sp patSyn =
  case patSynDeclArgs patSyn of
    PatSynInfixArgs left _ -> declKeywordNameSpan ("pattern " <> left <> " ") sp nameText'
    _ -> declKeywordNameSpan "pattern " sp nameText'
  where
    nameText' = unqualifiedNameText (patSynDeclName patSyn)

unboundPatSynArgAnnotation :: Maybe SourceSpan -> Text -> ResolveM Annotation
unboundPatSynArgAnnotation sp arg =
  resolution sp (IdentifierNamed arg) ResolutionNamespaceTerm (Unresolved "pattern synonym argument is not bound by the pattern")

resolveForeignDecl :: TermDefinition -> ForeignDecl -> ResolveM ForeignDecl
resolveForeignDecl termDefinition foreignDecl = do
  sp <- currentSpan
  name' <- resolveTermDefinitionAt sp termDefinition (foreignName foreignDecl)
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
  head' <- resolveBinderHeadKinds resolveHeadName (classDeclHead classDecl)
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
    ClassItemTypeSig names ty -> do
      scope <- currentScope
      sp <- currentSpan
      names' <- mapM (resolveTermDefinitionAt sp (topLevelTermDefinition scope)) names
      ClassItemTypeSig names' <$> resolveType ty
    ClassItemDefaultSig name ty -> ClassItemDefaultSig name <$> resolveType ty
    ClassItemDefault valueDecl -> do
      scope <- currentScope
      ClassItemDefault <$> withResetLocalSupply (resolveValueDecl (topLevelTermDefinition scope) valueDecl)
    ClassItemFixity {} -> pure classDeclItem
    ClassItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure classDeclItem
      | otherwise -> ClassItemAnn <$> unhandledSyntax ResolutionNamespaceTerm "ClassItemPragma" <*> pure classDeclItem
    ClassItemTypeFamilyDecl familyDecl -> ClassItemTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    ClassItemDataFamilyDecl familyDecl -> ClassItemDataFamilyDecl <$> resolveDataFamilyDecl "data " familyDecl
    ClassItemDefaultTypeInst familyInst -> ClassItemDefaultTypeInst <$> resolveTypeFamilyInst familyInst

resolveInstanceDecl :: InstanceDecl -> ResolveM InstanceDecl
resolveInstanceDecl instanceDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (instanceDeclForall instanceDecl)
  (context', head', items') <-
    extendScope forallScope $ do
      context' <- mapM resolveType (instanceDeclContext instanceDecl)
      head' <- resolveType (instanceDeclHead instanceDecl)
      items' <- mapM (resolveInstanceDeclItem (instanceHeadClass head')) (instanceDeclItems instanceDecl)
      pure (context', head', items')
  pure
    instanceDecl
      { instanceDeclForall = forallBinders',
        instanceDeclContext = context',
        instanceDeclHead = head',
        instanceDeclItems = items'
      }

-- | The class of a resolved instance head, with the name as written.
instanceHeadClass :: Type -> Maybe (Text, Entity)
instanceHeadClass ty =
  case ty of
    TAnn _ inner -> instanceHeadClass inner
    TParen inner -> instanceHeadClass inner
    TKindSig inner _ -> instanceHeadClass inner
    TApp fun _ -> instanceHeadClass fun
    TCon name Unpromoted ->
      listToMaybe
        [ (nameText name, resolutionTarget annotation)
        | annotation <- mapMaybe fromAnnotation (nameAnns name)
        ]
    _ -> Nothing

-- | A method binding or a method signature in an instance names a method
-- of the class. The lookup goes through the methods of the class and not
-- through the term scope, as GHC does. Thus the method name does not have
-- to be in scope. For example, the import list can name only the class,
-- or the class can be in scope only under a qualifier. A binder that is
-- not a method of the class is an error. If the class of the instance
-- head is not known, the term scope resolves the binder.
instanceMethodDefinition :: Maybe (Text, Entity) -> Scope -> TermDefinition
instanceMethodDefinition headClass scope name =
  case headClass of
    Just (className, resolvedClass@(EntityGlobal classGlobal))
      | methodLists@(_ : _) <- classMethodLists className resolvedClass ->
          Just
            ( if any (rendered `elem`) methodLists
                then Resolved (globalTerm (globalNamePackage classGlobal) (globalNameModule classGlobal) (unqualifiedNameText name))
                else Unresolved ("not a method of the class " <> T.unpack className)
            )
    _ -> Just (lookupTerm rendered scope)
  where
    rendered = renderUnqualifiedName name
    -- A class and its methods have the same defining module, so the
    -- resolved method name comes from the resolved class name.
    classMethodLists className resolvedClass =
      [ methods
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == Resolved resolvedClass,
        Just methods <- [Map.lookup className (scopeMethods candidate)]
      ]

-- | The scope that resolves the family name of an associated type instance
-- through the class of the instance head, like an instance method. The
-- family name can be out of scope when only the class is in scope, for
-- example through a qualified import.
associatedTypeInstanceScope :: Maybe (Text, Entity) -> Scope -> Type -> Scope
associatedTypeInstanceScope headClass scope lhs =
  case (headClass, typeHeadConstructorName lhs) of
    (Just (className, resolvedClass), Just familyName)
      | Unresolved _ <- lookupType familyName scope,
        found : _ <- associatedTypes className resolvedClass familyName ->
          emptyScope {scopeTypes = Map.singleton familyName found}
    _ -> emptyScope
  where
    associatedTypes className resolvedClass familyName =
      [ resolved
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == Resolved resolvedClass,
        familyName `elem` Map.findWithDefault [] className (scopeAssociatedTypes candidate),
        Resolved resolved@EntityGlobal {} <- [lookupType familyName candidate]
      ]

-- | The name of the type constructor at the head of a type application.
typeHeadConstructorName :: Type -> Maybe Text
typeHeadConstructorName ty =
  case ty of
    TAnn _ inner -> typeHeadConstructorName inner
    TParen inner -> typeHeadConstructorName inner
    TKindSig inner _ -> typeHeadConstructorName inner
    TApp fun _ -> typeHeadConstructorName fun
    TCon name Unpromoted -> Just (nameText name)
    TInfix _ name Unpromoted _ -> Just (nameText name)
    _ -> Nothing

resolveInstanceDeclItem :: Maybe (Text, Entity) -> InstanceDeclItem -> ResolveM InstanceDeclItem
resolveInstanceDeclItem headClass instanceDeclItem =
  case instanceDeclItem of
    InstanceItemAnn ann inner -> InstanceItemAnn ann <$> withPushedSpan ann (resolveInstanceDeclItem headClass inner)
    InstanceItemBind valueDecl -> do
      scope <- currentScope
      InstanceItemBind <$> withResetLocalSupply (resolveValueDecl (instanceMethodDefinition headClass scope) valueDecl)
    InstanceItemTypeSig names ty -> do
      scope <- currentScope
      sp <- currentSpan
      names' <- mapM (resolveTermDefinitionAt sp (instanceMethodDefinition headClass scope)) names
      InstanceItemTypeSig names' <$> resolveType ty
    InstanceItemFixity {} -> pure instanceDeclItem
    InstanceItemTypeFamilyInst familyInst -> do
      scope <- currentScope
      let familyScope = associatedTypeInstanceScope headClass scope (typeFamilyInstLhs familyInst)
      InstanceItemTypeFamilyInst <$> extendScope familyScope (resolveTypeFamilyInst familyInst)
    InstanceItemDataFamilyInst familyInst -> do
      scope <- currentScope
      let familyScope = associatedTypeInstanceScope headClass scope (dataFamilyInstHead familyInst)
      InstanceItemDataFamilyInst <$> extendScope familyScope (resolveDataFamilyInst familyInst)
    InstanceItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure instanceDeclItem
      | otherwise -> InstanceItemAnn <$> unhandledSyntax ResolutionNamespaceTerm "InstanceItemPragma" <*> pure instanceDeclItem

resolveStandaloneDerivingDecl :: StandaloneDerivingDecl -> ResolveM StandaloneDerivingDecl
resolveStandaloneDerivingDecl derivingDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (standaloneDerivingForall derivingDecl)
  (strategy', context', head') <-
    extendScope forallScope $
      (,,)
        <$> traverse resolveDerivingStrategy (standaloneDerivingStrategy derivingDecl)
        <*> mapM resolveType (standaloneDerivingContext derivingDecl)
        <*> resolveType (standaloneDerivingHead derivingDecl)
  pure
    derivingDecl
      { standaloneDerivingStrategy = strategy',
        standaloneDerivingForall = forallBinders',
        standaloneDerivingContext = context',
        standaloneDerivingHead = head'
      }

resolveDerivingClause :: DerivingClause -> ResolveM DerivingClause
resolveDerivingClause clause = do
  strategy' <- traverse resolveDerivingStrategy (derivingStrategy clause)
  classes' <-
    case derivingClasses clause of
      Left name -> Left <$> resolveTypeUseAtName name
      Right tys -> Right <$> mapM resolveType tys
  pure clause {derivingStrategy = strategy', derivingClasses = classes'}

resolveDerivingStrategy :: DerivingStrategy -> ResolveM DerivingStrategy
resolveDerivingStrategy strategy =
  case strategy of
    DerivingVia ty -> DerivingVia <$> resolveType ty
    _ -> pure strategy

resolveMatch :: Match -> ResolveM Match
resolveMatch match =
  withEffectiveSpan (sourceSpanFromAnns (matchAnns match)) $ do
    (patScope, pats') <- bindPatterns (matchPats match)
    rhsHere <- (rhsSpan (matchRhs match) <|>) <$> currentSpan
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
  withEffectiveSpan (peelGuardQualifierSpan qualifier) $ do
    scope <- currentScope
    let qualifierSpan = peelGuardQualifierSpan qualifier
        wrap = maybe id (GuardAnn . mkAnnotation) qualifierSpan
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
    -- An implicit parameter has no lexical binder. The type checker
    -- connects the use to a binding through constraint solving.
    EImplicitParam _ -> pure expr
    ETypeSyntax form ty -> ETypeSyntax form <$> resolveType ty
    EInt _ TInteger _ -> resolveIntegerLiteral expr
    EInt _ numericType _ -> resolvePrimitiveLiteralType numericType expr
    EFloat value TFractional text -> do
      integral <- numDecimalsIntegralValue value
      case integral of
        Just integer -> resolveIntegerLiteral (EInt integer TInteger text)
        Nothing -> resolveFractionalLiteral expr
    EFloat _ floatType _ ->
      maybe (resolveFractionalLiteral expr) (`resolvePrimitiveLiteralTypeName` expr) (primitiveFloatTypeName floatType)
    EChar {} -> pure expr
    ECharHash {} -> resolvePrimitiveLiteralTypeName "Char#" expr
    EString {} -> resolveStringLiteral expr
    EStringHash {} -> resolvePrimitiveLiteralTypeName "Addr#" expr
    EOverloadedLabel {} -> pure expr
    EIf cond trueBranch falseBranch -> do
      resolved <- EIf <$> resolveExpr cond <*> resolveExpr trueBranch <*> resolveExpr falseBranch
      annotateRebindableIf resolved
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
    -- The parser only builds this for the view-pattern arrow, so it reaches
    -- the resolver only when a view pattern appears where a pattern cannot.
    EViewPat lhs rhs ->
      EViewPat <$> resolveExpr lhs <*> resolveExpr rhs
    ENegate inner ->
      annotateNegate . ENegate =<< resolveExpr inner
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
    ERecordCon name fields wildcard -> do
      name' <- resolveTermUse name
      fields' <- resolveRecordFields fields
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (nameAnns name') <|> ambient
      wildcardFields <- resolveRecordConWildcardFields sp name fields wildcard
      pure (ERecordCon name' (fields' <> wildcardFields) False)
    ERecordUpd record fields ->
      ERecordUpd <$> resolveExpr record <*> resolveRecordUpdateFields fields
    EGetField record name ->
      EGetField <$> resolveExpr record <*> pure name
    EGetFieldProjection {} -> pure expr
    ETypeSig inner ty ->
      ETypeSig <$> resolveExpr inner <*> resolveType ty
    EParen inner ->
      EParen <$> resolveExpr inner
    EList items -> do
      items' <- mapM resolveExpr items
      sp <- currentSpan
      annotation <- resolution sp IdentifierList ResolutionNamespaceTerm (Resolved EntitySyntax)
      info <- currentModuleInfo
      let list = EAnn annotation (EList items')
      if OverloadedLists `elem` moduleInfoExtensions info
        then annotateSyntaxTerm "fromListN" list
        else pure list
    ETuple flavor items -> do
      items' <- mapM resolveMaybeExpr items
      sp <- currentSpan
      annotation <- resolution sp (IdentifierTuple flavor (length items)) ResolutionNamespaceTerm (Resolved EntitySyntax)
      pure (EAnn annotation (ETuple flavor items'))
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
    EQuasiQuote {} -> EAnn <$> unhandledSyntax ResolutionNamespaceTerm "EQuasiQuote" <*> pure expr
    EListComp body stmts -> do
      (scope, stmts') <- resolveCompStmts stmts
      body' <- withScope scope (resolveExpr body)
      pure (EListComp body' stmts')
    EListCompParallel {} -> EAnn <$> unhandledSyntax ResolutionNamespaceTerm "EListCompParallel" <*> pure expr
    -- Template Haskell quotes compile to a runtime error. The quoted
    -- syntax stays unresolved because nothing consumes it.
    ETHExpQuote {} -> pure expr
    ETHTypedQuote {} -> pure expr
    ETHDeclQuote {} -> pure expr
    ETHTypeQuote {} -> pure expr
    ETHPatQuote {} -> pure expr
    ETHNameQuote {} -> pure expr
    ETHTypeNameQuote {} -> pure expr
    EProc {} -> EAnn <$> unhandledSyntax ResolutionNamespaceTerm "EProc" <*> pure expr

-- | An overloaded integer literal applies fromInteger to an Integer.
--
-- The fromInteger annotation sits inside the Integer type annotation.
resolveIntegerLiteral :: Expr -> ResolveM Expr
resolveIntegerLiteral expr = do
  sp <- currentSpan
  maybeIntegerAnn <- integerTypeAnnotation sp
  annotated <- annotateSyntaxTerm "fromInteger" expr
  pure (maybe annotated (`EAnn` annotated) maybeIntegerAnn)

-- | An overloaded fractional literal applies fromRational to a Rational.
--
-- The type of the Rational comes from the type of the method, so the
-- literal gets only the fromRational term.
resolveFractionalLiteral :: Expr -> ResolveM Expr
resolveFractionalLiteral = annotateSyntaxTerm "fromRational"

-- | The integer value of a fractional literal under NumDecimals.
--
-- GHC's renamer changes a fractional literal with an integral value, such
-- as @1e12@, into an integer literal. The literal then has the type
-- @Num a => a@. The result is 'Nothing' without the extension or for a
-- value that is not integral.
numDecimalsIntegralValue :: Rational -> ResolveM (Maybe Integer)
numDecimalsIntegralValue value = do
  info <- currentModuleInfo
  pure $
    if NumDecimals `elem` moduleInfoExtensions info && denominator value == 1
      then Just (numerator value)
      else Nothing

-- | A fractional literal in a pattern with the NumDecimals change of
-- 'numDecimalsIntegralValue'. The annotations of the literal stay.
numDecimalsLiteral :: Literal -> ResolveM Literal
numDecimalsLiteral lit =
  case lit of
    LitAnn ann inner -> LitAnn ann <$> numDecimalsLiteral inner
    LitFloat value TFractional text ->
      maybe lit (\integer -> LitInt integer TInteger text) <$> numDecimalsIntegralValue value
    _ -> pure lit

-- | OverloadedStrings applies fromString to a String literal.
--
-- The argument type comes from the type of the method, so the literal gets
-- only the fromString term. Without the extension a string literal keeps
-- its [Char] type and gets no annotation.
resolveStringLiteral :: Expr -> ResolveM Expr
resolveStringLiteral expr = do
  info <- currentModuleInfo
  if OverloadedStrings `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "fromString" expr
    else pure expr

-- | The resolution of the Integer type in the built-in scope.
--
-- The result is 'Nothing' when the built-in scope does not have Integer.
-- A module without Integer then gives the literal only the fromInteger term.
integerTypeAnnotation :: Maybe SourceSpan -> ResolveM (Maybe Annotation)
integerTypeAnnotation sp = do
  info <- currentModuleInfo
  case lookupType "Integer" (moduleInfoBuiltinScope info) of
    Unresolved _ -> pure Nothing
    resolved -> Just <$> resolution sp (IdentifierNamed "Integer") ResolutionNamespaceType resolved

-- | Annotate an expression with the syntax term that its desugaring applies.
--
-- The term comes from the built-in scope.
-- RebindableSyntax takes the term from the lexical scope instead.
annotateSyntaxTerm :: Text -> Expr -> ResolveM Expr
annotateSyntaxTerm name expr = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (EAnn annotation expr)

-- | Negation applies the negate syntax term to its operand.
annotateNegate :: Expr -> ResolveM Expr
annotateNegate = annotateSyntaxTerm "negate"

resolvePrimitiveLiteralType :: NumericType -> Expr -> ResolveM Expr
resolvePrimitiveLiteralType numericType expr =
  case primitiveNumericTypeName numericType of
    Just name -> resolvePrimitiveLiteralTypeName name expr
    Nothing -> resolveIntegerLiteral expr

resolvePrimitiveLiteralTypeName :: Text -> Expr -> ResolveM Expr
resolvePrimitiveLiteralTypeName name expr = do
  sp <- currentSpan
  annotation <- primitiveLiteralTypeAnnotation sp name
  pure (EAnn annotation expr)

-- | The resolution annotation that names the type of a primitive literal.
primitiveLiteralTypeAnnotation :: Maybe SourceSpan -> Text -> ResolveM Annotation
primitiveLiteralTypeAnnotation sp name = do
  info <- currentModuleInfo
  resolution sp (IdentifierNamed name) ResolutionNamespaceType (lookupType name (moduleInfoBuiltinScope info))

-- | The type name of a primitive literal, or 'Nothing' for a boxed literal.
primitiveLiteralTypeName :: Literal -> Maybe Text
primitiveLiteralTypeName literal =
  case literal of
    LitAnn _ inner -> primitiveLiteralTypeName inner
    LitInt _ numericType _ -> primitiveNumericTypeName numericType
    LitFloat _ floatType _ -> primitiveFloatTypeName floatType
    LitChar {} -> Nothing
    LitCharHash {} -> Just "Char#"
    LitString {} -> Nothing
    LitStringHash {} -> Just "Addr#"

primitiveFloatTypeName :: FloatType -> Maybe Text
primitiveFloatTypeName floatType =
  case floatType of
    TFractional -> Nothing
    TFloatHash -> Just "Float#"
    TDoubleHash -> Just "Double#"

primitiveNumericTypeName :: NumericType -> Maybe Text
primitiveNumericTypeName numericType =
  case numericType of
    TInteger -> Nothing
    TIntHash -> Just "Int#"
    TWordHash -> Just "Word#"
    TInt8Hash -> Just "Int8#"
    TInt16Hash -> Just "Int16#"
    TInt32Hash -> Just "Int32#"
    TInt64Hash -> Just "Int64#"
    TWord8Hash -> Just "Word8#"
    TWord16Hash -> Just "Word16#"
    TWord32Hash -> Just "Word32#"
    TWord64Hash -> Just "Word64#"

-- | RebindableSyntax gives an if expression the in-scope ifThenElse.
-- An ordinary if expression uses the built-in Bool and gets no annotation.
annotateRebindableIf :: Expr -> ResolveM Expr
annotateRebindableIf expr = do
  info <- currentModuleInfo
  if RebindableSyntax `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "ifThenElse" expr
    else pure expr

-- | Annotate a literal pattern with the names that the type checker needs.
--
-- An overloaded integer pattern gets the syntax terms that compare it.
-- A string pattern gets them only under OverloadedStrings.
-- A primitive literal pattern gets the resolution of its primitive type.
annotatePatternLiteral :: (Literal -> Pattern) -> Literal -> ResolveM Pattern
annotatePatternLiteral mkPattern sourceLit = do
  lit <- numDecimalsLiteral sourceLit
  let pat = mkPattern lit
  sp <- (literalSpan lit <|>) <$> currentSpan
  case primitiveLiteralTypeName lit of
    Just typeName -> do
      typeAnn <- primitiveLiteralTypeAnnotation sp typeName
      pure (PAnn typeAnn pat)
    Nothing ->
      case peelLiteralAnn lit of
        LitInt _ TInteger _ -> do
          maybeIntegerAnn <- integerTypeAnnotation sp
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromInteger")
          pure (foldr PAnn pat (maybe methodAnns (: methodAnns) maybeIntegerAnn))
        LitFloat _ TFractional _ -> do
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromRational")
          pure (foldr PAnn pat methodAnns)
        LitString _ _ -> do
          info <- currentModuleInfo
          if OverloadedStrings `elem` moduleInfoExtensions info
            then do
              methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromString")
              pure (foldr PAnn pat methodAnns)
            else pure pat
        _ -> pure pat
  where
    -- A negated literal pattern also negates the converted literal.
    overloadedPatternMethods conversion =
      case peelPatternAnn (mkPattern sourceLit) of
        PNegLit {} -> [conversion, "negate", "=="]
        _ -> [conversion, "=="]

-- | The innermost source span a literal's annotations carry.
literalSpan :: Literal -> Maybe SourceSpan
literalSpan = go Nothing
  where
    go ambient (LitAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient _ = ambient

syntaxTermAnnotation :: Maybe SourceSpan -> Text -> ResolveM Annotation
syntaxTermAnnotation sp name = do
  resolved <- resolveSyntaxTerm name
  resolution sp (IdentifierNamed name) ResolutionNamespaceTerm resolved

resolveSyntaxTerm :: Text -> ResolveM Resolution
resolveSyntaxTerm name = do
  scope <- currentScope
  info <- currentModuleInfo
  pure $
    if RebindableSyntax `elem` moduleInfoExtensions info
      then rebindableSyntaxTerm info scope name
      else builtinSyntaxTerm info name

builtinSyntaxTerm :: ModuleInfo -> Text -> Resolution
builtinSyntaxTerm info name =
  if name `elem` builtinSyntaxTermNames
    then lookupTerm name (moduleInfoBuiltinScope info)
    else Unresolved "unknown built-in syntax term"
  where
    builtinSyntaxTermNames =
      [ "fromInteger",
        "fromRational",
        "fromString",
        "fromListN",
        "negate",
        "==",
        ">>=",
        ">>",
        "enumFrom",
        "enumFromThen",
        "enumFromTo",
        "enumFromThenTo"
      ]

rebindableSyntaxTerm :: ModuleInfo -> Scope -> Text -> Resolution
rebindableSyntaxTerm info scope name =
  case lookupTerm name scope of
    Resolved (EntityGlobal global)
      | globalNameModule global == "Prelude",
        not (moduleInfoExplicitPreludeImport info) ->
          Unresolved "unbound"
    resolved -> resolved

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

-- | A record update names the field selectors that it writes, and the
-- selector must be in scope. The resolution of each label tells the type
-- checker which data type the update rebuilds. Two data types can declare a
-- field with the same name, and then only the resolution separates them.
resolveRecordUpdateFields :: [RecordField Expr] -> ResolveM [RecordField Expr]
resolveRecordUpdateFields =
  mapM
    ( \field -> do
        name' <- resolveTermUseAtName (recordFieldName field)
        value' <- resolveExpr (recordFieldValue field)
        pure field {recordFieldName = name', recordFieldValue = value'}
    )

-- | A record wildcard in a construction fills each remaining field with the
-- variable that has the field name. The construction lists these fields as
-- puns, so a later phase sees an ordinary record construction.
resolveRecordConWildcardFields :: Maybe SourceSpan -> Name -> [RecordField Expr] -> Bool -> ResolveM [RecordField Expr]
resolveRecordConWildcardFields sp conName fields wildcard = do
  scope <- currentScope
  mapM fieldPun (recordWildcardFieldNames (scopeRecordFields scope) conName fields wildcard)
  where
    fieldPun fieldName = do
      value <- resolveTermUse (Name Nothing NameVarId fieldName (map mkAnnotation (maybeToList sp)))
      pure
        RecordField
          { recordFieldName = Name Nothing NameVarId fieldName [],
            recordFieldValue = EVar value,
            recordFieldPun = True
          }

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
            (scope', stmt') <- resolveDoStmt (null rest) stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveDoStmt :: Bool -> DoStmt Expr -> ResolveM (Scope, DoStmt Expr)
resolveDoStmt isLast stmt =
  case stmt of
    DoAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveDoStmt isLast inner)
      pure (scope', DoAnn ann inner')
    DoExpr body -> do
      scope <- currentScope
      body' <- resolveExpr body
      stmt' <- annotateDoMethod isLast ">>" (DoExpr body')
      pure (scope, stmt')
    DoBind pat body -> do
      scope <- currentScope
      body' <- resolveExpr body
      (patScope, pat') <- bindPattern pat
      stmt' <- annotateDoMethod isLast ">>=" (DoBind pat' body')
      pure (unionScope patScope scope, stmt')
    DoLetDecls decls -> do
      scope <- currentScope
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, DoLetDecls decls')
    DoRecStmt stmts -> do
      scope <- currentScope
      (_, stmts') <- resolveDoStmts stmts
      pure (scope, DoRecStmt stmts')

-- | Annotate a do statement with the method that sequences it.
--
-- A bind statement uses @>>=@ and an expression statement uses @>>@.
-- The last statement is the result of the block and gets no method.
-- Ordinary do notation uses the built-in Monad methods.
-- RebindableSyntax uses lexical lookup instead.
annotateDoMethod :: Bool -> Text -> DoStmt Expr -> ResolveM (DoStmt Expr)
annotateDoMethod isLast name stmt
  | isLast = pure stmt
  | otherwise = do
      sp <- currentSpan
      methodAnn <- syntaxTermAnnotation sp name
      pure (DoAnn methodAnn stmt)

resolveArithSeq :: ArithSeq -> ResolveM ArithSeq
resolveArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner ->
      ArithSeqAnn ann <$> withPushedSpan ann (resolveArithSeq inner)
    ArithSeqFrom from -> do
      resolved <- ArithSeqFrom <$> resolveExpr from
      annotateArithSeqMethod "enumFrom" resolved
    ArithSeqFromThen from then' -> do
      resolved <- ArithSeqFromThen <$> resolveExpr from <*> resolveExpr then'
      annotateArithSeqMethod "enumFromThen" resolved
    ArithSeqFromTo from to -> do
      resolved <- ArithSeqFromTo <$> resolveExpr from <*> resolveExpr to
      annotateArithSeqMethod "enumFromTo" resolved
    ArithSeqFromThenTo from then' to -> do
      resolved <- ArithSeqFromThenTo <$> resolveExpr from <*> resolveExpr then' <*> resolveExpr to
      annotateArithSeqMethod "enumFromThenTo" resolved

annotateArithSeqMethod :: Text -> ArithSeq -> ResolveM ArithSeq
annotateArithSeqMethod name arithSeq = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (ArithSeqAnn annotation arithSeq)

resolveBoundDecls :: Map.Map Text Entity -> Map.Map Text Scope -> [Decl] -> ResolveM [Decl]
resolveBoundDecls binderTargets signatureScopes decls = do
  decls' <- markMixedImplicitParamGroup decls
  resolveBindingGroup (\name -> Resolved <$> Map.lookup (renderUnqualifiedName name) binderTargets) signatureScopes decls'

-- | Mark each implicit-parameter binding in a group that also has other declarations.
--
-- GHC does not permit a @let@ or @where@ group with both kinds of binding.
markMixedImplicitParamGroup :: [Decl] -> ResolveM [Decl]
markMixedImplicitParamGroup decls
  | any isImplicitParamDecl decls && not (all isImplicitParamDecl decls) = mapM mark decls
  | otherwise = pure decls
  where
    isImplicitParamDecl decl =
      case snd (peelDeclSpan decl) of
        DeclImplicitParam {} -> True
        _ -> False
    mark decl =
      case peelDeclSpan decl of
        (declSpan, DeclImplicitParam name _ _) -> do
          ambient <- currentSpan
          ann <-
            resolution
              (spanStartNameSpan (declSpan <|> ambient) name)
              (IdentifierNamed name)
              ResolutionNamespaceTerm
              (Unresolved "implicit-parameter binding in a group with other bindings")
          pure (DeclAnn ann decl)
        _ -> pure decl

declSignatureScope :: Decl -> Map.Map Text Scope -> Maybe Scope
declSignatureScope decl signatureScopes =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) signatureScopes
    Nothing -> Nothing

-- | Bind a sequence of patterns left to right. A view pattern's expression
-- sees the variables that the patterns before it bind, so
-- @f egr (find egr -> i)@ and @(x, f x -> y)@ resolve as GHC scopes them.
bindPatterns :: [Pattern] -> ResolveM (Scope, [Pattern])
bindPatterns = go emptyScope
  where
    go bound [] = pure (bound, [])
    go bound (pat : pats) = do
      (scope, pat') <- extendScope bound (bindPattern pat)
      (bound', pats') <- go (scope `unionScope` bound) pats
      pure (bound', pat' : pats')

bindPattern :: Pattern -> ResolveM (Scope, Pattern)
bindPattern pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann $ do
        (scope, inner') <- bindPattern inner
        pure (scope, PAnn ann inner')
    PVar name -> do
      sp <- currentSpan
      entity <- freshLocal
      let key = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Resolved entity) name
      pure (termScope key entity, PVar name')
    PTypeBinder binder -> do
      entity <- freshLocal
      binder' <- traverseTyVarBinderKind binder
      let binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) entity) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      pure (binderScope, PTypeBinder binder')
    PTypeSyntax form ty -> do
      ty' <- resolveType ty
      pure (emptyScope, PTypeSyntax form ty')
    PWildcard -> pure (emptyScope, pat)
    PLit lit -> do
      pat' <- annotatePatternLiteral PLit lit
      pure (emptyScope, pat')
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
    -- A built-in constructor has no name for a scope to bind, so only the
    -- type arguments and the sub-patterns need resolving.
    PBuiltinCon builtin typeArgs pats -> do
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, PBuiltinCon builtin typeArgs' pats')
    PInfix {} -> do
      bound <- traverse bindPattern (flattenInfixPattern pat)
      let scope = foldr (\(operandScope, _) acc -> unionScope acc operandScope) emptyScope bound
      pat' <- resolveInfixChain PInfix (fmap snd bound)
      pure (scope, pat')
    PView expr inner -> do
      expr' <- resolveExpr expr
      (scope, inner') <- bindPattern inner
      pure (scope, PView expr' inner')
    PAs alias inner -> do
      here <- currentSpan
      let aliasKey = renderUnqualifiedName alias
      aliasEntity <- freshLocal
      alias' <- resolveUnqualifiedNameTo (spanStartNameSpan here aliasKey) ResolutionNamespaceTerm (Resolved aliasEntity) alias
      let aliasScope = termScope aliasKey aliasEntity
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
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (nameAnns name') <|> ambient
      -- A record wildcard binds each remaining field to a variable with the
      -- field name. The pattern lists these fields as puns, so a later phase
      -- sees an ordinary record pattern.
      let wildcardScope = Scope (Map.fromList wildcardEntries) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      wildcardFields <-
        mapM
          ( \(fieldName, entity) -> do
              let binder = (mkUnqualifiedName NameVarId fieldName) {unqualifiedNameAnns = map mkAnnotation (maybeToList sp)}
              binder' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Resolved entity) binder
              pure
                RecordField
                  { recordFieldName = Name Nothing NameVarId fieldName [],
                    recordFieldValue = PVar binder',
                    recordFieldPun = True
                  }
          )
          wildcardEntries
      pure (foldr unionScope wildcardScope fieldScopes, PRecord name' (fields' <> wildcardFields) False)
    PTypeSig inner ty -> do
      (scope, inner') <- bindPattern inner
      ty' <- resolveType ty
      pure (scope, PTypeSig inner' ty')
    PNegLit lit -> do
      pat' <- annotatePatternLiteral PNegLit lit
      pure (emptyScope, pat')
    PSplice expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, PSplice expr')
    PQuasiQuote {} -> do
      ann <- unhandledSyntax ResolutionNamespaceTerm "PQuasiQuote"
      pure (emptyScope, PAnn ann pat)
  where
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

termScope :: Text -> Entity -> Scope
termScope key resolvedName =
  Scope (Map.singleton key resolvedName) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

resolvePatternDefinition :: TermDefinition -> Pattern -> ResolveM Pattern
resolvePatternDefinition termDefinition pat =
  case pat of
    PAnn ann inner ->
      PAnn ann <$> withPushedSpan ann (resolvePatternDefinition termDefinition inner)
    PVar name -> do
      sp <- currentSpan
      PVar <$> resolveTermDefinitionAt sp termDefinition name
    PTypeBinder binder -> do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure (PTypeBinder (binder {tyVarBinderKind = kind'}))
    PTypeSyntax form ty ->
      PTypeSyntax form <$> resolveType ty
    PWildcard -> pure pat
    PLit lit -> annotatePatternLiteral PLit lit
    PQuasiQuote {} -> PAnn <$> unhandledSyntax ResolutionNamespaceTerm "PQuasiQuote" <*> pure pat
    PTuple flavor pats ->
      PTuple flavor <$> mapM (resolvePatternDefinition termDefinition) pats
    PUnboxedSum alt arity inner ->
      PUnboxedSum alt arity <$> resolvePatternDefinition termDefinition inner
    PList pats ->
      PList <$> mapM (resolvePatternDefinition termDefinition) pats
    PCon name typeArgs pats ->
      PCon <$> resolveTermUseAtName name <*> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PBuiltinCon builtin typeArgs pats ->
      PBuiltinCon builtin <$> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PInfix {} -> do
      operands <- traverse (resolvePatternDefinition termDefinition) (flattenInfixPattern pat)
      resolveInfixChain PInfix operands
    PView expr inner ->
      PView <$> withResetLocalSupply (resolveExpr expr) <*> resolvePatternDefinition termDefinition inner
    PAs alias inner -> do
      sp <- currentSpan
      PAs <$> resolveTermDefinitionAt sp termDefinition alias <*> resolvePatternDefinition termDefinition inner
    PStrict inner ->
      PStrict <$> resolvePatternDefinition termDefinition inner
    PIrrefutable inner ->
      PIrrefutable <$> resolvePatternDefinition termDefinition inner
    PNegLit lit -> annotatePatternLiteral PNegLit lit
    PParen inner ->
      PParen <$> resolvePatternDefinition termDefinition inner
    PRecord name fields wildcard -> do
      name' <- resolveTermUseAtName name
      fields' <-
        mapM
          ( \field -> do
              value' <- resolvePatternDefinition termDefinition (recordFieldValue field)
              pure field {recordFieldValue = value'}
          )
          fields
      wildcardNames <- wildcardFieldNames name fields wildcard
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (nameAnns name') <|> ambient
      -- A record wildcard binds each remaining field to a variable with the
      -- field name. The pattern lists these fields as puns, so a later phase
      -- sees an ordinary record pattern.
      wildcardFields <-
        mapM
          ( \fieldName -> do
              let binder = (mkUnqualifiedName NameVarId fieldName) {unqualifiedNameAnns = map mkAnnotation (maybeToList sp)}
              binder' <- resolveTermDefinitionAt sp termDefinition binder
              pure
                RecordField
                  { recordFieldName = Name Nothing NameVarId fieldName [],
                    recordFieldValue = PVar binder',
                    recordFieldPun = True
                  }
          )
          wildcardNames
      pure (PRecord name' (fields' <> wildcardFields) False)
    PTypeSig inner ty ->
      PTypeSig <$> resolvePatternDefinition termDefinition inner <*> resolveType ty
    PSplice expr ->
      PSplice <$> withResetLocalSupply (resolveExpr expr)

bindRecordWildcardFields :: Name -> [RecordField Pattern] -> Bool -> ResolveM [(Text, Entity)]
bindRecordWildcardFields conName fields wildcard =
  mapM bindField =<< wildcardFieldNames conName fields wildcard
  where
    bindField fieldName = do
      entity <- freshLocal
      pure (fieldName, entity)

-- | The field names that a record wildcard binds, taken from the
-- constructor in the current scope.
wildcardFieldNames :: Name -> [RecordField Pattern] -> Bool -> ResolveM [Text]
wildcardFieldNames conName fields wildcard = do
  scope <- currentScope
  pure (recordWildcardFieldNames (scopeRecordFields scope) conName fields wildcard)

resolveDataDecl :: Text -> DataDecl -> ResolveM DataDecl
resolveDataDecl keyword dataDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan keyword declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (dataDeclHead dataDecl)
  context' <- mapM resolveType (dataDeclContext dataDecl)
  kind' <- traverse resolveType (dataDeclKind dataDecl)
  constructors' <- mapM resolveDataConDecl (dataDeclConstructors dataDecl) >>= mapM (resolveDataConDefinitions scope)
  deriving' <- mapM resolveDerivingClause (dataDeclDeriving dataDecl)
  pure
    dataDecl
      { dataDeclHead = head',
        dataDeclContext = context',
        dataDeclKind = kind',
        dataDeclConstructors = constructors',
        dataDeclDeriving = deriving'
      }

resolveTypeFamilyDecl :: TypeFamilyDecl -> ResolveM TypeFamilyDecl
resolveTypeFamilyDecl familyDecl = do
  (paramScope, params') <- bindTyVarBinders (typeFamilyDeclParams familyDecl)
  (head', resultSig', equations') <-
    extendScope paramScope $
      (,,)
        <$> resolveType (typeFamilyDeclHead familyDecl)
        <*> traverse resolveTypeFamilyResultSig (typeFamilyDeclResultSig familyDecl)
        <*> traverse (mapM resolveTypeFamilyEq) (typeFamilyDeclEquations familyDecl)
  pure
    familyDecl
      { typeFamilyDeclHead = head',
        typeFamilyDeclParams = params',
        typeFamilyDeclResultSig = resultSig',
        typeFamilyDeclEquations = equations'
      }

resolveTypeFamilyResultSig :: TypeFamilyResultSig -> ResolveM TypeFamilyResultSig
resolveTypeFamilyResultSig resultSig =
  case resultSig of
    TypeFamilyKindSig ty -> TypeFamilyKindSig <$> resolveType ty
    TypeFamilyTyVarSig binder -> TypeFamilyTyVarSig <$> resolveTyVarBinderKind binder
    TypeFamilyInjectiveSig binder injectivity ->
      TypeFamilyInjectiveSig <$> resolveTyVarBinderKind binder <*> pure injectivity

-- | Resolve a declaration head: its own name, and the kind signature of
-- every type variable it binds. A kind signature in a binder mentions
-- type constructors -- @(s :: Type -> Type)@ names @Type@ -- and those
-- have to be resolved like any other type, or the type checker is left to
-- guess the constructor from its unqualified name.
resolveBinderHeadKinds ::
  (UnqualifiedName -> ResolveM UnqualifiedName) ->
  BinderHead UnqualifiedName ->
  ResolveM (BinderHead UnqualifiedName)
resolveBinderHeadKinds resolveHeadName head' =
  case head' of
    PrefixBinderHead name params ->
      PrefixBinderHead <$> resolveHeadName name <*> mapM resolveTyVarBinderKind params
    InfixBinderHead lhs name rhs params ->
      InfixBinderHead
        <$> resolveTyVarBinderKind lhs
        <*> resolveHeadName name
        <*> resolveTyVarBinderKind rhs
        <*> mapM resolveTyVarBinderKind params

resolveTyVarBinderKind :: TyVarBinder -> ResolveM TyVarBinder
resolveTyVarBinderKind binder = do
  kind' <- traverse resolveType (tyVarBinderKind binder)
  pure binder {tyVarBinderKind = kind'}

resolveTypeFamilyEq :: TypeFamilyEq -> ResolveM TypeFamilyEq
resolveTypeFamilyEq equation = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyEqForall equation)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> resolveType (typeFamilyEqLhs equation)
        <*> resolveType (typeFamilyEqRhs equation)
  pure
    equation
      { typeFamilyEqForall = forallBinders',
        typeFamilyEqLhs = lhs',
        typeFamilyEqRhs = rhs'
      }

resolveTypeFamilyInst :: TypeFamilyInst -> ResolveM TypeFamilyInst
resolveTypeFamilyInst familyInst = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyInstForall familyInst)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> resolveType (typeFamilyInstLhs familyInst)
        <*> resolveType (typeFamilyInstRhs familyInst)
  pure
    familyInst
      { typeFamilyInstForall = forallBinders',
        typeFamilyInstLhs = lhs',
        typeFamilyInstRhs = rhs'
      }

-- | Resolve a data family declaration. The keyword is what the declaration
-- starts with, which locates the family name inside the declaration span:
-- @data family@ at the top level and @data@ inside a class body.
resolveDataFamilyDecl :: Text -> DataFamilyDecl -> ResolveM DataFamilyDecl
resolveDataFamilyDecl keyword familyDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan keyword declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (dataFamilyDeclHead familyDecl)
  kind' <- traverse resolveType (dataFamilyDeclKind familyDecl)
  pure familyDecl {dataFamilyDeclHead = head', dataFamilyDeclKind = kind'}

resolveDataFamilyInst :: DataFamilyInst -> ResolveM DataFamilyInst
resolveDataFamilyInst familyInst = do
  scope <- currentScope
  (forallScope, forallBinders') <- bindTyVarBinders (dataFamilyInstForall familyInst)
  (head', kind', constructors', deriving') <-
    extendScope forallScope $
      (,,,)
        <$> resolveType (dataFamilyInstHead familyInst)
        <*> traverse resolveType (dataFamilyInstKind familyInst)
        <*> (mapM resolveDataConDecl (dataFamilyInstConstructors familyInst) >>= mapM (resolveDataConDefinitions scope))
        <*> mapM resolveDerivingClause (dataFamilyInstDeriving familyInst)
  pure
    familyInst
      { dataFamilyInstForall = forallBinders',
        dataFamilyInstHead = head',
        dataFamilyInstKind = kind',
        dataFamilyInstConstructors = constructors',
        dataFamilyInstDeriving = deriving'
      }

resolveTypeSynDecl :: TypeSynDecl -> ResolveM TypeSynDecl
resolveTypeSynDecl typeSynDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "type " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (typeSynHead typeSynDecl)
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
  head' <- resolveBinderHeadKinds resolveHeadName (newtypeDeclHead newtypeDecl)
  kind' <- traverse resolveType (newtypeDeclKind newtypeDecl)
  constructor' <- traverse resolveDataConDecl (newtypeDeclConstructor newtypeDecl) >>= traverse (resolveDataConDefinitions scope)
  deriving' <- mapM resolveDerivingClause (newtypeDeclDeriving newtypeDecl)
  pure
    newtypeDecl
      { newtypeDeclHead = head',
        newtypeDeclKind = kind',
        newtypeDeclConstructor = constructor',
        newtypeDeclDeriving = deriving'
      }

resolveDataConDecl :: DataConDecl -> ResolveM DataConDecl
resolveDataConDecl dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> DataConAnn ann <$> withPushedSpan ann (resolveDataConDecl inner)
    PrefixCon forallVars context name bangTypes -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        PrefixCon forallVars' <$> mapM resolveType context <*> pure name <*> mapM resolveBangType bangTypes
    InfixCon forallVars context lhs name rhs -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        InfixCon forallVars' <$> mapM resolveType context <*> resolveBangType lhs <*> pure name <*> resolveBangType rhs
    RecordCon forallVars context name fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        RecordCon forallVars' <$> mapM resolveType context <*> pure name <*> mapM resolveFieldDecl fields
    GadtCon telescopes context names body -> do
      (forallScope, telescopes') <- bindForallTelescopes telescopes
      extendScope forallScope $
        GadtCon telescopes' <$> mapM resolveType context <*> pure names <*> resolveGadtBody body
    TupleCon forallVars context flavor fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        TupleCon forallVars' <$> mapM resolveType context <*> pure flavor <*> mapM resolveBangType fields
    UnboxedSumCon forallVars context pos arity field -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        UnboxedSumCon forallVars' <$> mapM resolveType context <*> pure pos <*> pure arity <*> resolveBangType field
    ListCon forallVars context -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        ListCon forallVars' <$> mapM resolveType context
  where
    resolveBangType bt = do
      ty' <- resolveType (bangType bt)
      pure bt {bangType = ty'}
    resolveFieldDecl fieldDecl = do
      fieldType' <- resolveBangType (fieldType fieldDecl)
      pure fieldDecl {fieldType = fieldType'}

-- | Bind the type variables of a chain of @forall@ telescopes.
-- Each telescope is in the scope of the telescopes before it.
bindForallTelescopes :: [ForallTelescope] -> ResolveM (Scope, [ForallTelescope])
bindForallTelescopes = foldM step (emptyScope, [])
  where
    step (scope, acc) telescope = do
      (binderScope, binders') <- extendScope scope (bindTyVarBinders (forallTelescopeBinders telescope))
      pure (binderScope `unionScope` scope, acc <> [telescope {forallTelescopeBinders = binders'}])

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
      TCon <$> resolveTypeConstructorUse promoted name <*> pure promoted
    TBuiltinCon {} -> pure ty
    TImplicitParam name inner ->
      TImplicitParam name <$> resolveType inner
    TTypeLit {} -> pure ty
    TStar {} -> pure ty
    TForall telescope inner -> do
      (binderScope, binders') <- withResetLocalSupply (bindTyVarBinders (forallTelescopeBinders telescope))
      inner' <- extendScope binderScope (resolveType inner)
      pure (TForall (telescope {forallTelescopeBinders = binders'}) inner')
    TApp left right ->
      TApp <$> resolveType left <*> resolveType right
    TTypeApp left right ->
      TTypeApp <$> resolveType left <*> resolveType right
    TInfix {} -> resolveInfixType ty
    TFun arrowKind left right ->
      TFun <$> resolveArrowKind arrowKind <*> resolveType left <*> resolveType right
    TTuple flavor promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      syntaxResolution <- resolution sp (IdentifierTuple flavor (length items)) (typePromotionNamespace promotion) (Resolved EntitySyntax)
      pure (annotateTypeSyntax sp syntaxResolution (TTuple flavor promotion items'))
    TUnboxedSum items ->
      TUnboxedSum <$> mapM resolveType items
    TList promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      syntaxResolution <- resolution sp IdentifierList (typePromotionNamespace promotion) (Resolved EntitySyntax)
      pure (annotateTypeSyntax sp syntaxResolution (TList promotion items'))
    TParen inner ->
      TParen <$> resolveType inner
    TKindSig inner kind ->
      TKindSig <$> resolveType inner <*> resolveType kind
    TContext constraints inner ->
      TContext <$> mapM resolveType constraints <*> resolveType inner
    TSplice expr ->
      TSplice <$> withResetLocalSupply (resolveExpr expr)
    TWildcard -> pure ty
    TQuasiQuote {} -> TAnn <$> unhandledSyntax ResolutionNamespaceType "TQuasiQuote" <*> pure ty

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

annotateTypeSyntax :: Maybe SourceSpan -> Annotation -> Type -> Type
annotateTypeSyntax sp syntaxResolution =
  TAnn syntaxResolution . maybe id (TAnn . mkAnnotation) sp

bindTyVarBinders :: [TyVarBinder] -> ResolveM (Scope, [TyVarBinder])
bindTyVarBinders =
  foldM step (emptyScope, [])
  where
    step (boundScope, acc) binder = do
      binder' <- extendScope boundScope (traverseTyVarBinderKind binder)
      entity <- freshLocal
      let boundScope' = insertType (tyVarBinderName binder) entity boundScope
      pure (boundScope', acc <> [binder'])
    traverseTyVarBinderKind binder = do
      kind' <- traverse resolveType (tyVarBinderKind binder)
      pure binder {tyVarBinderKind = kind'}

allocateLocalDeclBinders :: [Decl] -> ResolveM (Map.Map Text Entity, Scope)
allocateLocalDeclBinders decls = do
  recordFields <- scopeRecordFields <$> currentScope
  foldM (step recordFields) (Map.empty, emptyScope) decls
  where
    step recordFields acc decl = foldM addBinder acc (declBinderCandidates recordFields decl)
    addBinder (targets, scope) (_, name) = do
      entity <- freshLocal
      let key = renderUnqualifiedName name
      pure (Map.insert key entity targets, insertTerm key entity scope)

-- | Collect all term binders introduced by a declaration (handles tuple patterns etc.)
declBinderCandidates :: Map.Map Text [Text] -> Decl -> [(Maybe SourceSpan, UnqualifiedName)]
declBinderCandidates recordFields decl =
  let (outerSp, innerDecl) = peelDeclSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = outerSp
               in [(spanStartNameSpan loc (renderUnqualifiedName name), name)]
            PatternBind _ pat _ ->
              let loc = peelPatternSpan pat <|> outerSp
               in collectPatVarBinders recordFields loc pat
        DeclTypeSig [name] _ ->
          [(spanStartNameSpan outerSp (renderUnqualifiedName name), name)]
        _ -> []

declBinderCandidate :: Decl -> Maybe (Maybe SourceSpan, UnqualifiedName)
declBinderCandidate decl =
  let (outerSp, innerDecl) = peelDeclSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = outerSp
               in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
            PatternBind _ pat _ ->
              case peelPatternAnn pat of
                PVar name ->
                  let loc =
                        peelPatternSpan pat <|> outerSp
                   in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
                _ -> Nothing
        DeclTypeSig [name] _ ->
          Just (spanStartNameSpan outerSp (renderUnqualifiedName name), name)
        _ -> Nothing

topLevelTermDefinition :: Scope -> TermDefinition
topLevelTermDefinition scope name =
  Just (lookupTerm (renderUnqualifiedName name) scope)

resolveTermDefinitionAt :: Maybe SourceSpan -> TermDefinition -> UnqualifiedName -> ResolveM UnqualifiedName
resolveTermDefinitionAt span' termDefinition name =
  case termDefinition name of
    Just resolved ->
      resolveUnqualifiedNameTo (spanStartNameSpan span' (renderUnqualifiedName name)) ResolutionNamespaceTerm resolved name
    Nothing -> pure name

resolveUnqualifiedNameTo :: Maybe SourceSpan -> ResolutionNamespace -> Resolution -> UnqualifiedName -> ResolveM UnqualifiedName
resolveUnqualifiedNameTo span' namespace resolved name =
  withResolution span' (IdentifierNamed (renderUnqualifiedName name)) namespace resolved $
    \ann -> name {unqualifiedNameAnns = ann : unqualifiedNameAnns name}

resolveNameTo :: Maybe SourceSpan -> ResolutionNamespace -> Resolution -> Name -> ResolveM Name
resolveNameTo span' namespace resolved name =
  withResolution span' (IdentifierNamed (nameText name)) namespace resolved $
    \ann -> name {nameAnns = ann : nameAnns name}

resolveTermUse :: Name -> ResolveM Name
resolveTermUse name = do
  sp <- currentSpan
  scope <- currentScope
  resolveNameTo sp ResolutionNamespaceTerm (resolveTermName scope name) name

-- | Resolve a term name that has its own place in the source, such as an
-- infix operator. The parser gives the name the span of its token. A name
-- without a span takes the start of the enclosing syntax.
resolveTermUseAtName :: Name -> ResolveM Name
resolveTermUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  let nameSpan = sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan sp (nameText name)
  resolveNameTo nameSpan ResolutionNamespaceTerm (resolveTermName scope name) name

resolveInfixExpr :: Expr -> ResolveM Expr
resolveInfixExpr expr = do
  operands <- traverse resolveExpr (flattenInfixExpr expr)
  resolveInfixChain EInfix operands

-- | Check fixities before annotation. Each operator gets one resolution.
-- An ambiguous chain keeps its left-nested tree and marks the selected operator.
resolveInfixChain :: (a -> Name -> a -> a) -> InfixChain Name a -> ResolveM a
resolveInfixChain build chain = do
  scope <- currentScope
  ambient <- currentSpan
  let ops = prepareInfix (resolveFixityName scope) chain
  case ambiguousInfixOp ops of
    Nothing -> rebuildInfix build <$> traverseOperators resolveOperator ops
    Just ambiguous ->
      buildLeftInfix build <$> traverseOperators (resolveAmbiguousOperator ambient ambiguous) ops
  where
    resolveOperator op = do
      name <- resolveTermUseAtName (resolvedInfixName op)
      pure op {resolvedInfixName = name}
    resolveAmbiguousOperator ambient ambiguous op
      | resolvedInfixIndex op == resolvedInfixIndex ambiguous =
          ambiguousFixityName ambient (resolvedInfixName op)
      | otherwise = resolveTermUseAtName (resolvedInfixName op)

ambiguousFixityName :: Maybe SourceSpan -> Name -> ResolveM Name
ambiguousFixityName ambient name = do
  ann <-
    resolution
      (sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan ambient (nameText name))
      (IdentifierNamed (nameText name))
      ResolutionNamespaceTerm
      (Unresolved "ambiguous fixity")
  pure name {nameAnns = ann : nameAnns name}

-- | Apply operator fixities to a type chain and keep each promotion.
resolveInfixType :: Type -> ResolveM Type
resolveInfixType ty = do
  operands <- traverse resolveType (flattenInfix split ty)
  scope <- currentScope
  ambient <- currentSpan
  let operators = prepareInfix (resolveFixityName scope . fst) operands
  case ambiguousInfixOp operators of
    Nothing -> rebuildInfix build <$> traverseOperators resolveOperator operators
    Just ambiguous -> buildLeftInfix build <$> traverseOperators (resolveAmbiguous ambient ambiguous) operators
  where
    split (TInfix left name promotion right) = Just (left, (name, promotion), right)
    split _ = Nothing
    build left (name, promotion) = TInfix left name promotion
    resolveOperator operator = do
      let (name, promotion) = resolvedInfixName operator
      resolved <- resolveTypeConstructorUse promotion name
      pure operator {resolvedInfixName = (resolved, promotion)}
    resolveAmbiguous ambient ambiguous operator
      | resolvedInfixIndex operator == resolvedInfixIndex ambiguous = do
          let (name, promotion) = resolvedInfixName operator
          annotation <-
            resolution
              (sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan ambient (nameText name))
              (IdentifierNamed (nameText name))
              (typePromotionNamespace promotion)
              (Unresolved "ambiguous fixity")
          pure (name {nameAnns = annotation : nameAnns name}, promotion)
      | otherwise = resolvedInfixName <$> resolveOperator operator

flattenInfixExpr :: Expr -> InfixChain Name Expr
flattenInfixExpr = flattenInfix split
  where
    split (EInfix left op right) = Just (left, op, right)
    split _ = Nothing

flattenInfixPattern :: Pattern -> InfixChain Name Pattern
flattenInfixPattern = flattenInfix split
  where
    split (PInfix left op right) = Just (left, op, right)
    split _ = Nothing

resolveTypeConstructorUse :: TypePromotion -> Name -> ResolveM Name
resolveTypeConstructorUse promotion name =
  case promotion of
    Unpromoted -> do
      sp <- currentSpan
      scope <- currentScope
      resolveNameTo sp ResolutionNamespaceType (resolveTypeName scope name) name
    Promoted -> do
      sp <- currentSpan
      scope <- currentScope
      resolveNameTo sp ResolutionNamespaceTerm (resolveTermName scope name) name

typePromotionNamespace :: TypePromotion -> ResolutionNamespace
typePromotionNamespace promotion =
  case promotion of
    Unpromoted -> ResolutionNamespaceType
    Promoted -> ResolutionNamespaceTerm

resolveTypeUseAtName :: Name -> ResolveM Name
resolveTypeUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  let nameSpan = sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan sp (nameText name)
  resolveNameTo nameSpan ResolutionNamespaceType (resolveTypeName scope name) name

resolveScopedTypeVariableUse :: UnqualifiedName -> ResolveM UnqualifiedName
resolveScopedTypeVariableUse name = do
  sp <- currentSpan
  scope <- currentScope
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
  -- A type variable the scope does not know is left alone: the binder that
  -- would give it a meaning may be implicit, so this is no error.
  case resolved of
    Unresolved _ -> pure name
    _ -> resolveUnqualifiedNameTo sp ResolutionNamespaceType resolved name

resolveDataConDefinitions :: Scope -> DataConDecl -> ResolveM DataConDecl
resolveDataConDefinitions scope =
  go Nothing
  where
    go ambient current =
      case current of
        DataConAnn ann inner -> DataConAnn ann <$> go (pushSpanFromAnn ambient ann) inner
        PrefixCon forallVars context name bangTypes ->
          (\name' -> PrefixCon forallVars context name' bangTypes) <$> resolveConstructor ambient name
        RecordCon forallVars context name fields ->
          (\name' -> RecordCon forallVars context name' fields) <$> resolveConstructor ambient name
        InfixCon forallVars context lhs name rhs ->
          (\name' -> InfixCon forallVars context lhs name' rhs) <$> resolveConstructor ambient name
        GadtCon forallVars context names body ->
          (\names' -> GadtCon forallVars context names' body) <$> mapM (resolveConstructor ambient) names
        TupleCon {} -> pure current
        UnboxedSumCon {} -> pure current
        ListCon {} -> do
          ann <- resolution ambient IdentifierList ResolutionNamespaceTerm (Resolved EntitySyntax)
          pure (DataConAnn ann current)

    resolveConstructor span' name =
      let rendered = renderUnqualifiedName name
       in resolveUnqualifiedNameTo
            (spanStartNameSpan span' rendered)
            ResolutionNamespaceTerm
            (lookupTerm rendered scope)
            name
