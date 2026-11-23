{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Resolve
  ( -- * Resolving a unit
    resolveUnit,
    ResolvedUnit (..),
    ResolvedModule (..),
    mkResolvedModule,
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
    fieldSelectorName,
    ResolutionNamespace (..),

    -- * Annotations
    Resolved (..),
    ResolutionResult (..),
    resolvedOf,
    RecKnotVariables (..),
    Identifier (..),
    displayIdentifier,
    resolutionOf,
    nameResolution,
    termResolution,
    binderResolution,
    binderEntity,
    nameEntity,
    nameOrigin,

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
    moduleExportsWithOrigins,
    moduleExportKeys,
    lookupModuleExport,
    filterModuleExports,
    collectModuleExports,
    collectModuleExportsWithDeps,
    moduleDeclarationExports,
    resolveExportList,
    ResolvedExport (..),
    exportedLocalNames,

    -- * Record field selectors
    recordSelectorNames,
    isFieldSelectorOf,

    -- * Builtins
    Builtins,
    builtins,

    -- * Names in the scope of a module
    moduleTopLevelName,
    resolveSpliceBodies,
    toParserExpr,
  )
where

import Aihc.Parser.Syntax
import Aihc.Resolve.Infix
import Aihc.Resolve.Monad
import Aihc.Resolve.Result
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Syntax qualified as R
import Aihc.Resolve.Syntax.FromParser
import Aihc.Resolve.Syntax.ToParser (toParserExpr)
import Aihc.Resolve.Syntax.Traverse (Walkable, collectResolutions)
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Data.Either (partitionEithers)
import Data.List (find, mapAccumL)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
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
--
-- The resolver does not run Template Haskell. A top-level splice makes
-- declarations that the resolver cannot see. Thus, a unit with a top-level
-- splice fails with one error for each splice, and the resolver does not
-- resolve its names. Each name that a splice must make would otherwise give
-- an "unbound" error that hides the cause.
resolveUnit :: Builtins -> ModuleExports -> [ModuleUnit] -> Either ResolveFailure ResolvedUnit
resolveUnit (Builtins builtinScope) exports packageModules
  | not (null spliceErrors) = Left (ResolveFailure spliceErrors spliceModules)
  | null errors = Right (ResolvedUnit modules)
  | otherwise = Left (ResolveFailure errors modules)
  where
    spliceChecks = map (topLevelSpliceErrors . moduleUnitAst) packageModules
    spliceErrors = concatMap fst spliceChecks
    spliceModules =
      zipWith
        (\unit (_, modu) -> mkResolvedModule (moduleUnitPackage unit) (moduleUnitExtensions unit) modu [])
        packageModules
        spliceChecks
    step currentNextLocal unit =
      resolveModule builtinScope (moduleUnitPackage unit) exports (moduleUnitExtensions unit) currentNextLocal (moduleUnitAst unit)
    (_, resolved) = mapAccumL step 0 packageModules
    errors = concatMap (\(moduleErrors, _, _) -> moduleErrors) resolved
    modules =
      zipWith
        (\unit (_, visibleTerms, modu) -> mkResolvedModule (moduleUnitPackage unit) (moduleUnitExtensions unit) modu visibleTerms)
        packageModules
        resolved

-- | The top-level entity of a name in the scope of a module: a declaration
-- of the module or a name that the module imports. A Template Haskell name
-- quote in the body of a splice uses it, because the compiler runs the
-- splice before it resolves the module. The result is 'Nothing' for a name
-- that the scope does not have. The function makes the scope of the module
-- one time, for all the names.
moduleTopLevelName :: Builtins -> ModuleExports -> ModuleUnit -> ResolutionNamespace -> Name -> Maybe GlobalName
moduleTopLevelName (Builtins builtinScope) exports unit =
  \namespace name ->
    case resolve namespace scope name of
      Found (EntityGlobal global) -> Just global
      _ -> Nothing
  where
    package = moduleUnitPackage unit
    modu = moduleUnitAst unit
    scope = moduleScope builtinScope package exports (moduleUnitExtensions unit) modu
    resolve namespace =
      case namespace of
        ResolutionNamespaceType -> resolveTypeName
        _ -> resolveTermName

-- | Resolve the body of each splice of a module as an expression in the
-- scope of the module. A quote in a splice body names the declarations of
-- the module, also when the module does not export them. The compiler runs
-- the splices before it resolves the module, so it resolves the bodies
-- here, and changes their quotes with these resolutions. A name that does
-- not resolve has no resolution: the errors are not reported here, because
-- the splice module reports them.
resolveSpliceBodies :: Builtins -> ModuleExports -> ModuleUnit -> [Expr] -> [R.Expr]
resolveSpliceBodies (Builtins builtinScope) exports unit bodies =
  let (_, _, resolved) =
        runResolveM
          scope
          (moduleInfo builtinScope (moduleUnitExtensions unit) modu)
          0
          (mapM (withResetLocalSupply . resolveExpr) bodies)
   in resolved
  where
    package = moduleUnitPackage unit
    modu = moduleUnitAst unit
    scope = moduleScope builtinScope package exports (moduleUnitExtensions unit) modu

resolveModule :: Scope -> Package -> ModuleExports -> [Extension] -> Int -> Module -> (Int, ([ResolveError], [GlobalName], R.Module))
resolveModule builtinScope package exports extensions nextLocal modu =
  let (imports', importErrors) = resolveModuleImports package exports (moduleImports modu)
      scope = moduleScope builtinScope package exports extensions modu
      (nextLocal', declErrors, decls') =
        runResolveM
          scope
          (moduleInfo builtinScope extensions modu)
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
          resolvedModuleTree modu imports' decls'
        )
      )

-- | The resolved tree of a module, from the resolved imports and the
-- resolved declarations. The module head stays unresolved.
resolvedModuleTree :: Module -> [R.ImportDecl] -> [R.Decl] -> R.Module
resolvedModuleTree modu imports decls =
  R.Module
    { R.moduleAnns = moduleAnns modu,
      R.moduleHead = fromParserModuleHead <$> moduleHead modu,
      R.moduleLanguagePragmas = moduleLanguagePragmas modu,
      R.moduleImports = imports,
      R.moduleDecls = decls
    }

-- | The errors for the top-level Template Haskell splices of a module, and
-- the unresolved module with the error of each splice.
topLevelSpliceErrors :: Module -> ([ResolveError], R.Module)
topLevelSpliceErrors modu =
  let (errors, decls') = unzip (map checkDecl (moduleDecls modu))
   in (concat errors, resolvedModuleTree modu (map fromParserImportDecl (moduleImports modu)) decls')
  where
    checkDecl decl =
      case peelDeclSpan decl of
        (span', DeclSplice expr) ->
          let resolveError =
                ResolveError
                  { resolveErrorSpan = span',
                    resolveErrorName = spliceHeadName expr,
                    resolveErrorNamespace = ResolutionNamespaceTerm,
                    resolveErrorMessage = topLevelSpliceMessage
                  }
           in ([resolveError], R.DeclError resolveError (fromParserDecl decl))
        _ -> ([], fromParserDecl decl)

-- | The message of a top-level splice error.
topLevelSpliceMessage :: String
topLevelSpliceMessage = "top-level Template Haskell splices are not supported"

-- | The name of the function that a splice applies, such as @makeLenses@ in
-- @makeLenses ''T@. A splice with no such function gives the text "splice".
spliceHeadName :: Expr -> Text
spliceHeadName expr =
  case expr of
    EAnn _ inner -> spliceHeadName inner
    EParen inner -> spliceHeadName inner
    ETHSplice inner -> spliceHeadName inner
    EApp fun _ -> spliceHeadName fun
    EVar name -> nameText name
    _ -> T.pack "splice"

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
resolveModuleImports :: Package -> ModuleExports -> [ImportDecl] -> ([R.ImportDecl], [ResolveError])
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
       in ((fromParserImportDecl importDecl) {R.importDeclError = Just resolveError}, [resolveError])

missingModuleImport :: String -> ImportDecl -> ResolveError
missingModuleImport message importDecl =
  resolveErrorAt
    (importModuleNameSpan importDecl)
    (IdentifierNamed (importDeclModule importDecl))
    ResolutionNamespaceModule
    message

annotateMissingImportItems :: Scope -> ImportDecl -> (R.ImportDecl, [ResolveError])
annotateMissingImportItems originScope importDecl =
  case importDeclSpec importDecl of
    Just importSpec@ImportSpec {importSpecHiding = False, importSpecItems} ->
      let annotated = map annotateItem importSpecItems
       in ( resolved {R.importDeclSpec = Just (R.ImportSpec (importSpecAnns importSpec) False (map fst annotated))},
            mapMaybe snd annotated
          )
    _ -> (resolved, [])
  where
    resolved = fromParserImportDecl importDecl
    annotateItem item =
      case missingImportItemAnnotation originScope item of
        Nothing -> (fromParserImportItem item, Nothing)
        Just annotation -> (annotateImportItemError annotation item, Just annotation)

annotateImportItemError :: ResolveError -> ImportItem -> R.ImportItem
annotateImportItemError resolveError item =
  -- Keep the diagnostic span as the carrier span for annotated-source overlays.
  R.ImportItemError resolveError (maybe id (R.ImportAnn . mkAnnotation) (resolveErrorSpan resolveError) (fromParserImportItem item))

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

resolveBindingGroup :: TermDefinition -> Map.Map Text Scope -> [Decl] -> ResolveM [R.Decl]
resolveBindingGroup _ _ [] = pure []
resolveBindingGroup termDefinition signatureScopes (decl : rest) = do
  (signatureScopes', decl') <- resolveBindingDecl termDefinition signatureScopes decl
  decls' <- resolveBindingGroup termDefinition signatureScopes' rest
  pure (decl' : decls')

resolveBindingDecl :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, R.Decl)
resolveBindingDecl termDefinition signatureScopes decl = do
  scope <- currentScope
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
  withScope scoped (resolveDeclWithSignatureScope termDefinition signatureScopes decl)

resolveDeclWithSignatureScope :: TermDefinition -> Map.Map Text Scope -> Decl -> ResolveM (Map.Map Text Scope, R.Decl)
resolveDeclWithSignatureScope termDefinition signatureScopes decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann $ do
        (signatureScopes', inner') <- resolveDeclWithSignatureScope termDefinition signatureScopes inner
        pure (signatureScopes', R.DeclAnn ann inner')
    DeclTypeSig names ty -> do
      sp <- currentSpan
      (binderScope, ty') <- resolveTypeSignature ty
      names' <- mapM (resolveTermDefinitionAt sp termDefinition) names
      let signatureScopes' =
            List.foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
      pure (signatureScopes', R.DeclTypeSig names' ty')
    _ -> do
      decl' <- resolveDecl termDefinition decl
      let signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
      pure (signatureScopes', decl')

resolveDecl :: TermDefinition -> Decl -> ResolveM R.Decl
resolveDecl termDefinition (DeclAnn ann inner) =
  withPushedSpan ann (resolveDecl termDefinition inner)
resolveDecl termDefinition decl =
  resolveDeclCore termDefinition decl

-- | The error for syntax the resolver has no case for. The name is the
-- constructor of the form it met.
unhandledSyntax :: ResolutionNamespace -> Text -> ResolveM ResolveError
unhandledSyntax namespace constructor = do
  sp <- currentSpan
  resolveFailure sp (IdentifierNamed constructor) namespace "unhandled syntax"

resolveDeclCore :: TermDefinition -> Decl -> ResolveM R.Decl
resolveDeclCore termDefinition decl =
  case decl of
    DeclAnn ann inner ->
      withPushedSpan ann (resolveDeclCore termDefinition inner)
    DeclValue valueDecl ->
      R.DeclValue <$> resolveValueDecl termDefinition valueDecl
    DeclImplicitParam name expr mDecls -> do
      -- An implicit-parameter binding does not bind a term name.
      -- The type checker connects each use to its binding.
      (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
      expr' <- extendScope localScope (resolveExpr expr)
      mDecls' <- traverse (extendScope localScope . resolveBoundDecls binderAnnotations Map.empty) mDecls
      pure (R.DeclImplicitParam name expr' mDecls')
    DeclTypeSig names ty -> do
      ty' <- resolveType ty
      pure (R.DeclTypeSig (map fromParserUnqualifiedName names) ty')
    DeclStandaloneKindSig name kind -> do
      scope <- currentScope
      sp <- currentSpan
      let rendered = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      R.DeclStandaloneKindSig name' <$> resolveType kind
    DeclTypeData dataDecl ->
      R.DeclTypeData <$> resolveDataDecl "type data " dataDecl
    DeclData dataDecl ->
      R.DeclData <$> resolveDataDecl "data " dataDecl
    DeclTypeSyn typeSynDecl ->
      R.DeclTypeSyn <$> resolveTypeSynDecl typeSynDecl
    DeclSplice expr -> R.DeclSplice <$> resolveExpr expr
    DeclNewtype newtypeDecl ->
      R.DeclNewtype <$> resolveNewtypeDecl newtypeDecl
    DeclClass classDecl ->
      R.DeclClass <$> resolveClassDecl classDecl
    DeclDefault tys ->
      R.DeclDefault <$> mapM resolveType tys
    DeclFixity {} -> pure (fromParserDecl decl)
    DeclForeign foreignDecl ->
      R.DeclForeign <$> resolveForeignDecl termDefinition foreignDecl
    DeclRoleAnnotation roleAnnotation -> do
      scope <- currentScope
      sp <- currentSpan
      let name = roleAnnotationName roleAnnotation
          rendered = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceType (lookupType rendered scope) name
      pure (R.DeclRoleAnnotation (R.RoleAnnotation name' (roleAnnotationRoles roleAnnotation)))
    DeclPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure (fromParserPragmaDecl pragma)
      | otherwise -> R.DeclError <$> unhandledSyntax ResolutionNamespaceTerm "DeclPragma" <*> pure (fromParserPragmaDecl pragma)
    DeclRules rules ->
      R.DeclRules <$> mapM resolveRuleDecl rules
    DeclPatSyn patSyn -> do
      sp <- currentSpan
      (patSyn', unboundArgs) <- resolvePatSynDecl termDefinition patSyn
      unboundErrors <- mapM (unboundPatSynArgError sp) unboundArgs
      pure (List.foldl' (flip R.DeclError) (R.DeclPatSyn patSyn') unboundErrors)
    DeclPatSynSig names ty -> do
      sp <- currentSpan
      (_, ty') <- resolveTypeSignature ty
      names' <- mapM (resolveTermDefinitionAt (declKeywordNameSpan "pattern " sp "") termDefinition) names
      pure (R.DeclPatSynSig names' ty')
    DeclInstance instanceDecl ->
      R.DeclInstance <$> resolveInstanceDecl instanceDecl
    DeclStandaloneDeriving derivingDecl ->
      R.DeclStandaloneDeriving <$> resolveStandaloneDerivingDecl derivingDecl
    DeclTypeFamilyDecl familyDecl ->
      R.DeclTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    DeclDataFamilyDecl dataFamilyDecl ->
      R.DeclDataFamilyDecl <$> resolveDataFamilyDecl "data family " dataFamilyDecl
    DeclTypeFamilyInst familyInst ->
      R.DeclTypeFamilyInst <$> resolveTypeFamilyInst familyInst
    DeclDataFamilyInst dataFamilyInst ->
      R.DeclDataFamilyInst <$> resolveDataFamilyInst dataFamilyInst

-- | Resolve one rewrite rule of a @RULES@ pragma.
--
-- The type variables of a leading @forall@ scope over the types of the
-- pattern variables and over both sides, and the pattern variables scope
-- over both sides: inside a @RULES@ pragma, ScopedTypeVariables is always
-- on. A type variable that no @forall@ binds is left for the type checker
-- to bind, as in a signature. The left-hand side must be a top-level
-- variable applied to arguments; anything else is an error on the rule.
resolveRuleDecl :: RuleDecl -> ResolveM R.RuleDecl
resolveRuleDecl rule =
  withEffectiveSpan (sourceSpanFromAnns (ruleAnns rule)) $
    withResetLocalSupply $ do
      (typeScope, typeBinders') <- bindTyVarBinders (ruleTypeBinders rule)
      extendScope typeScope $ do
        (binderScope, binders') <- bindRuleBinders (ruleBinders rule)
        extendScope binderScope $ do
          lhs' <- resolveExpr (ruleLhs rule)
          rhs' <- resolveExpr (ruleRhs rule)
          ruleError' <-
            if ruleLhsHeadIsTopLevel lhs'
              then pure Nothing
              else do
                sp <- currentSpan
                Just
                  <$> resolveFailure
                    sp
                    (IdentifierNamed (ruleName rule))
                    ResolutionNamespaceTerm
                    "the left-hand side of a rule must be a top-level variable applied to arguments"
          pure
            R.RuleDecl
              { R.ruleAnns = ruleAnns rule,
                R.ruleError = ruleError',
                R.ruleName = ruleName rule,
                R.ruleActivation = ruleActivation rule,
                R.ruleTypeBinders = typeBinders',
                R.ruleBinders = binders',
                R.ruleLhs = lhs',
                R.ruleRhs = rhs'
              }

-- | Bind the pattern variables of a rule, each with a fresh local name.
-- The type of a binder is resolved in the scope of the rule's type
-- variables, which the caller has put in scope.
bindRuleBinders :: [RuleBinder] -> ResolveM (Scope, [R.RuleBinder])
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
        name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Found entity) name
        pure (insertTerm key entity bound, acc <> [R.RuleBinder (ruleBinderAnns binder) name' ty'])

-- | Whether the head of a resolved rule left-hand side is a top-level
-- variable: the function of an application chain, or the operator of an
-- infix expression.
ruleLhsHeadIsTopLevel :: R.Expr -> Bool
ruleLhsHeadIsTopLevel expr =
  case expr of
    R.EAnn _ inner -> ruleLhsHeadIsTopLevel inner
    R.EResolved _ inner -> ruleLhsHeadIsTopLevel inner
    R.EParen inner -> ruleLhsHeadIsTopLevel inner
    R.EApp fun _ -> ruleLhsHeadIsTopLevel fun
    R.ETypeApp fun _ -> ruleLhsHeadIsTopLevel fun
    R.EInfix _ op _ -> isTopLevelValue op
    R.EVar name -> isTopLevelValue name
    _ -> False
  where
    isTopLevelValue name =
      case resolvedNameTarget name of
        Just EntityGlobal {} -> R.nameType name == NameVarId || R.nameType name == NameVarSym
        _ -> False

-- | The entity of a resolved name occurrence, if it resolved.
resolvedNameTarget :: R.Name -> Maybe Entity
resolvedNameTarget name = resolutionTarget <$> (resolvedOf =<< R.nameResolution name)

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
    -- A cost-centre pragma on a declaration is a profiler hint.
    PragmaSCC _ -> True
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

resolveValueDecl :: TermDefinition -> ValueDecl -> ResolveM R.ValueDecl
resolveValueDecl termDefinition valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      sp <- currentSpan
      name' <- resolveTermDefinitionAt sp termDefinition name
      R.FunctionBind name' <$> mapM resolveMatch matches
    PatternBind multTag pat rhs ->
      R.PatternBind (fromParserMultiplicityTag multTag) <$> resolvePatternDefinition termDefinition pat <*> resolveRhs rhs

-- | Resolve a pattern synonym declaration. The right-hand side pattern
-- binds the argument variables. The result gives the arguments that the
-- pattern does not bind.
resolvePatSynDecl :: TermDefinition -> PatSynDecl -> ResolveM (R.PatSynDecl, [Text])
resolvePatSynDecl termDefinition patSyn = do
  sp <- currentSpan
  let name = patSynDeclName patSyn
      nameSpan = patSynNameSpan sp patSyn
  name' <-
    case termDefinition name of
      Just resolved -> resolveUnqualifiedNameTo nameSpan ResolutionNamespaceTerm resolved name
      Nothing -> pure (fromParserUnqualifiedName name)
  (patScope, pat') <- bindPattern (patSynDeclPat patSyn)
  dir' <-
    case patSynDeclDir patSyn of
      PatSynExplicitBidirectional matches -> R.PatSynExplicitBidirectional <$> mapM resolveMatch matches
      PatSynUnidirectional -> pure R.PatSynUnidirectional
      PatSynBidirectional -> pure R.PatSynBidirectional
  let unboundArgs = [arg | arg <- patSynArgNames (patSynDeclArgs patSyn), not (Map.member arg (scopeTerms patScope))]
  pure (R.PatSynDecl name' (patSynDeclArgs patSyn) pat' dir', unboundArgs)

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

unboundPatSynArgError :: Maybe SourceSpan -> Text -> ResolveM ResolveError
unboundPatSynArgError sp arg =
  resolveFailure sp (IdentifierNamed arg) ResolutionNamespaceTerm "pattern synonym argument is not bound by the pattern"

resolveForeignDecl :: TermDefinition -> ForeignDecl -> ResolveM R.ForeignDecl
resolveForeignDecl termDefinition foreignDecl = do
  sp <- currentSpan
  name' <- resolveTermDefinitionAt sp termDefinition (foreignName foreignDecl)
  ty' <- resolveType (foreignType foreignDecl)
  pure
    R.ForeignDecl
      { R.foreignDirection = foreignDirection foreignDecl,
        R.foreignCallConv = foreignCallConv foreignDecl,
        R.foreignSafety = foreignSafety foreignDecl,
        R.foreignEntity = foreignEntity foreignDecl,
        R.foreignName = name',
        R.foreignType = ty'
      }

resolveClassDecl :: ClassDecl -> ResolveM R.ClassDecl
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
    R.ClassDecl
      { R.classDeclContext = context',
        R.classDeclHead = head',
        R.classDeclFundeps = classDeclFundeps classDecl,
        R.classDeclItems = items'
      }

resolveClassDeclItem :: ClassDeclItem -> ResolveM R.ClassDeclItem
resolveClassDeclItem classDeclItem =
  case classDeclItem of
    ClassItemAnn ann inner -> R.ClassItemAnn ann <$> withPushedSpan ann (resolveClassDeclItem inner)
    ClassItemTypeSig names ty -> do
      scope <- currentScope
      sp <- currentSpan
      names' <- mapM (resolveTermDefinitionAt sp (topLevelTermDefinition scope)) names
      R.ClassItemTypeSig names' <$> resolveType ty
    ClassItemDefaultSig name ty -> R.ClassItemDefaultSig (fromParserUnqualifiedName name) <$> resolveType ty
    ClassItemDefault valueDecl -> do
      scope <- currentScope
      R.ClassItemDefault <$> withResetLocalSupply (resolveValueDecl (topLevelTermDefinition scope) valueDecl)
    ClassItemFixity {} -> pure (fromParserClassDeclItem classDeclItem)
    ClassItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure (fromParserClassDeclItem classDeclItem)
      | otherwise -> R.ClassItemError <$> unhandledSyntax ResolutionNamespaceTerm "ClassItemPragma" <*> pure (fromParserClassDeclItem classDeclItem)
    ClassItemTypeFamilyDecl familyDecl -> R.ClassItemTypeFamilyDecl <$> resolveTypeFamilyDecl familyDecl
    ClassItemDataFamilyDecl familyDecl -> R.ClassItemDataFamilyDecl <$> resolveDataFamilyDecl "data " familyDecl
    ClassItemDefaultTypeInst familyInst -> R.ClassItemDefaultTypeInst <$> resolveTypeFamilyInst familyInst

resolveInstanceDecl :: InstanceDecl -> ResolveM R.InstanceDecl
resolveInstanceDecl instanceDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (instanceDeclForall instanceDecl)
  (context', head', items') <-
    extendScope forallScope $ do
      context' <- mapM resolveType (instanceDeclContext instanceDecl)
      head' <- resolveType (instanceDeclHead instanceDecl)
      items' <- mapM (resolveInstanceDeclItem (instanceHeadClass head')) (instanceDeclItems instanceDecl)
      pure (context', head', items')
  pure
    R.InstanceDecl
      { R.instanceDeclPragmas = instanceDeclPragmas instanceDecl,
        R.instanceDeclWarning = instanceDeclWarning instanceDecl,
        R.instanceDeclForall = forallBinders',
        R.instanceDeclContext = context',
        R.instanceDeclHead = head',
        R.instanceDeclItems = items'
      }

-- | The class of a resolved instance head, with the name as written.
instanceHeadClass :: R.Type -> Maybe (Text, Entity)
instanceHeadClass ty =
  case ty of
    R.TAnn _ inner -> instanceHeadClass inner
    R.TResolved _ inner -> instanceHeadClass inner
    R.TParen inner -> instanceHeadClass inner
    R.TKindSig inner _ -> instanceHeadClass inner
    R.TApp fun _ -> instanceHeadClass fun
    R.TCon name Unpromoted -> (,) (R.nameText name) <$> resolvedNameTarget name
    _ -> Nothing

-- | A method binding or a method signature in an instance names a method
-- of the class. The lookup goes through the methods of the class and not
-- through the term scope, as GHC does. Thus the method name does not have
-- to be in scope. For example, the import list can name only the class,
-- or the class can be in scope only under a qualifier. A binder that is
-- not a method of the class is an error. If the class of the instance
-- head is not known, the term scope resolves the binder.
--
-- The result of a Template Haskell splice can name the class with its
-- package and its module, and the class can be out of scope. Then the
-- binder names the term of that name in the module of the class, and the
-- type checker finds a binder that is not a method of the class.
instanceMethodDefinition :: Maybe (Text, Entity) -> Scope -> TermDefinition
instanceMethodDefinition headClass scope name =
  case headClass of
    Just (className, resolvedClass@(EntityGlobal classGlobal))
      | methodLists@(_ : _) <- classMethodLists className resolvedClass ->
          Just
            ( if any (rendered `elem`) methodLists
                then Found (classMethod classGlobal)
                else NotFound ("not a method of the class " <> T.unpack className)
            )
      | not (classInScope className resolvedClass) -> Just (Found (classMethod classGlobal))
    _ -> Just (lookupTerm rendered scope)
  where
    rendered = renderUnqualifiedName name
    -- A class and its methods have the same defining module, so the
    -- resolved method name comes from the resolved class name.
    classMethod classGlobal = globalTerm (globalNamePackage classGlobal) (globalNameModule classGlobal) (unqualifiedNameText name)
    classInScope className resolvedClass =
      any (\candidate -> lookupType className candidate == Found resolvedClass) (scope : Map.elems (scopeQualifiedModules scope))
    classMethodLists className resolvedClass =
      [ methods
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == Found resolvedClass,
        Just methods <- [Map.lookup className (scopeMethods candidate)]
      ]

-- | The scope that resolves the family name of an associated type instance
-- through the class of the instance head, like an instance method. The
-- lookup goes through the associated types of the class and not through
-- the type scope, as GHC does. Thus the family name can be out of scope,
-- for example when only the class is in scope through a qualified import.
-- The family name can also be ambiguous in the type scope, for example
-- when an import brings in a different type with the same name. If the
-- class of the instance head is not known, the type scope resolves the
-- family name.
associatedTypeInstanceScope :: Maybe (Text, Entity) -> Scope -> Type -> Scope
associatedTypeInstanceScope headClass scope lhs =
  case (headClass, typeHeadConstructorName lhs) of
    (Just (className, resolvedClass@(EntityGlobal classGlobal)), Just familyName)
      | any (familyName `elem`) (associatedTypeLists className resolvedClass) ->
          -- A class and its associated types have the same defining
          -- module, so the resolved family name comes from the resolved
          -- class name.
          let family =
                EntityGlobal
                  GlobalName
                    { globalNameText = familyName,
                      globalNamePackage = globalNamePackage classGlobal,
                      globalNameModule = globalNameModule classGlobal,
                      globalNameNamespace = ResolutionNamespaceType
                    }
           in emptyScope {scopeTypes = Map.singleton familyName family}
    _ -> emptyScope
  where
    associatedTypeLists className resolvedClass =
      [ families
      | candidate <- scope : Map.elems (scopeQualifiedModules scope),
        lookupType className candidate == Found resolvedClass,
        Just families <- [Map.lookup className (scopeAssociatedTypes candidate)]
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

resolveInstanceDeclItem :: Maybe (Text, Entity) -> InstanceDeclItem -> ResolveM R.InstanceDeclItem
resolveInstanceDeclItem headClass instanceDeclItem =
  case instanceDeclItem of
    InstanceItemAnn ann inner -> R.InstanceItemAnn ann <$> withPushedSpan ann (resolveInstanceDeclItem headClass inner)
    InstanceItemBind valueDecl -> do
      scope <- currentScope
      R.InstanceItemBind <$> withResetLocalSupply (resolveValueDecl (instanceMethodDefinition headClass scope) valueDecl)
    InstanceItemTypeSig names ty -> do
      scope <- currentScope
      sp <- currentSpan
      names' <- mapM (resolveTermDefinitionAt sp (instanceMethodDefinition headClass scope)) names
      R.InstanceItemTypeSig names' <$> resolveType ty
    InstanceItemFixity {} -> pure (fromParserInstanceDeclItem instanceDeclItem)
    InstanceItemTypeFamilyInst familyInst -> do
      scope <- currentScope
      let familyScope = associatedTypeInstanceScope headClass scope (typeFamilyInstLhs familyInst)
      R.InstanceItemTypeFamilyInst <$> resolveTypeFamilyInstWithLhsScope familyScope familyInst
    InstanceItemDataFamilyInst familyInst -> do
      scope <- currentScope
      let familyScope = associatedTypeInstanceScope headClass scope (dataFamilyInstHead familyInst)
      R.InstanceItemDataFamilyInst <$> extendScope familyScope (resolveDataFamilyInst familyInst)
    InstanceItemPragma pragma
      | ignoredPragma (pragmaType pragma) -> pure (fromParserInstanceDeclItem instanceDeclItem)
      | otherwise -> R.InstanceItemError <$> unhandledSyntax ResolutionNamespaceTerm "InstanceItemPragma" <*> pure (fromParserInstanceDeclItem instanceDeclItem)

resolveStandaloneDerivingDecl :: StandaloneDerivingDecl -> ResolveM R.StandaloneDerivingDecl
resolveStandaloneDerivingDecl derivingDecl = do
  (forallScope, forallBinders') <- bindTyVarBinders (standaloneDerivingForall derivingDecl)
  (strategy', context', head') <-
    extendScope forallScope $
      (,,)
        <$> traverse resolveDerivingStrategy (standaloneDerivingStrategy derivingDecl)
        <*> mapM resolveType (standaloneDerivingContext derivingDecl)
        <*> resolveType (standaloneDerivingHead derivingDecl)
  pure
    R.StandaloneDerivingDecl
      { R.standaloneDerivingStrategy = strategy',
        R.standaloneDerivingPragmas = standaloneDerivingPragmas derivingDecl,
        R.standaloneDerivingWarning = standaloneDerivingWarning derivingDecl,
        R.standaloneDerivingForall = forallBinders',
        R.standaloneDerivingContext = context',
        R.standaloneDerivingHead = head'
      }

resolveDerivingClause :: DerivingClause -> ResolveM R.DerivingClause
resolveDerivingClause clause = do
  strategy' <- traverse resolveDerivingStrategy (derivingStrategy clause)
  classes' <-
    case derivingClasses clause of
      Left name -> Left <$> resolveTypeUseAtName name
      Right tys -> Right <$> mapM resolveType tys
  pure (R.DerivingClause strategy' classes')

resolveDerivingStrategy :: DerivingStrategy -> ResolveM R.DerivingStrategy
resolveDerivingStrategy strategy =
  case strategy of
    DerivingVia ty -> R.DerivingVia <$> resolveType ty
    DerivingStock -> pure R.DerivingStock
    DerivingNewtype -> pure R.DerivingNewtype
    DerivingAnyclass -> pure R.DerivingAnyclass

resolveMatch :: Match -> ResolveM R.Match
resolveMatch match =
  withEffectiveSpan (sourceSpanFromAnns (matchAnns match)) $ do
    (patScope, pats') <- bindPatterns (matchPats match)
    rhsHere <- (rhsSpan (matchRhs match) <|>) <$> currentSpan
    rhs' <- extendScope patScope (withAmbientSpan rhsHere (resolveRhs (matchRhs match)))
    pure (R.Match (matchAnns match) (matchHeadForm match) pats' rhs')

resolveRhs :: Rhs Expr -> ResolveM (R.Rhs R.Expr)
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
        pure (R.UnguardedRhs anns expr' mDecls')
    GuardedRhss anns guardedRhss mDecls ->
      withEffectiveSpan (sourceSpanFromAnns anns) $ do
        -- Pre-allocate where-clause binders so guards can reference them.
        (binderAnnotations, localScope) <- allocateLocalDeclBinders (fromMaybe [] mDecls)
        guardedRhss' <- extendScope localScope (mapM resolveGuardedRhs guardedRhss)
        mDecls' <- case mDecls of
          Nothing -> pure Nothing
          Just decls -> Just <$> extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (R.GuardedRhss anns guardedRhss' mDecls')

resolveGuardedRhs :: GuardedRhs Expr -> ResolveM (R.GuardedRhs R.Expr)
resolveGuardedRhs guardedRhs =
  withEffectiveSpan (sourceSpanFromAnns (guardedRhsAnns guardedRhs)) $ do
    (scope', guards') <- resolveGuardQualifiers (guardedRhsGuards guardedRhs)
    body' <- withScope scope' (resolveExpr (guardedRhsBody guardedRhs))
    pure (R.GuardedRhs (guardedRhsAnns guardedRhs) guards' body')

resolveGuardQualifiers :: [GuardQualifier] -> ResolveM (Scope, [R.GuardQualifier])
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

resolveGuardQualifier :: GuardQualifier -> ResolveM (Scope, R.GuardQualifier)
resolveGuardQualifier qualifier =
  withEffectiveSpan (peelGuardQualifierSpan qualifier) $ do
    scope <- currentScope
    let qualifierSpan = peelGuardQualifierSpan qualifier
        wrap = maybe id (R.GuardAnn . mkAnnotation) qualifierSpan
    case peelGuardQualifierAnn qualifier of
      GuardExpr expr -> do
        expr' <- resolveExpr expr
        pure (scope, wrap (R.GuardExpr expr'))
      GuardPat pat expr -> do
        expr' <- resolveExpr expr
        (patScope, pat') <- bindPattern pat
        pure (unionScope patScope scope, wrap (R.GuardPat pat' expr'))
      GuardLet decls -> do
        (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
        decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
        pure (unionScope localScope scope, wrap (R.GuardLet decls'))
      GuardAnn _ _ -> pure (scope, fromParserGuardQualifier qualifier)

resolveExpr :: Expr -> ResolveM R.Expr
resolveExpr expr =
  case expr of
    EAnn ann inner ->
      R.EAnn ann <$> withPushedSpan ann (resolveExpr inner)
    EVar name -> do
      inQuote <- inExpressionQuote
      scope <- currentScope
      -- GHC accepts a variable or a constructor that is not in scope in
      -- an expression quote. The quote gives an unbound variable, and the
      -- code that gets the syntax tree finds its binder.
      case termUseResolution scope name of
        NotFound _
          | inQuote,
            isNothing (nameQualifier name) ->
              pure (fromParserExpr expr)
        _ -> R.EVar <$> resolveTermUse name
    -- An implicit parameter has no lexical binder. The type checker
    -- connects the use to a binding through constraint solving.
    EImplicitParam _ -> pure (fromParserExpr expr)
    ETypeSyntax form ty -> R.ETypeSyntax form <$> resolveType ty
    EInt _ TInteger _ -> resolveIntegerLiteral (fromParserExpr expr)
    EInt _ numericType _ -> resolvePrimitiveLiteralType numericType (fromParserExpr expr)
    EFloat value TFractional text -> do
      integral <- numDecimalsIntegralValue value
      case integral of
        Just integer -> resolveIntegerLiteral (R.EInt integer TInteger text)
        Nothing -> resolveFractionalLiteral (fromParserExpr expr)
    EFloat _ floatType _ ->
      maybe (resolveFractionalLiteral (fromParserExpr expr)) (`resolvePrimitiveLiteralTypeName` fromParserExpr expr) (primitiveFloatTypeName floatType)
    EChar {} -> pure (fromParserExpr expr)
    ECharHash {} -> resolvePrimitiveLiteralTypeName "Char#" (fromParserExpr expr)
    EString {} -> resolveStringLiteral (fromParserExpr expr)
    EStringHash {} -> resolvePrimitiveLiteralTypeName "Addr#" (fromParserExpr expr)
    EOverloadedLabel {} -> pure (fromParserExpr expr)
    EIf cond trueBranch falseBranch -> do
      resolved <- R.EIf <$> resolveExpr cond <*> resolveExpr trueBranch <*> resolveExpr falseBranch
      annotateRebindableIf resolved
    EMultiWayIf guardedRhss ->
      R.EMultiWayIf <$> mapM resolveGuardedRhs guardedRhss
    ELambdaPats pats body -> do
      (patScope, pats') <- bindPatterns pats
      body' <- extendScope patScope (resolveExpr body)
      pure (R.ELambdaPats pats' body')
    ELambdaCase alts ->
      R.ELambdaCase <$> mapM resolveCaseAlt alts
    ELambdaCases alts ->
      R.ELambdaCases <$> mapM resolveLambdaCaseAlt alts
    EInfix {} ->
      resolveInfixExpr expr
    -- The parser only builds this for the view-pattern arrow, so it reaches
    -- the resolver only when a view pattern appears where a pattern cannot.
    EViewPat lhs rhs ->
      R.EViewPat <$> resolveExpr lhs <*> resolveExpr rhs
    ENegate inner ->
      annotateNegate . R.ENegate =<< resolveExpr inner
    ESectionL inner op ->
      R.ESectionL <$> resolveExpr inner <*> resolveTermUseAtName op
    ESectionR op inner ->
      R.ESectionR <$> resolveTermUseAtName op <*> resolveExpr inner
    ELetDecls decls body -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      body' <- extendScope localScope (resolveExpr body)
      pure (R.ELetDecls decls' body')
    ECase scrutinee alts ->
      R.ECase <$> resolveExpr scrutinee <*> mapM resolveCaseAlt alts
    EArithSeq arithSeq -> do
      sequence' <- R.EArithSeq <$> resolveArithSeq arithSeq
      info <- currentModuleInfo
      -- Under OverloadedLists, GHC applies fromList to an arithmetic
      -- sequence. The sequence does not have a static length, thus it does
      -- not use fromListN.
      if OverloadedLists `elem` moduleInfoExtensions info
        then annotateSyntaxTerm "fromList" sequence'
        else pure sequence'
    ERecordCon name fields wildcard -> do
      name' <- resolveTermUse name
      fields' <- resolveRecordFields fields
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (R.nameAnns name') <|> ambient
      wildcardFields <- resolveRecordConWildcardFields sp name fields wildcard
      pure (R.ERecordCon name' (fields' <> wildcardFields) False)
    ERecordUpd record fields ->
      R.ERecordUpd <$> resolveExpr record <*> resolveRecordUpdateFields fields
    -- Record dot syntax applies the getField syntax term, as GHC does.
    -- The expression e.x is getField at the label "x", applied to e.
    -- A projection such as (.x.y) applies getField for each field in turn.
    EGetField record name -> do
      record' <- resolveExpr record
      annotateSyntaxTerm "getField" (R.EGetField record' (fromParserName name))
    EGetFieldProjection {} -> annotateSyntaxTerm "getField" (fromParserExpr expr)
    ETypeSig inner ty ->
      R.ETypeSig <$> resolveExpr inner <*> resolveType ty
    EParen inner ->
      R.EParen <$> resolveExpr inner
    EList items -> do
      items' <- mapM resolveExpr items
      sp <- currentSpan
      annotation <- resolution sp IdentifierList ResolutionNamespaceTerm (Found EntitySyntax)
      info <- currentModuleInfo
      let list = R.EResolved annotation (R.EList items')
      if OverloadedLists `elem` moduleInfoExtensions info
        then annotateSyntaxTerm "fromListN" list
        else pure list
    ETuple flavor items -> do
      items' <- mapM resolveMaybeExpr items
      sp <- currentSpan
      annotation <- resolution sp (IdentifierTuple flavor (length items)) ResolutionNamespaceTerm (Found EntitySyntax)
      pure (R.EResolved annotation (R.ETuple flavor items'))
    EUnboxedSum alt arity inner ->
      R.EUnboxedSum alt arity <$> resolveExpr inner
    ETypeApp fun ty ->
      R.ETypeApp <$> resolveExpr fun <*> resolveType ty
    EApp fun arg ->
      R.EApp <$> resolveExpr fun <*> resolveExpr arg
    ETHSplice inner ->
      R.ETHSplice <$> withExpressionQuote False (resolveExpr inner)
    ETHTypedSplice inner ->
      R.ETHTypedSplice <$> withExpressionQuote False (resolveExpr inner)
    EPragma pragma inner ->
      R.EPragma pragma <$> resolveExpr inner
    EDo stmts DoMdo -> do
      stmts' <- resolveMdoStmts stmts
      pure (R.EDo stmts' DoMdo)
    EDo stmts DoPlain -> do
      (_, stmts') <- resolveDoStmts stmts
      applicative <- applicativeDo stmts stmts'
      pure (fromMaybe (R.EDo stmts' DoPlain) applicative)
    EDo stmts flavor -> do
      (_, stmts') <- resolveDoStmts stmts
      pure (R.EDo stmts' flavor)
    -- A quasi-quote expression compiles to a runtime error. The quoter
    -- and the quoted text stay unresolved because nothing consumes them.
    EQuasiQuote {} -> pure (fromParserExpr expr)
    EListComp body stmts -> do
      info <- currentModuleInfo
      case traverse monadCompStmt stmts of
        Just monadStmts
          | MonadComprehensions `elem` moduleInfoExtensions info ->
              resolveMonadComprehension body monadStmts
        _ -> do
          (scope, stmts') <- resolveCompStmts stmts
          body' <- withScope scope (resolveExpr body)
          pure (R.EListComp body' stmts')
    -- Each branch of a parallel comprehension sees only the outer scope.
    -- The body sees the binders of all branches.
    EListCompParallel body branches -> do
      (bounds, branches') <- mapAndUnzipM resolveCompStmtsBinding branches
      body' <- extendScope (foldr unionScope emptyScope bounds) (resolveExpr body)
      pure (R.EListCompParallel body' branches')
    -- Template Haskell quotes compile to a runtime error. The quoted
    -- syntax stays unresolved because nothing consumes it.
    -- The body of an expression quote is code for a later stage, but its
    -- names have the scope of the quote. The change of a quote into an
    -- expression that builds its syntax tree reads their resolutions.
    ETHExpQuote inner ->
      R.ETHExpQuote <$> withExpressionQuote True (resolveExpr inner)
    ETHTypedQuote {} -> pure (fromParserExpr expr)
    -- The body of a declaration or pattern quote stays unresolved. But
    -- each splice in the body is an expression at the level of the quote,
    -- and the type checker examines it.
    ETHDeclQuote decls ->
      R.ETHDeclQuote <$> mapM (convertDecl quoteSpliceHooks) decls
    -- The names of a type quote have the scope of the quote, as the names
    -- of an expression quote. The change of the quote reads them.
    ETHTypeQuote ty ->
      R.ETHTypeQuote <$> resolveType ty
    ETHPatQuote pat ->
      R.ETHPatQuote <$> convertPattern quoteSpliceHooks pat
    -- A name quote gives the name of a term or of a type. The change of
    -- the quote into a Template Haskell name reads the resolution.
    ETHNameQuote inner ->
      R.ETHNameQuote <$> resolveExpr inner
    ETHTypeNameQuote ty ->
      R.ETHTypeNameQuote <$> resolveType ty
    EProc {} -> R.EResolved . ResolutionError <$> unhandledSyntax ResolutionNamespaceTerm "EProc" <*> pure (fromParserExpr expr)

-- | Resolve the splices in the unresolved body of a quote.
--
-- The copy does not go into a nested quote, because the splices of a
-- nested quote are at a different level.
quoteSpliceHooks :: SpliceHooks ResolveM
quoteSpliceHooks =
  SpliceHooks
    { hookExprSplice = resolveExpr,
      hookTypeSplice = withResetLocalSupply . resolveExpr,
      hookPatternSplice = withResetLocalSupply . resolveExpr,
      hookDeclSplice = resolveExpr
    }

-- | An overloaded integer literal applies fromInteger to an Integer.
--
-- The fromInteger resolution sits inside the Integer type resolution.
resolveIntegerLiteral :: R.Expr -> ResolveM R.Expr
resolveIntegerLiteral expr = do
  sp <- currentSpan
  maybeIntegerAnn <- integerTypeAnnotation sp
  annotated <- annotateSyntaxTerm "fromInteger" expr
  pure (maybe annotated (`R.EResolved` annotated) maybeIntegerAnn)

-- | An overloaded fractional literal applies fromRational to a Rational.
--
-- The type of the Rational comes from the type of the method, so the
-- literal gets only the fromRational term.
resolveFractionalLiteral :: R.Expr -> ResolveM R.Expr
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
-- its [Char] type and gets no resolution.
resolveStringLiteral :: R.Expr -> ResolveM R.Expr
resolveStringLiteral expr = do
  info <- currentModuleInfo
  if OverloadedStrings `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "fromString" expr
    else pure expr

-- | The resolution of the Integer type in the built-in scope.
--
-- The result is 'Nothing' when the built-in scope does not have Integer.
-- A module without Integer then gives the literal only the fromInteger term.
integerTypeAnnotation :: Maybe SourceSpan -> ResolveM (Maybe ResolutionResult)
integerTypeAnnotation sp = do
  info <- currentModuleInfo
  case lookupType "Integer" (moduleInfoBuiltinScope info) of
    NotFound _ -> pure Nothing
    resolved -> Just <$> resolution sp (IdentifierNamed "Integer") ResolutionNamespaceType resolved

-- | Give an expression the syntax term that its desugaring applies.
--
-- The term comes from the built-in scope.
-- RebindableSyntax takes the term from the lexical scope instead.
annotateSyntaxTerm :: Text -> R.Expr -> ResolveM R.Expr
annotateSyntaxTerm name expr = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (R.EResolved annotation expr)

-- | Negation applies the negate syntax term to its operand.
annotateNegate :: R.Expr -> ResolveM R.Expr
annotateNegate = annotateSyntaxTerm "negate"

resolvePrimitiveLiteralType :: NumericType -> R.Expr -> ResolveM R.Expr
resolvePrimitiveLiteralType numericType expr =
  case primitiveNumericTypeName numericType of
    Just name -> resolvePrimitiveLiteralTypeName name expr
    Nothing -> resolveIntegerLiteral expr

resolvePrimitiveLiteralTypeName :: Text -> R.Expr -> ResolveM R.Expr
resolvePrimitiveLiteralTypeName name expr = do
  sp <- currentSpan
  annotation <- primitiveLiteralTypeAnnotation sp name
  pure (R.EResolved annotation expr)

-- | The resolution that names the type of a primitive literal.
primitiveLiteralTypeAnnotation :: Maybe SourceSpan -> Text -> ResolveM ResolutionResult
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
-- An ordinary if expression uses the built-in Bool and gets no resolution.
annotateRebindableIf :: R.Expr -> ResolveM R.Expr
annotateRebindableIf expr = do
  info <- currentModuleInfo
  if RebindableSyntax `elem` moduleInfoExtensions info
    then annotateSyntaxTerm "ifThenElse" expr
    else pure expr

-- | Give a literal pattern the names that the type checker needs.
--
-- An overloaded integer pattern gets the syntax terms that compare it.
-- A string pattern gets them only under OverloadedStrings.
-- A primitive literal pattern gets the resolution of its primitive type.
annotatePatternLiteral :: (Literal -> R.Pattern) -> Literal -> ResolveM R.Pattern
annotatePatternLiteral mkPattern sourceLit = do
  lit <- numDecimalsLiteral sourceLit
  let pat = mkPattern lit
  sp <- (literalSpan lit <|>) <$> currentSpan
  case primitiveLiteralTypeName lit of
    Just typeName -> do
      typeAnn <- primitiveLiteralTypeAnnotation sp typeName
      pure (R.PResolved typeAnn pat)
    Nothing ->
      case peelLiteralAnn lit of
        LitInt _ TInteger _ -> do
          maybeIntegerAnn <- integerTypeAnnotation sp
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromInteger")
          pure (foldr R.PResolved pat (maybe methodAnns (: methodAnns) maybeIntegerAnn))
        LitFloat _ TFractional _ -> do
          methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromRational")
          pure (foldr R.PResolved pat methodAnns)
        LitString _ _ -> do
          info <- currentModuleInfo
          if OverloadedStrings `elem` moduleInfoExtensions info
            then do
              methodAnns <- mapM (syntaxTermAnnotation sp) (overloadedPatternMethods "fromString")
              pure (foldr R.PResolved pat methodAnns)
            else pure pat
        _ -> pure pat
  where
    -- A negated literal pattern also negates the converted literal.
    overloadedPatternMethods conversion =
      case R.peelPatternAnn (mkPattern sourceLit) of
        R.PNegLit {} -> [conversion, "negate", "=="]
        _ -> [conversion, "=="]

-- | Give an n+k pattern the names that its match uses.
--
-- The Haskell 98 Report gives the match of @n+k@ against @v@ as the test
-- @v >= fromInteger k@, and then @n@ binds to @v - fromInteger k@.
annotateNPlusKPattern :: R.UnqualifiedName -> Literal -> ResolveM R.Pattern
annotateNPlusKPattern name sourceLit = do
  lit <- numDecimalsLiteral sourceLit
  sp <- (literalSpan lit <|>) <$> currentSpan
  maybeIntegerAnn <- integerTypeAnnotation sp
  methodAnns <- mapM (syntaxTermAnnotation sp) ["fromInteger", ">=", "-"]
  pure (foldr R.PResolved (R.PNPlusK name lit) (maybe methodAnns (: methodAnns) maybeIntegerAnn))

-- | The innermost source span a literal's annotations carry.
literalSpan :: Literal -> Maybe SourceSpan
literalSpan = go Nothing
  where
    go ambient (LitAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient _ = ambient

syntaxTermAnnotation :: Maybe SourceSpan -> Text -> ResolveM ResolutionResult
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
    else NotFound "unknown built-in syntax term"
  where
    builtinSyntaxTermNames =
      [ "fromInteger",
        "fromRational",
        "fromString",
        "fromList",
        "fromListN",
        "toList",
        "getField",
        "negate",
        "==",
        ">=",
        "-",
        ">>=",
        ">>",
        "return",
        "fail",
        "mfix",
        "guard",
        "fmap",
        "<*>",
        "enumFrom",
        "enumFromThen",
        "enumFromTo",
        "enumFromThenTo"
      ]

rebindableSyntaxTerm :: ModuleInfo -> Scope -> Text -> Resolution
rebindableSyntaxTerm info scope name =
  case lookupTerm name scope of
    Found (EntityGlobal global)
      | globalNameModule global == "Prelude",
        not (moduleInfoExplicitPreludeImport info) ->
          NotFound "unbound"
    resolved -> resolved

resolveMaybeExpr :: Maybe Expr -> ResolveM (Maybe R.Expr)
resolveMaybeExpr = traverse resolveExpr

resolveCompStmts :: [CompStmt] -> ResolveM (Scope, [R.CompStmt])
resolveCompStmts stmts = do
  scope <- currentScope
  (bound, stmts') <- resolveCompStmtsBinding stmts
  pure (unionScope bound scope, stmts')

-- | Resolve the statements of a comprehension in the current scope. The
-- result scope has only the binders of the statements, and a later binder
-- shadows an earlier binder with the same name.
resolveCompStmtsBinding :: [CompStmt] -> ResolveM (Scope, [R.CompStmt])
resolveCompStmtsBinding stmts = do
  scope <- currentScope
  go scope emptyScope stmts
  where
    go outer bound stmts' =
      withScope (unionScope bound outer) $
        case stmts' of
          [] -> pure (bound, [])
          stmt : rest -> do
            (stmtBound, stmt') <- resolveCompStmt stmt
            (bound', rest') <- go outer (unionScope stmtBound bound) rest
            pure (bound', stmt' : rest')

-- | Resolve one statement of a comprehension. The result scope has only
-- the binders of the statement.
resolveCompStmt :: CompStmt -> ResolveM (Scope, R.CompStmt)
resolveCompStmt stmt =
  case stmt of
    CompAnn ann inner -> do
      (bound, inner') <- withPushedSpan ann (resolveCompStmt inner)
      pure (bound, R.CompAnn ann inner')
    CompGen pat src -> do
      src' <- resolveExpr src
      (patScope, pat') <- bindPattern pat
      pure (patScope, R.CompGen pat' src')
    CompGuard guard -> do
      guard' <- resolveExpr guard
      pure (emptyScope, R.CompGuard guard')
    CompLetDecls decls -> do
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (localScope, R.CompLetDecls decls')
    CompThen expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, R.CompThen expr')
    CompThenBy f byExpr -> do
      f' <- resolveExpr f
      byExpr' <- resolveExpr byExpr
      pure (emptyScope, R.CompThenBy f' byExpr')
    CompGroupUsing expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, R.CompGroupUsing expr')
    CompGroupByUsing byExpr usingExpr -> do
      byExpr' <- resolveExpr byExpr
      usingExpr' <- resolveExpr usingExpr
      pure (emptyScope, R.CompGroupByUsing byExpr' usingExpr')

-- | A comprehension statement in a monadic form. A TransformListComp
-- statement has no such form.
data MonadCompStmt
  = MonadCompAnn Annotation MonadCompStmt
  | MonadCompDo (DoStmt Expr)
  | MonadCompGuard Expr

monadCompStmt :: CompStmt -> Maybe MonadCompStmt
monadCompStmt stmt =
  case stmt of
    CompAnn ann inner -> MonadCompAnn ann <$> monadCompStmt inner
    CompGen pat src -> Just (MonadCompDo (DoBind pat src))
    CompLetDecls decls -> Just (MonadCompDo (DoLetDecls decls))
    CompGuard condition -> Just (MonadCompGuard condition)
    _ -> Nothing

-- | Under MonadComprehensions, a comprehension @[e | quals]@ is a do block
-- in each monad, as in GHC:
--
-- > do { quals'; return e }
--
-- A generator becomes a bind, a let stays a let, and a guard @g@ becomes
-- the statement @guard g@. The do block then gets the usual monad methods.
resolveMonadComprehension :: Expr -> [MonadCompStmt] -> ResolveM R.Expr
resolveMonadComprehension body stmts = do
  scope <- currentScope
  (scope', stmts') <- go scope stmts
  withScope scope' $ do
    body' <- resolveExpr body
    returnVar <- monadCompSyntaxVar "return"
    pure (R.EDo (stmts' <> [R.DoExpr (R.EApp returnVar body')]) DoPlain)
  where
    go scope stmts'' =
      withScope scope $
        case stmts'' of
          [] -> pure (scope, [])
          stmt : rest -> do
            (scope', stmt') <- resolveMonadCompStmt stmt
            (scope'', rest') <- go scope' rest
            pure (scope'', stmt' : rest')

resolveMonadCompStmt :: MonadCompStmt -> ResolveM (Scope, R.DoStmt R.Expr)
resolveMonadCompStmt stmt =
  case stmt of
    MonadCompAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveMonadCompStmt inner)
      pure (scope', R.DoAnn ann inner')
    MonadCompDo doStmt -> resolveDoStmt False doStmt
    MonadCompGuard condition -> do
      scope <- currentScope
      condition' <- resolveExpr condition
      guardVar <- monadCompSyntaxVar "guard"
      stmt' <- annotateDoMethod False ">>" (R.DoExpr (R.EApp guardVar condition'))
      pure (scope, stmt')

-- | A variable that names a syntax term of a monad comprehension.
monadCompSyntaxVar :: Text -> ResolveM R.Expr
monadCompSyntaxVar name = do
  sp <- currentSpan
  ann <- syntaxTermAnnotation sp name
  pure (R.EVar (R.Name Nothing NameVarId name [] (Just ann)))

resolveCaseAlt :: CaseAlt Expr -> ResolveM (R.CaseAlt R.Expr)
resolveCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (caseAltAnns alt)) $ do
    (patScope, pat') <- bindPattern (caseAltPattern alt)
    rhs' <- extendScope patScope (resolveRhs (caseAltRhs alt))
    pure (R.CaseAlt (caseAltAnns alt) pat' rhs')

resolveLambdaCaseAlt :: LambdaCaseAlt -> ResolveM R.LambdaCaseAlt
resolveLambdaCaseAlt alt =
  withEffectiveSpan (sourceSpanFromAnns (lambdaCaseAltAnns alt)) $ do
    (patScope, pats') <- bindPatterns (lambdaCaseAltPats alt)
    rhs' <- extendScope patScope (resolveRhs (lambdaCaseAltRhs alt))
    pure (R.LambdaCaseAlt (lambdaCaseAltAnns alt) pats' rhs')

resolveRecordFields :: [RecordField Expr] -> ResolveM [R.RecordField R.Expr]
resolveRecordFields =
  mapM
    ( \field -> do
        value' <- resolveExpr (recordFieldPunValue field)
        pure (R.RecordField (fromParserName (recordFieldName field)) value' (recordFieldPun field))
    )

-- | The value of a record field. A pun @{ M.x }@ uses the variable @x@, as
-- in GHC. The qualifier selects only the field.
recordFieldPunValue :: RecordField Expr -> Expr
recordFieldPunValue field
  | recordFieldPun field = unqualify (recordFieldValue field)
  | otherwise = recordFieldValue field
  where
    unqualify expr =
      case expr of
        EAnn annotation inner -> EAnn annotation (unqualify inner)
        EVar name -> EVar name {nameQualifier = Nothing}
        _ -> expr

-- | A record update names the field selectors that it writes, and the
-- selector must be in scope. The resolution of each label tells the type
-- checker which data type the update rebuilds. Two data types can declare a
-- field with the same name, and then only the resolution separates them.
resolveRecordUpdateFields :: [RecordField Expr] -> ResolveM [R.RecordField R.Expr]
resolveRecordUpdateFields =
  mapM
    ( \field -> do
        name' <- resolveTermUseAtName (recordFieldName field)
        value' <- resolveExpr (recordFieldPunValue field)
        pure (R.RecordField name' value' (recordFieldPun field))
    )

-- | A record wildcard in a construction fills each remaining field with the
-- variable that has the field name. The construction lists these fields as
-- puns, so a later phase sees an ordinary record construction.
resolveRecordConWildcardFields :: Maybe SourceSpan -> Name -> [RecordField Expr] -> Bool -> ResolveM [R.RecordField R.Expr]
resolveRecordConWildcardFields sp conName fields wildcard = do
  scope <- currentScope
  mapM fieldPun (recordWildcardFieldNames scope conName fields wildcard)
  where
    fieldPun fieldName = do
      value <- resolveTermUse (Name Nothing NameVarId fieldName (map mkAnnotation (maybeToList sp)))
      pure
        R.RecordField
          { R.recordFieldName = R.mkName Nothing NameVarId fieldName,
            R.recordFieldValue = R.EVar value,
            R.recordFieldPun = True
          }

resolveDoStmts :: [DoStmt Expr] -> ResolveM (Scope, [R.DoStmt R.Expr])
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

resolveDoStmt :: Bool -> DoStmt Expr -> ResolveM (Scope, R.DoStmt R.Expr)
resolveDoStmt isLast stmt =
  case stmt of
    DoAnn ann inner -> do
      (scope', inner') <- withPushedSpan ann (resolveDoStmt isLast inner)
      pure (scope', R.DoAnn ann inner')
    DoExpr body -> do
      scope <- currentScope
      body' <- resolveExpr body
      stmt' <- annotateDoMethod isLast ">>" (R.DoExpr body')
      pure (scope, stmt')
    DoBind pat body -> do
      scope <- currentScope
      body' <- resolveExpr body
      (patScope, pat') <- bindPattern pat
      stmt' <- annotateDoMethod isLast ">>=" =<< annotateDoBindFail isLast pat' (R.DoBind pat' body')
      pure (unionScope patScope scope, stmt')
    DoLetDecls decls -> do
      scope <- currentScope
      (binderAnnotations, localScope) <- allocateLocalDeclBinders decls
      decls' <- extendScope localScope (resolveBoundDecls binderAnnotations Map.empty decls)
      pure (unionScope localScope scope, R.DoLetDecls decls')
    DoRecStmt stmts -> do
      scope <- currentScope
      (targets, recScope) <- allocateDoStmtBinders stmts
      stmts' <- extendScope recScope (mapM (resolveRecGroupStmt targets) stmts)
      stmt' <- annotateRecStmt (R.DoRecStmt stmts')
      pure (unionScope recScope scope, stmt')

-- | Rewrite a resolved do block with the Applicative methods when
-- ApplicativeDo is on. GHC does this rewrite in its renamer.
--
-- The rewrite applies only when the block has this form:
--
-- > do { p1 <- e1; ...; pn <- en; return e }
--
-- An expression statement is a bind with a wildcard pattern. Each pattern
-- is a variable, a wildcard, or a lazy pattern. No action uses a variable
-- that an earlier statement binds. The last statement applies @return@ or
-- @pure@, directly or with @$@. Then the block is
--
-- > fmap (\p1 ... pn -> e) e1 <*> ... <*> en
--
-- and it needs only a Functor or an Applicative instance. A block of a
-- different form stays a monadic do block.
--
-- The block can also have let statements between the binds. A let
-- statement must not use a variable that an earlier pattern binds. As in
-- GHC, the let statements then go around the applicative expression.
--
-- GHC also splits a block with dependencies into independent segments.
-- This rewrite does not do that.
--
-- The first list has the source statements, and the second list has the
-- resolved statements.
applicativeDo :: [DoStmt Expr] -> [R.DoStmt R.Expr] -> ResolveM (Maybe R.Expr)
applicativeDo sourceStmts stmts = do
  info <- currentModuleInfo
  fieldScope <- currentScope
  let extensions = moduleInfoExtensions info
      binderNames = concatMap (doStmtBinderKeys fieldScope) sourceStmts
  if ApplicativeDo `notElem` extensions
    || RebindableSyntax `elem` extensions
    || length binderNames /= Set.size (Set.fromList binderNames)
    then pure Nothing
    else case List.unsnoc (map R.peelDoStmtAnn stmts) of
      Just (args, R.DoExpr final)
        | Just parts <- traverse applicativeArg args,
          (lets, actions@(_ : _)) <- partitionEithers parts,
          independentActions actions,
          independentLets parts,
          Just result <- returnedExpr info final ->
            Just . flip (foldr R.ELetDecls) lets <$> applicativeExpr actions result
      _ -> pure Nothing
  where
    applicativeArg stmt =
      case stmt of
        R.DoBind pat action | lazyPattern pat -> Just (Right (pat, action))
        R.DoExpr action -> Just (Right (R.PWildcard, action))
        R.DoLetDecls decls -> Just (Left decls)
        _ -> Nothing
    lazyPattern pat =
      case R.peelPatternAnn pat of
        R.PParen inner -> lazyPattern inner
        R.PVar {} -> True
        R.PWildcard -> True
        R.PIrrefutable {} -> True
        _ -> False

-- | Whether no let statement uses a variable that an earlier pattern binds.
-- As in GHC, such let statements move to the start of the block. Thus they
-- go around the applicative expression, and each action and the result can
-- use their variables.
independentLets :: [Either [R.Decl] (R.Pattern, R.Expr)] -> Bool
independentLets = go Set.empty
  where
    go _ [] = True
    go bound (Left decls : rest) =
      Set.disjoint (localEntities decls) bound && go bound rest
    go bound (Right (pat, _) : rest) =
      go (Set.union bound (localEntities pat)) rest

-- | Whether no action or pattern uses a variable that an earlier pattern
-- binds.
independentActions :: [(R.Pattern, R.Expr)] -> Bool
independentActions = go Set.empty
  where
    go _ [] = True
    go bound ((pat, action) : rest) =
      let patLocals = localEntities pat
          uses = Set.union (localEntities action) patLocals
       in Set.disjoint uses bound && go (Set.union bound patLocals) rest

-- | The local entities that the resolutions of a resolved node name.
localEntities :: (Walkable a) => a -> Set.Set Entity
localEntities = Set.fromList . collectResolutions localEntity
  where
    localEntity result =
      case result of
        ResolutionOk Resolved {resolutionTarget = target@EntityLocal {}} -> Just target
        _ -> Nothing

-- | The argument of the last statement of an applicative do block. The
-- statement must be @return e@, @pure e@, @return $ e@, or @pure $ e@.
returnedExpr :: ModuleInfo -> R.Expr -> Maybe R.Expr
returnedExpr info expr =
  case peelExpr expr of
    R.EApp fun arg | isReturn fun -> Just arg
    R.EInfix fun op arg | isDollar op && isReturn fun -> Just arg
    _ -> Nothing
  where
    peelExpr e =
      case e of
        R.EAnn _ inner -> peelExpr inner
        R.EResolved _ inner -> peelExpr inner
        R.EParen inner -> peelExpr inner
        _ -> e
    builtinTarget name =
      case lookupTerm name (moduleInfoBuiltinScope info) of
        Found entity -> Just entity
        NotFound _ -> Nothing
    isReturn fun =
      case peelExpr fun of
        R.EVar name ->
          case resolvedNameTarget name of
            Just target -> Just target `elem` map builtinTarget ["return", "pure"]
            Nothing -> False
        _ -> False
    isDollar op =
      case resolvedNameTarget op of
        Just (EntityGlobal global) ->
          globalNameText global == "$" && globalNameModule global `elem` ["GHC.Base", "GHC.Internal.Base"]
        _ -> False

-- | Build @fmap (\p1 ... pn -> e) e1 <*> ... <*> en@.
applicativeExpr :: [(R.Pattern, R.Expr)] -> R.Expr -> ResolveM R.Expr
applicativeExpr actions result = do
  sp <- currentSpan
  fmapVar <- syntaxTermVar sp NameVarId "fmap"
  apVar <- syntaxTermVar sp NameVarSym "<*>"
  let lambda = R.EParen (R.ELambdaPats (map fst actions) result)
  case map snd actions of
    first : rest ->
      pure (foldl (R.EApp . R.EApp apVar) (R.EApp (R.EApp fmapVar lambda) first) rest)
    [] -> pure result
  where
    syntaxTermVar sp kind name = do
      ann <- syntaxTermAnnotation sp name
      pure (R.EVar (R.Name Nothing kind name [] (Just ann)))

-- | Under OverloadedLists, a list pattern @[p1, ..., pn]@ matches through
-- @toList@, as in GHC. The resolver only records the syntax terms: @toList@
-- for a match, and @fromListN@ for the expression of a bidirectional
-- pattern synonym. The type checker and the desugarer do the conversion.
overloadedListPattern :: R.Pattern -> ResolveM R.Pattern
overloadedListPattern pat = do
  info <- currentModuleInfo
  if OverloadedLists `elem` moduleInfoExtensions info
    then do
      sp <- currentSpan
      methodAnns <- mapM (syntaxTermAnnotation sp) ["toList", "fromListN"]
      pure (foldr R.PResolved pat methodAnns)
    else pure pat

-- | Resolve the statements of an @mdo@ block.
--
-- Each variable that a statement binds is in scope in all statements of the
-- block, as in GHC. The statements before the last one then go into
-- segments. A segment is a minimal group of statements that some statement
-- uses before the group binds it. Each segment becomes a @rec@ statement, and
-- the other statements stay as ordinary statements. Thus the type checker
-- and the desugarer see only @rec@ statements, and an @mdo@ block without a
-- recursive use does not need a 'MonadFix' instance.
resolveMdoStmts :: [DoStmt Expr] -> ResolveM [R.DoStmt R.Expr]
resolveMdoStmts stmts =
  case List.unsnoc stmts of
    Nothing -> pure []
    Just (body, final) -> do
      (targets, recScope) <- allocateDoStmtBinders body
      fieldScope <- currentScope
      extendScope recScope $ do
        body' <- mapM (resolveRecGroupStmt targets) body
        (_, final') <- resolveDoStmt True final
        segmented <- segmentMdoStmts fieldScope targets (fst (doStmtUses final')) (zip body body')
        pure (segmented <> [final'])

-- | Put each recursive segment of the resolved statements of an @mdo@ block
-- into a @rec@ statement. Each pair has the source statement and the
-- resolved statement. The set has the local variables that the last
-- statement of the block uses. Each @rec@ statement gets its
-- 'RecKnotVariables'.
segmentMdoStmts :: Scope -> Map.Map Text Entity -> Set.Set Entity -> [(DoStmt Expr, R.DoStmt R.Expr)] -> ResolveM [R.DoStmt R.Expr]
segmentMdoStmts fieldScope targets finalUses stmts = go indexed
  where
    indexed = zip [0 :: Int ..] (map snd stmts)
    stmtUses = Map.fromList [(index, doStmtUses stmt) | (index, stmt) <- indexed]
    usesOf index = maybe Set.empty fst (Map.lookup index stmtUses)
    -- The knot of the segment from the first index to the last index. It
    -- has each variable of the segment that a statement uses before the
    -- segment binds it, and each variable that a later statement uses.
    knotVariables first end =
      let segmentIndices = [first .. end]
          laterUses = Set.unions (finalUses : map usesOf [end + 1 .. length stmts - 1])
          earlyUse entity definer =
            or
              [ Set.member entity used && (index < definer || (selfUse && index == definer))
              | index <- segmentIndices,
                Just (used, selfUse) <- [Map.lookup index stmtUses]
              ]
       in Set.fromList
            [ entity
            | index <- segmentIndices,
              entity <- definitions !! index,
              Set.member entity laterUses || earlyUse entity index
            ]
    definitions =
      [ mapMaybe (`Map.lookup` targets) (doStmtBinderKeys fieldScope source)
      | (source, _) <- stmts
      ]
    definingIndex =
      Map.fromList [(entity, index) | (index, entities) <- zip [0 ..] definitions, entity <- entities]
    -- The last statement that the statement at an index uses before it binds
    -- the variable. A bind statement can use its own variables, but a let
    -- statement that uses its own variables is an ordinary recursive let.
    reach :: Int -> R.DoStmt R.Expr -> Maybe Int
    reach index stmt =
      let (uses, selfUse) = doStmtUses stmt
          laterUses =
            [ definer
            | entity <- Set.toList uses,
              Just definer <- [Map.lookup entity definingIndex],
              definer > index || (selfUse && definer == index)
            ]
       in if null laterUses then Nothing else Just (maximum laterUses)
    reaches = Map.fromList [(index, reach index stmt) | (index, stmt) <- indexed]
    reachOf index = Map.findWithDefault Nothing index reaches
    go [] = pure []
    go remaining@((index, stmt) : rest) =
      case reachOf index of
        Nothing -> (stmt :) <$> go rest
        Just firstEnd -> do
          let end = extend index firstEnd
              (segment, after) = splitAt (end - index + 1) remaining
              knot = RecKnotVariables (knotVariables index end)
          recStmt <- annotateRecStmt (R.DoRecKnot knot (R.DoRecStmt (map snd segment)))
          (recStmt :) <$> go after
    -- A statement inside the segment can reach further than the segment.
    extend index end =
      let end' = maximum (end : mapMaybe reachOf [index .. end])
       in if end' == end then end else extend index end'

-- | The local variables that a resolved statement uses, and whether the
-- statement can use its own variables recursively.
doStmtUses :: R.DoStmt R.Expr -> (Set.Set Entity, Bool)
doStmtUses stmt =
  case R.peelDoStmtAnn stmt of
    R.DoBind _ body -> (localEntities body, True)
    R.DoExpr body -> (localEntities body, False)
    _ -> (localEntities stmt, False)

-- | Resolve one statement of a recursive group. The variables of the whole
-- group are already in scope, and each binder takes its entity from the
-- targets. Each statement gets a sequencing method, because the desugarer
-- puts the tuple of the group variables after the last statement.
resolveRecGroupStmt :: Map.Map Text Entity -> DoStmt Expr -> ResolveM (R.DoStmt R.Expr)
resolveRecGroupStmt targets stmt =
  case stmt of
    DoAnn ann inner ->
      R.DoAnn ann <$> withPushedSpan ann (resolveRecGroupStmt targets inner)
    DoExpr body -> do
      body' <- resolveExpr body
      annotateDoMethod False ">>" (R.DoExpr body')
    DoBind pat body -> do
      body' <- resolveExpr body
      pat' <- resolvePatternDefinition definition pat
      annotateDoMethod False ">>=" =<< annotateDoBindFail False pat' (R.DoBind pat' body')
    DoLetDecls decls ->
      R.DoLetDecls <$> resolveBoundDecls targets Map.empty decls
    DoRecStmt stmts -> do
      stmts' <- mapM (resolveRecGroupStmt targets) stmts
      annotateRecStmt (R.DoRecStmt stmts')
  where
    definition name = Found <$> Map.lookup (renderUnqualifiedName name) targets

-- | Give a @rec@ statement the methods of its desugaring: @mfix@ ties the
-- knot, @return@ gives the tuple of the group variables, and @>>=@ binds
-- that tuple for the statements that follow.
annotateRecStmt :: R.DoStmt R.Expr -> ResolveM (R.DoStmt R.Expr)
annotateRecStmt stmt = do
  sp <- currentSpan
  mfixAnn <- syntaxTermAnnotation sp "mfix"
  returnAnn <- syntaxTermAnnotation sp "return"
  bindAnn <- syntaxTermAnnotation sp ">>="
  pure (R.DoResolved mfixAnn (R.DoResolved returnAnn (R.DoResolved bindAnn stmt)))

-- | Give one entity to each variable that the statements bind.
allocateDoStmtBinders :: [DoStmt Expr] -> ResolveM (Map.Map Text Entity, Scope)
allocateDoStmtBinders stmts = do
  fieldScope <- currentScope
  foldM addBinder (Map.empty, emptyScope) (concatMap (doStmtBinderKeys fieldScope) stmts)
  where
    addBinder (targets, scope) key
      | Map.member key targets = pure (targets, scope)
      | otherwise = do
          entity <- freshLocal
          pure (Map.insert key entity targets, insertTerm key entity scope)

-- | The names of the variables that a statement binds, in source order.
-- The record fields give the variables of a record wildcard pattern.
doStmtBinderKeys :: Scope -> DoStmt Expr -> [Text]
doStmtBinderKeys fieldScope stmt =
  case stmt of
    DoAnn _ inner -> doStmtBinderKeys fieldScope inner
    DoBind pat _ -> map (renderUnqualifiedName . snd) (collectPatVarBinders fieldScope Nothing pat)
    DoLetDecls decls -> map (renderUnqualifiedName . snd) (concatMap (declBinderCandidates fieldScope) decls)
    DoExpr _ -> []
    DoRecStmt stmts -> concatMap (doStmtBinderKeys fieldScope) stmts

-- | Give a do statement the method that sequences it.
--
-- A bind statement uses @>>=@ and an expression statement uses @>>@.
-- The last statement is the result of the block and gets no method.
-- Ordinary do notation uses the built-in Monad methods.
-- RebindableSyntax uses lexical lookup instead.
annotateDoMethod :: Bool -> Text -> R.DoStmt R.Expr -> ResolveM (R.DoStmt R.Expr)
annotateDoMethod isLast name stmt
  | isLast = pure stmt
  | otherwise = do
      sp <- currentSpan
      methodAnn <- syntaxTermAnnotation sp name
      pure (R.DoResolved methodAnn stmt)

-- | Give a bind statement the @fail@ method when its pattern can fail to
-- match. The method goes inside the @>>=@ method. The type checker drops
-- the method when the constructors of the pattern make it irrefutable.
annotateDoBindFail :: Bool -> R.Pattern -> R.DoStmt R.Expr -> ResolveM (R.DoStmt R.Expr)
annotateDoBindFail isLast pat stmt
  | isLast || not (patternMayFail pat) = pure stmt
  | otherwise = do
      sp <- currentSpan
      failAnn <- syntaxTermAnnotation sp "fail"
      pure (R.DoResolved failAnn stmt)

-- | Whether a pattern can fail to match, as far as its syntax tells. A
-- constructor pattern can fail here; the type checker knows the other
-- constructors of its type and makes the final decision.
patternMayFail :: R.Pattern -> Bool
patternMayFail pat =
  case pat of
    R.PAnn _ inner -> patternMayFail inner
    R.PResolved _ inner -> patternMayFail inner
    R.PParen inner -> patternMayFail inner
    R.PAs _ inner -> patternMayFail inner
    R.PStrict inner -> patternMayFail inner
    R.PTypeSig inner _ -> patternMayFail inner
    R.PView _ inner -> patternMayFail inner
    R.PVar {} -> False
    R.PWildcard -> False
    R.PIrrefutable {} -> False
    R.PTypeBinder {} -> False
    R.PTypeSyntax {} -> False
    R.PTuple _ items -> any patternMayFail items
    R.PBuiltinCon (BuiltinTuple _ _) _ items -> any patternMayFail items
    _ -> True

resolveArithSeq :: ArithSeq -> ResolveM R.ArithSeq
resolveArithSeq arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner ->
      R.ArithSeqAnn ann <$> withPushedSpan ann (resolveArithSeq inner)
    ArithSeqFrom from -> do
      resolved <- R.ArithSeqFrom <$> resolveExpr from
      annotateArithSeqMethod "enumFrom" resolved
    ArithSeqFromThen from then' -> do
      resolved <- R.ArithSeqFromThen <$> resolveExpr from <*> resolveExpr then'
      annotateArithSeqMethod "enumFromThen" resolved
    ArithSeqFromTo from to -> do
      resolved <- R.ArithSeqFromTo <$> resolveExpr from <*> resolveExpr to
      annotateArithSeqMethod "enumFromTo" resolved
    ArithSeqFromThenTo from then' to -> do
      resolved <- R.ArithSeqFromThenTo <$> resolveExpr from <*> resolveExpr then' <*> resolveExpr to
      annotateArithSeqMethod "enumFromThenTo" resolved

annotateArithSeqMethod :: Text -> R.ArithSeq -> ResolveM R.ArithSeq
annotateArithSeqMethod name arithSeq = do
  sp <- currentSpan
  annotation <- syntaxTermAnnotation sp name
  pure (R.ArithSeqResolved annotation arithSeq)

resolveBoundDecls :: Map.Map Text Entity -> Map.Map Text Scope -> [Decl] -> ResolveM [R.Decl]
resolveBoundDecls binderTargets signatureScopes decls = do
  mixedErrors <- mixedImplicitParamErrors decls
  decls' <- resolveBindingGroup (\name -> Found <$> Map.lookup (renderUnqualifiedName name) binderTargets) signatureScopes decls
  pure (zipWith (maybe id R.DeclError) mixedErrors decls')

-- | The error of each implicit-parameter binding in a group that also has
-- other declarations, in the order of the declarations.
--
-- GHC does not permit a @let@ or @where@ group with both kinds of binding.
mixedImplicitParamErrors :: [Decl] -> ResolveM [Maybe ResolveError]
mixedImplicitParamErrors decls
  | any isImplicitParamDecl decls && not (all isImplicitParamDecl decls) = mapM mark decls
  | otherwise = pure (map (const Nothing) decls)
  where
    isImplicitParamDecl decl =
      case snd (peelDeclSpan decl) of
        DeclImplicitParam {} -> True
        _ -> False
    mark decl =
      case peelDeclSpan decl of
        (declSpan, DeclImplicitParam name _ _) -> do
          ambient <- currentSpan
          Just
            <$> resolveFailure
              (spanStartNameSpan (declSpan <|> ambient) name)
              (IdentifierNamed name)
              ResolutionNamespaceTerm
              "implicit-parameter binding in a group with other bindings"
        _ -> pure Nothing

declSignatureScope :: Decl -> Map.Map Text Scope -> Maybe Scope
declSignatureScope decl signatureScopes =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) signatureScopes
    Nothing -> Nothing

-- | Bind a sequence of patterns left to right. A view pattern's expression
-- sees the variables that the patterns before it bind, so
-- @f egr (find egr -> i)@ and @(x, f x -> y)@ resolve as GHC scopes them.
bindPatterns :: [Pattern] -> ResolveM (Scope, [R.Pattern])
bindPatterns = go emptyScope
  where
    go bound [] = pure (bound, [])
    go bound (pat : pats) = do
      (scope, pat') <- extendScope bound (bindPattern pat)
      (bound', pats') <- go (scope `unionScope` bound) pats
      pure (bound', pat' : pats')

bindPattern :: Pattern -> ResolveM (Scope, R.Pattern)
bindPattern pat =
  case pat of
    PAnn ann inner ->
      withPushedSpan ann $ do
        (scope, inner') <- bindPattern inner
        pure (scope, R.PAnn ann inner')
    PVar name -> do
      sp <- currentSpan
      entity <- freshLocal
      let key = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Found entity) name
      pure (termScope key entity, R.PVar name')
    PTypeBinder binder -> do
      entity <- freshLocal
      binder' <- resolveTyVarBinderKind binder
      let binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) entity) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      pure (binderScope, R.PTypeBinder binder')
    PTypeSyntax form ty -> do
      ty' <- resolveType ty
      pure (emptyScope, R.PTypeSyntax form ty')
    PWildcard -> pure (emptyScope, R.PWildcard)
    PLit lit -> do
      pat' <- annotatePatternLiteral R.PLit lit
      pure (emptyScope, pat')
    PTuple flavor pats -> do
      (scope, pats') <- bindPatterns pats
      pure (scope, R.PTuple flavor pats')
    PUnboxedSum alt arity inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, R.PUnboxedSum alt arity inner')
    PList pats -> do
      (scope, pats') <- bindPatterns pats
      pat' <- overloadedListPattern (R.PList pats')
      pure (scope, pat')
    PCon name typeArgs pats -> do
      name' <- resolveTermUseAtName name
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, R.PCon name' typeArgs' pats')
    -- A built-in constructor has no name for a scope to bind, so only the
    -- type arguments and the sub-patterns need resolving.
    PBuiltinCon builtin typeArgs pats -> do
      typeArgs' <- mapM resolveType typeArgs
      (scope, pats') <- bindPatterns pats
      pure (scope, R.PBuiltinCon builtin typeArgs' pats')
    PInfix {} -> do
      bound <- traverse bindPattern (flattenInfixPattern pat)
      let scope = foldr (\(operandScope, _) acc -> unionScope acc operandScope) emptyScope bound
      pat' <- resolveInfixChain R.PInfix (fmap snd bound)
      pure (scope, pat')
    PView expr inner -> do
      expr' <- resolveExpr expr
      (scope, inner') <- bindPattern inner
      pure (scope, R.PView expr' inner')
    PAs alias inner -> do
      here <- currentSpan
      let aliasKey = renderUnqualifiedName alias
      aliasEntity <- freshLocal
      alias' <- resolveUnqualifiedNameTo (spanStartNameSpan here aliasKey) ResolutionNamespaceTerm (Found aliasEntity) alias
      let aliasScope = termScope aliasKey aliasEntity
      (innerScope, inner') <- bindPattern inner
      pure (unionScope innerScope aliasScope, R.PAs alias' inner')
    PStrict inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, R.PStrict inner')
    PIrrefutable inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, R.PIrrefutable inner')
    PParen inner -> do
      (scope, inner') <- bindPattern inner
      pure (scope, R.PParen inner')
    PRecord name fields wildcard -> do
      name' <- resolveTermUseAtName name
      (fieldScopes, fields') <-
        mapAndUnzipM
          ( \field -> do
              (fieldScope, fieldPat') <- bindPattern (recordFieldValue field)
              pure (fieldScope, R.RecordField (fromParserName (recordFieldName field)) fieldPat' (recordFieldPun field))
          )
          fields
      wildcardEntries <- bindRecordWildcardFields name fields wildcard
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (R.nameAnns name') <|> ambient
      -- A record wildcard binds each remaining field to a variable with the
      -- field name. The pattern lists these fields as puns, so a later phase
      -- sees an ordinary record pattern.
      let wildcardScope = Scope (Map.fromList wildcardEntries) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
      wildcardFields <-
        mapM
          ( \(fieldName, entity) -> do
              let binder = (mkUnqualifiedName NameVarId fieldName) {unqualifiedNameAnns = map mkAnnotation (maybeToList sp)}
              binder' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Found entity) binder
              pure
                R.RecordField
                  { R.recordFieldName = R.mkName Nothing NameVarId fieldName,
                    R.recordFieldValue = R.PVar binder',
                    R.recordFieldPun = True
                  }
          )
          wildcardEntries
      pure (foldr unionScope wildcardScope fieldScopes, R.PRecord name' (fields' <> wildcardFields) False)
    PTypeSig inner ty -> do
      (scope, inner') <- bindPattern inner
      ty' <- resolveType ty
      pure (scope, R.PTypeSig inner' ty')
    PNegLit lit -> do
      pat' <- annotatePatternLiteral R.PNegLit lit
      pure (emptyScope, pat')
    PNPlusK name lit -> do
      sp <- currentSpan
      entity <- freshLocal
      let key = renderUnqualifiedName name
      name' <- resolveUnqualifiedNameTo sp ResolutionNamespaceTerm (Found entity) name
      pat' <- annotateNPlusKPattern name' lit
      pure (termScope key entity, pat')
    PSplice expr -> do
      expr' <- resolveExpr expr
      pure (emptyScope, R.PSplice expr')
    PQuasiQuote {} -> do
      err <- unhandledSyntax ResolutionNamespaceTerm "PQuasiQuote"
      pure (emptyScope, R.PResolved (ResolutionError err) (fromParserPattern pat))

termScope :: Text -> Entity -> Scope
termScope key entity =
  Scope (Map.singleton key entity) Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

resolvePatternDefinition :: TermDefinition -> Pattern -> ResolveM R.Pattern
resolvePatternDefinition termDefinition pat =
  case pat of
    PAnn ann inner ->
      R.PAnn ann <$> withPushedSpan ann (resolvePatternDefinition termDefinition inner)
    PVar name -> do
      sp <- currentSpan
      R.PVar <$> resolveTermDefinitionAt sp termDefinition name
    PTypeBinder binder ->
      R.PTypeBinder <$> resolveTyVarBinderKind binder
    PTypeSyntax form ty ->
      R.PTypeSyntax form <$> resolveType ty
    PWildcard -> pure R.PWildcard
    PLit lit -> annotatePatternLiteral R.PLit lit
    PQuasiQuote {} -> R.PResolved . ResolutionError <$> unhandledSyntax ResolutionNamespaceTerm "PQuasiQuote" <*> pure (fromParserPattern pat)
    PTuple flavor pats ->
      R.PTuple flavor <$> mapM (resolvePatternDefinition termDefinition) pats
    PUnboxedSum alt arity inner ->
      R.PUnboxedSum alt arity <$> resolvePatternDefinition termDefinition inner
    PList pats ->
      overloadedListPattern . R.PList =<< mapM (resolvePatternDefinition termDefinition) pats
    PCon name typeArgs pats ->
      R.PCon <$> resolveTermUseAtName name <*> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PBuiltinCon builtin typeArgs pats ->
      R.PBuiltinCon builtin <$> mapM resolveType typeArgs <*> mapM (resolvePatternDefinition termDefinition) pats
    PInfix {} -> do
      operands <- traverse (resolvePatternDefinition termDefinition) (flattenInfixPattern pat)
      resolveInfixChain R.PInfix operands
    PView expr inner ->
      R.PView <$> withResetLocalSupply (resolveExpr expr) <*> resolvePatternDefinition termDefinition inner
    PAs alias inner -> do
      sp <- currentSpan
      R.PAs <$> resolveTermDefinitionAt sp termDefinition alias <*> resolvePatternDefinition termDefinition inner
    PStrict inner ->
      R.PStrict <$> resolvePatternDefinition termDefinition inner
    PIrrefutable inner ->
      R.PIrrefutable <$> resolvePatternDefinition termDefinition inner
    PNegLit lit -> annotatePatternLiteral R.PNegLit lit
    PNPlusK name lit -> do
      sp <- currentSpan
      name' <- resolveTermDefinitionAt sp termDefinition name
      annotateNPlusKPattern name' lit
    PParen inner ->
      R.PParen <$> resolvePatternDefinition termDefinition inner
    PRecord name fields wildcard -> do
      name' <- resolveTermUseAtName name
      fields' <-
        mapM
          ( \field -> do
              value' <- resolvePatternDefinition termDefinition (recordFieldValue field)
              pure (R.RecordField (fromParserName (recordFieldName field)) value' (recordFieldPun field))
          )
          fields
      wildcardNames <- wildcardFieldNames name fields wildcard
      ambient <- currentSpan
      let sp = sourceSpanFromAnns (R.nameAnns name') <|> ambient
      -- A record wildcard binds each remaining field to a variable with the
      -- field name. The pattern lists these fields as puns, so a later phase
      -- sees an ordinary record pattern.
      wildcardFields <-
        mapM
          ( \fieldName -> do
              let binder = (mkUnqualifiedName NameVarId fieldName) {unqualifiedNameAnns = map mkAnnotation (maybeToList sp)}
              binder' <- resolveTermDefinitionAt sp termDefinition binder
              pure
                R.RecordField
                  { R.recordFieldName = R.mkName Nothing NameVarId fieldName,
                    R.recordFieldValue = R.PVar binder',
                    R.recordFieldPun = True
                  }
          )
          wildcardNames
      pure (R.PRecord name' (fields' <> wildcardFields) False)
    PTypeSig inner ty ->
      R.PTypeSig <$> resolvePatternDefinition termDefinition inner <*> resolveType ty
    PSplice expr ->
      R.PSplice <$> withResetLocalSupply (resolveExpr expr)

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
  pure (recordWildcardFieldNames scope conName fields wildcard)

resolveDataDecl :: Text -> DataDecl -> ResolveM R.DataDecl
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
  constructors' <- mapM resolveDataConDecl (dataDeclConstructors dataDecl) >>= mapM (resolveDataConDefinitions scope (declarationFirstConstructor (dataDeclConstructors dataDecl)))
  deriving' <- mapM resolveDerivingClause (dataDeclDeriving dataDecl)
  pure
    R.DataDecl
      { R.dataDeclCTypePragma = dataDeclCTypePragma dataDecl,
        R.dataDeclHead = head',
        R.dataDeclContext = context',
        R.dataDeclKind = kind',
        R.dataDeclConstructors = constructors',
        R.dataDeclDeriving = deriving'
      }

resolveTypeFamilyDecl :: TypeFamilyDecl -> ResolveM R.TypeFamilyDecl
resolveTypeFamilyDecl familyDecl = do
  (paramScope, params') <- bindTyVarBinders (typeFamilyDeclParams familyDecl)
  (head', resultSig', equations') <-
    extendScope paramScope $
      (,,)
        <$> resolveType (typeFamilyDeclHead familyDecl)
        <*> traverse resolveTypeFamilyResultSig (typeFamilyDeclResultSig familyDecl)
        <*> traverse (mapM resolveTypeFamilyEq) (typeFamilyDeclEquations familyDecl)
  pure
    R.TypeFamilyDecl
      { R.typeFamilyDeclHeadForm = typeFamilyDeclHeadForm familyDecl,
        R.typeFamilyDeclExplicitFamilyKeyword = typeFamilyDeclExplicitFamilyKeyword familyDecl,
        R.typeFamilyDeclHead = head',
        R.typeFamilyDeclParams = params',
        R.typeFamilyDeclResultSig = resultSig',
        R.typeFamilyDeclEquations = equations'
      }

resolveTypeFamilyResultSig :: TypeFamilyResultSig -> ResolveM R.TypeFamilyResultSig
resolveTypeFamilyResultSig resultSig =
  case resultSig of
    TypeFamilyKindSig ty -> R.TypeFamilyKindSig <$> resolveType ty
    TypeFamilyTyVarSig binder -> R.TypeFamilyTyVarSig <$> resolveTyVarBinderKind binder
    TypeFamilyInjectiveSig binder injectivity ->
      R.TypeFamilyInjectiveSig <$> resolveTyVarBinderKind binder <*> pure injectivity

-- | Resolve a declaration head: its own name, and the kind signature of
-- every type variable it binds. A kind signature in a binder mentions
-- type constructors -- @(s :: Type -> Type)@ names @Type@ -- and those
-- have to be resolved like any other type, or the type checker is left to
-- guess the constructor from its unqualified name.
resolveBinderHeadKinds ::
  (UnqualifiedName -> ResolveM R.UnqualifiedName) ->
  BinderHead UnqualifiedName ->
  ResolveM (R.BinderHead R.UnqualifiedName)
resolveBinderHeadKinds resolveHeadName head' =
  case head' of
    PrefixBinderHead name params ->
      R.PrefixBinderHead <$> resolveHeadName name <*> mapM resolveTyVarBinderKind params
    InfixBinderHead lhs name rhs params ->
      R.InfixBinderHead
        <$> resolveTyVarBinderKind lhs
        <*> resolveHeadName name
        <*> resolveTyVarBinderKind rhs
        <*> mapM resolveTyVarBinderKind params

-- | Resolve the kind signature of a type variable binder. The binder itself
-- gets no entity here.
resolveTyVarBinderKind :: TyVarBinder -> ResolveM R.TyVarBinder
resolveTyVarBinderKind binder = do
  kind' <- traverse resolveType (tyVarBinderKind binder)
  pure
    R.TyVarBinder
      { R.tyVarBinderAnns = tyVarBinderAnns binder,
        R.tyVarBinderName = tyVarBinderName binder,
        R.tyVarBinderKind = kind',
        R.tyVarBinderSpecificity = tyVarBinderSpecificity binder,
        R.tyVarBinderVisibility = tyVarBinderVisibility binder
      }

resolveTypeFamilyEq :: TypeFamilyEq -> ResolveM R.TypeFamilyEq
resolveTypeFamilyEq equation = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyEqForall equation)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> resolveType (typeFamilyEqLhs equation)
        <*> resolveType (typeFamilyEqRhs equation)
  pure
    R.TypeFamilyEq
      { R.typeFamilyEqAnns = typeFamilyEqAnns equation,
        R.typeFamilyEqForall = forallBinders',
        R.typeFamilyEqHeadForm = typeFamilyEqHeadForm equation,
        R.typeFamilyEqLhs = lhs',
        R.typeFamilyEqRhs = rhs'
      }

resolveTypeFamilyInst :: TypeFamilyInst -> ResolveM R.TypeFamilyInst
resolveTypeFamilyInst = resolveTypeFamilyInstWithLhsScope emptyScope

-- | Resolve a type family equation. The extra scope applies to the left
-- side only. An associated type instance names its family there without
-- the qualifier of the class. The right side resolves in the scope of the
-- module: in @instance C.MonadConc (Program pty n) where type ThreadId
-- (Program pty n) = ThreadId@ of @dejafu@, the right side is the data type
-- @ThreadId@ that the module imports, and the family is only in scope as
-- @C.ThreadId@.
resolveTypeFamilyInstWithLhsScope :: Scope -> TypeFamilyInst -> ResolveM R.TypeFamilyInst
resolveTypeFamilyInstWithLhsScope lhsScope familyInst = do
  (forallScope, forallBinders') <- bindTyVarBinders (typeFamilyInstForall familyInst)
  (lhs', rhs') <-
    extendScope forallScope $
      (,)
        <$> extendScope lhsScope (resolveType (typeFamilyInstLhs familyInst))
        <*> resolveType (typeFamilyInstRhs familyInst)
  pure
    R.TypeFamilyInst
      { R.typeFamilyInstForall = forallBinders',
        R.typeFamilyInstHeadForm = typeFamilyInstHeadForm familyInst,
        R.typeFamilyInstLhs = lhs',
        R.typeFamilyInstRhs = rhs'
      }

-- | Resolve a data family declaration. The keyword is what the declaration
-- starts with, which locates the family name inside the declaration span:
-- @data family@ at the top level and @data@ inside a class body.
resolveDataFamilyDecl :: Text -> DataFamilyDecl -> ResolveM R.DataFamilyDecl
resolveDataFamilyDecl keyword familyDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan keyword declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (dataFamilyDeclHead familyDecl)
  kind' <- traverse resolveType (dataFamilyDeclKind familyDecl)
  pure (R.DataFamilyDecl head' kind')

resolveDataFamilyInst :: DataFamilyInst -> ResolveM R.DataFamilyInst
resolveDataFamilyInst familyInst = do
  scope <- currentScope
  (forallScope, forallBinders') <- bindTyVarBinders (dataFamilyInstForall familyInst)
  (head', kind', constructors', deriving') <-
    extendScope forallScope $
      (,,,)
        <$> resolveType (dataFamilyInstHead familyInst)
        <*> traverse resolveType (dataFamilyInstKind familyInst)
        <*> (mapM resolveDataConDecl (dataFamilyInstConstructors familyInst) >>= mapM (resolveDataConDefinitions scope (declarationFirstConstructor (dataFamilyInstConstructors familyInst))))
        <*> mapM resolveDerivingClause (dataFamilyInstDeriving familyInst)
  pure
    R.DataFamilyInst
      { R.dataFamilyInstIsNewtype = dataFamilyInstIsNewtype familyInst,
        R.dataFamilyInstForall = forallBinders',
        R.dataFamilyInstHead = head',
        R.dataFamilyInstKind = kind',
        R.dataFamilyInstConstructors = constructors',
        R.dataFamilyInstDeriving = deriving'
      }

resolveTypeSynDecl :: TypeSynDecl -> ResolveM R.TypeSynDecl
resolveTypeSynDecl typeSynDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "type " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (typeSynHead typeSynDecl)
  body' <- resolveType (typeSynBody typeSynDecl)
  pure (R.TypeSynDecl head' body')

resolveNewtypeDecl :: NewtypeDecl -> ResolveM R.NewtypeDecl
resolveNewtypeDecl newtypeDecl = do
  scope <- currentScope
  declSpan <- currentSpan
  let resolveHeadName name =
        let rendered = renderUnqualifiedName name
            span' = declKeywordNameSpan "newtype " declSpan rendered
         in resolveUnqualifiedNameTo span' ResolutionNamespaceType (lookupType rendered scope) name
  head' <- resolveBinderHeadKinds resolveHeadName (newtypeDeclHead newtypeDecl)
  kind' <- traverse resolveType (newtypeDeclKind newtypeDecl)
  constructor' <- traverse resolveDataConDecl (newtypeDeclConstructor newtypeDecl) >>= traverse (resolveDataConDefinitions scope (declarationFirstConstructor (maybeToList (newtypeDeclConstructor newtypeDecl))))
  deriving' <- mapM resolveDerivingClause (newtypeDeclDeriving newtypeDecl)
  pure
    R.NewtypeDecl
      { R.newtypeDeclCTypePragma = newtypeDeclCTypePragma newtypeDecl,
        R.newtypeDeclHead = head',
        R.newtypeDeclContext = map fromParserType (newtypeDeclContext newtypeDecl),
        R.newtypeDeclKind = kind',
        R.newtypeDeclConstructor = constructor',
        R.newtypeDeclDeriving = deriving'
      }

-- | Resolve the types of a constructor declaration. The constructor name
-- and the field names stay unresolved: 'resolveDataConDefinitions'
-- resolves them.
resolveDataConDecl :: DataConDecl -> ResolveM R.DataConDecl
resolveDataConDecl dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> R.DataConAnn ann <$> withPushedSpan ann (resolveDataConDecl inner)
    PrefixCon forallVars context name bangTypes -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.PrefixCon forallVars' <$> mapM resolveType context <*> pure (fromParserUnqualifiedName name) <*> mapM resolveBangType bangTypes
    InfixCon forallVars context lhs name rhs -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.InfixCon forallVars' <$> mapM resolveType context <*> resolveBangType lhs <*> pure (fromParserUnqualifiedName name) <*> resolveBangType rhs
    RecordCon forallVars context name fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.RecordCon forallVars' <$> mapM resolveType context <*> pure (fromParserUnqualifiedName name) <*> mapM resolveFieldDecl fields
    GadtCon telescopes context names body -> do
      (forallScope, telescopes') <- bindForallTelescopes telescopes
      extendScope forallScope $
        R.GadtCon telescopes' <$> mapM resolveType context <*> pure (map fromParserUnqualifiedName names) <*> resolveGadtBody body
    TupleCon forallVars context flavor fields -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.TupleCon forallVars' <$> mapM resolveType context <*> pure flavor <*> mapM resolveBangType fields
    UnboxedSumCon forallVars context pos arity field -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.UnboxedSumCon forallVars' <$> mapM resolveType context <*> pure pos <*> pure arity <*> resolveBangType field
    ListCon forallVars context -> do
      (forallScope, forallVars') <- bindTyVarBinders forallVars
      extendScope forallScope $
        R.ListCon forallVars' <$> mapM resolveType context

resolveBangType :: BangType -> ResolveM R.BangType
resolveBangType bt = do
  ty' <- resolveType (bangType bt)
  pure
    R.BangType
      { R.bangAnns = bangAnns bt,
        R.bangPragmas = bangPragmas bt,
        R.bangStrict = bangStrict bt,
        R.bangLazy = bangLazy bt,
        R.bangType = ty'
      }

-- | Resolve the type of a field declaration. The multiplicity and the
-- field names stay unresolved.
resolveFieldDecl :: FieldDecl -> ResolveM R.FieldDecl
resolveFieldDecl fieldDecl = do
  fieldType' <- resolveBangType (fieldType fieldDecl)
  pure
    R.FieldDecl
      { R.fieldAnns = fieldAnns fieldDecl,
        R.fieldNames = map fromParserUnqualifiedName (fieldNames fieldDecl),
        R.fieldMultiplicity = fromParserType <$> fieldMultiplicity fieldDecl,
        R.fieldType = fieldType'
      }

-- | Bind the type variables of a chain of @forall@ telescopes.
-- Each telescope is in the scope of the telescopes before it.
bindForallTelescopes :: [ForallTelescope] -> ResolveM (Scope, [R.ForallTelescope])
bindForallTelescopes = foldM step (emptyScope, [])
  where
    step (scope, acc) telescope = do
      (binderScope, binders') <- extendScope scope (bindTyVarBinders (forallTelescopeBinders telescope))
      pure (binderScope `unionScope` scope, acc <> [R.ForallTelescope (forallTelescopeVisibility telescope) binders'])

resolveGadtBody :: GadtBody -> ResolveM R.GadtBody
resolveGadtBody body =
  case body of
    GadtPrefixBody bangTypes ty ->
      R.GadtPrefixBody <$> mapM resolveBangTypePair bangTypes <*> resolveType ty
    GadtRecordBody fields ty ->
      R.GadtRecordBody <$> mapM resolveFieldDecl fields <*> resolveType ty
  where
    resolveBangTypePair (bt, arrowKind) = do
      bt' <- resolveBangType bt
      pure (bt', fromParserArrowKind arrowKind)

resolveType :: Type -> ResolveM R.Type
resolveType ty =
  case ty of
    TAnn ann inner -> withPushedSpan ann (resolveType inner)
    TVar name ->
      R.TVar <$> resolveScopedTypeVariableUse name
    TCon name promoted ->
      R.TCon <$> resolveTypeConstructorUse promoted name <*> pure promoted
    TBuiltinCon {} -> pure (fromParserType ty)
    TImplicitParam name inner ->
      R.TImplicitParam name <$> resolveType inner
    TTypeLit {} -> pure (fromParserType ty)
    TStar {} -> pure (fromParserType ty)
    TForall telescope inner -> do
      (binderScope, binders') <- withResetLocalSupply (bindTyVarBinders (forallTelescopeBinders telescope))
      inner' <- extendScope binderScope (resolveType inner)
      pure (R.TForall (R.ForallTelescope (forallTelescopeVisibility telescope) binders') inner')
    TApp left right ->
      R.TApp <$> resolveType left <*> resolveType right
    TTypeApp left right ->
      R.TTypeApp <$> resolveType left <*> resolveType right
    TInfix {} -> resolveInfixType ty
    TFun arrowKind left right ->
      R.TFun <$> resolveArrowKind arrowKind <*> resolveType left <*> resolveType right
    TTuple flavor promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      syntaxResolution <- resolution sp (IdentifierTuple flavor (length items)) (typePromotionNamespace promotion) (Found EntitySyntax)
      pure (annotateTypeSyntax sp syntaxResolution (R.TTuple flavor promotion items'))
    TUnboxedSum items ->
      R.TUnboxedSum <$> mapM resolveType items
    TList promotion items -> do
      items' <- mapM resolveType items
      sp <- currentSpan
      syntaxResolution <- resolution sp IdentifierList (typePromotionNamespace promotion) (Found EntitySyntax)
      pure (annotateTypeSyntax sp syntaxResolution (R.TList promotion items'))
    TParen inner ->
      R.TParen <$> resolveType inner
    TKindSig inner kind ->
      R.TKindSig <$> resolveType inner <*> resolveType kind
    TContext constraints inner ->
      R.TContext <$> mapM resolveType constraints <*> resolveType inner
    TSplice expr ->
      R.TSplice <$> withExpressionQuote False (withResetLocalSupply (resolveExpr expr))
    TWildcard -> pure R.TWildcard
    TQuasiQuote {} -> R.TResolved . ResolutionError <$> unhandledSyntax ResolutionNamespaceType "TQuasiQuote" <*> pure (fromParserType ty)

resolveArrowKind :: ArrowKind -> ResolveM R.ArrowKind
resolveArrowKind arrowKind =
  case arrowKind of
    ArrowUnrestricted -> pure R.ArrowUnrestricted
    ArrowLinear -> pure R.ArrowLinear
    ArrowExplicit ty -> R.ArrowExplicit <$> resolveType ty

resolveTypeSignature :: Type -> ResolveM (Scope, R.Type)
resolveTypeSignature ty =
  case ty of
    -- Type signatures may carry span-only 'TAnn' wrappers (see 'typeAnnSpan'); peel
    -- them so we still allocate scoped type variables and advance 'nextLocal'.
    TAnn ann sub -> withPushedSpan ann (resolveTypeSignature sub)
    TForall telescope inner -> do
      (binderScope, binders') <- bindTyVarBinders (forallTelescopeBinders telescope)
      inner' <- extendScope binderScope (resolveType inner)
      pure (binderScope, R.TForall (R.ForallTelescope (forallTelescopeVisibility telescope) binders') inner')
    _ -> do
      ty' <- resolveType ty
      pure (emptyScope, ty')

annotateTypeSyntax :: Maybe SourceSpan -> ResolutionResult -> R.Type -> R.Type
annotateTypeSyntax sp syntaxResolution =
  R.TResolved syntaxResolution . maybe id (R.TAnn . mkAnnotation) sp

bindTyVarBinders :: [TyVarBinder] -> ResolveM (Scope, [R.TyVarBinder])
bindTyVarBinders =
  foldM step (emptyScope, [])
  where
    step (boundScope, acc) binder = do
      binder' <- extendScope boundScope (resolveTyVarBinderKind binder)
      entity <- freshLocal
      let boundScope' = insertType (tyVarBinderName binder) entity boundScope
      pure (boundScope', acc <> [binder'])

allocateLocalDeclBinders :: [Decl] -> ResolveM (Map.Map Text Entity, Scope)
allocateLocalDeclBinders decls = do
  fieldScope <- currentScope
  (targets, scope) <- foldM (step fieldScope) (Map.empty, emptyScope) decls
  -- A local binder hides the fixity of an outer operator with the same
  -- name. Thus, each local binder gets its local fixity or the default.
  let declared = Map.unions (map localDeclFixities decls)
      fixities = Map.union declared (Map.map (const defaultOperatorFixity) targets)
  pure (targets, scope {scopeFixities = Map.restrictKeys fixities (Map.keysSet targets)})
  where
    step fieldScope acc decl = foldM addBinder acc (declBinderCandidates fieldScope decl)
    addBinder (targets, scope) (_, name) = do
      entity <- freshLocal
      let key = renderUnqualifiedName name
      pure (Map.insert key entity targets, insertTerm key entity scope)

-- | The term operator fixities that a local declaration group declares.
localDeclFixities :: Decl -> Map.Map Text OperatorFixity
localDeclFixities decl =
  case snd (peelDeclSpan decl) of
    DeclFixity assoc mNamespace mPrec ops
      | mNamespace /= Just IEEntityNamespaceType ->
          Map.fromList [(renderUnqualifiedName op, OperatorFixity assoc (fromMaybe 9 mPrec)) | op <- ops]
    _ -> Map.empty

-- | Collect all term binders introduced by a declaration (handles tuple patterns etc.)
declBinderCandidates :: Scope -> Decl -> [(Maybe SourceSpan, UnqualifiedName)]
declBinderCandidates fieldScope decl =
  let (outerSp, innerDecl) = peelDeclSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind name _ ->
              let loc = outerSp
               in [(spanStartNameSpan loc (renderUnqualifiedName name), name)]
            PatternBind _ pat _ ->
              let loc = peelPatternSpan pat <|> outerSp
               in collectPatVarBinders fieldScope loc pat
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

resolveTermDefinitionAt :: Maybe SourceSpan -> TermDefinition -> UnqualifiedName -> ResolveM R.UnqualifiedName
resolveTermDefinitionAt span' termDefinition name =
  case termDefinition name of
    Just resolved ->
      resolveUnqualifiedNameTo (spanStartNameSpan span' (renderUnqualifiedName name)) ResolutionNamespaceTerm resolved name
    Nothing -> pure (fromParserUnqualifiedName name)

resolveUnqualifiedNameTo :: Maybe SourceSpan -> ResolutionNamespace -> Resolution -> UnqualifiedName -> ResolveM R.UnqualifiedName
resolveUnqualifiedNameTo span' namespace resolved name =
  withResolution span' (IdentifierNamed (renderUnqualifiedName name)) namespace resolved $
    \result -> R.UnqualifiedName (unqualifiedNameType name) (unqualifiedNameText name) (unqualifiedNameAnns name) (Just result)

resolveNameTo :: Maybe SourceSpan -> ResolutionNamespace -> Resolution -> Name -> ResolveM R.Name
resolveNameTo span' namespace resolved name =
  withResolution span' (IdentifierNamed (nameText name)) namespace resolved $
    \result -> nameWithResult name result

-- | A name occurrence with its resolution result.
nameWithResult :: Name -> ResolutionResult -> R.Name
nameWithResult name result =
  R.Name (nameQualifier name) (nameType name) (nameText name) (nameAnns name) (Just result)

resolveTermUse :: Name -> ResolveM R.Name
resolveTermUse name = do
  sp <- currentSpan
  scope <- currentScope
  resolveNameTo sp ResolutionNamespaceTerm (termUseResolution scope name) name

-- | The resolution of a term occurrence: the term of its 'ExactTerm' mark,
-- or the term that the scope gives for its name.
termUseResolution :: Scope -> Name -> Resolution
termUseResolution scope name =
  maybe (resolveTermName scope name) (Found . EntityGlobal) (exactTerm name)

-- | Resolve a term name that has its own place in the source, such as an
-- infix operator. The parser gives the name the span of its token. A name
-- without a span takes the start of the enclosing syntax.
resolveTermUseAtName :: Name -> ResolveM R.Name
resolveTermUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  let nameSpan = sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan sp (nameText name)
  resolveNameTo nameSpan ResolutionNamespaceTerm (termUseResolution scope name) name

resolveInfixExpr :: Expr -> ResolveM R.Expr
resolveInfixExpr expr = do
  operands <- traverse resolveExpr (flattenInfixExpr expr)
  resolveInfixChain R.EInfix operands

-- | Check fixities before resolving the operators. Each operator gets one
-- resolution. An ambiguous chain keeps its left-nested tree and marks the
-- selected operator.
resolveInfixChain :: (a -> R.Name -> a -> a) -> InfixChain Name a -> ResolveM a
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

ambiguousFixityName :: Maybe SourceSpan -> Name -> ResolveM R.Name
ambiguousFixityName ambient name = do
  result <-
    resolution
      (sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan ambient (nameText name))
      (IdentifierNamed (nameText name))
      ResolutionNamespaceTerm
      (NotFound "ambiguous fixity")
  pure (nameWithResult name result)

-- | Apply operator fixities to a type chain and keep each promotion.
resolveInfixType :: Type -> ResolveM R.Type
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
    build left (name, promotion) = R.TInfix left name promotion
    resolveOperator operator = do
      let (name, promotion) = resolvedInfixName operator
      resolved <-
        if isTypeVariableOperator promotion name
          then resolveTypeVariableOperator name
          else resolveTypeConstructorUse promotion name
      pure operator {resolvedInfixName = (resolved, promotion)}
    -- A type variable in backticks, as in a `k` b, is a type variable use.
    -- Its binder can be implicit. Thus, an unknown name is not an error.
    isTypeVariableOperator promotion name =
      promotion == Unpromoted && isNothing (nameQualifier name) && nameType name == NameVarId
    resolveTypeVariableOperator name = do
      sp <- currentSpan
      scope <- currentScope
      case lookupType (nameText name) scope of
        NotFound _ -> pure (fromParserName name)
        resolved -> resolveNameTo sp ResolutionNamespaceType resolved name
    resolveAmbiguous ambient ambiguous operator
      | resolvedInfixIndex operator == resolvedInfixIndex ambiguous = do
          let (name, promotion) = resolvedInfixName operator
          result <-
            resolution
              (sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan ambient (nameText name))
              (IdentifierNamed (nameText name))
              (typePromotionNamespace promotion)
              (NotFound "ambiguous fixity")
          pure (nameWithResult name result, promotion)
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

resolveTypeConstructorUse :: TypePromotion -> Name -> ResolveM R.Name
resolveTypeConstructorUse promotion name =
  case promotion of
    Unpromoted -> do
      sp <- currentSpan
      scope <- currentScope
      info <- currentModuleInfo
      case maybe (resolveTypeName scope name) (Found . EntityGlobal) (exactType name) of
        unresolved@(NotFound _)
          | DataKinds `elem` moduleInfoExtensions info ->
              case termUseResolution scope name of
                NotFound _ -> resolveNameTo sp ResolutionNamespaceType unresolved name
                resolved -> resolveNameTo sp ResolutionNamespaceTerm resolved name
        resolved -> resolveNameTo sp ResolutionNamespaceType resolved name
    -- A promoted constructor of a splice result has an 'ExactTerm' mark.
    Promoted -> do
      sp <- currentSpan
      scope <- currentScope
      resolveNameTo sp ResolutionNamespaceTerm (termUseResolution scope name) name

typePromotionNamespace :: TypePromotion -> ResolutionNamespace
typePromotionNamespace promotion =
  case promotion of
    Unpromoted -> ResolutionNamespaceType
    Promoted -> ResolutionNamespaceTerm

resolveTypeUseAtName :: Name -> ResolveM R.Name
resolveTypeUseAtName name = do
  sp <- currentSpan
  scope <- currentScope
  let nameSpan = sourceSpanFromAnns (nameAnns name) <|> spanStartNameSpan sp (nameText name)
  resolveNameTo nameSpan ResolutionNamespaceType (resolveTypeName scope name) name

resolveScopedTypeVariableUse :: UnqualifiedName -> ResolveM R.UnqualifiedName
resolveScopedTypeVariableUse name = do
  sp <- currentSpan
  scope <- currentScope
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
  -- A type variable the scope does not know is left alone: the binder that
  -- would give it a meaning may be implicit, so this is no error.
  case resolved of
    NotFound _ -> pure (fromParserUnqualifiedName name)
    _ -> resolveUnqualifiedNameTo sp ResolutionNamespaceType resolved name

-- | The selector name of each record field label of one declaration. A
-- duplicate record field has a selector with its own name, which the
-- resolver puts on the field binder. Any other label is its own selector
-- name.
--
-- The declarations are parser syntax with the resolutions in annotations,
-- as the type checker reads them.
recordSelectorNames :: [DataConDecl] -> Map.Map Text Text
recordSelectorNames constructors =
  Map.fromList
    [ (unqualifiedNameText name, selector)
    | constructor <- constructors,
      fields <- recordFields constructor,
      field <- fields,
      name <- fieldNames field,
      let selector =
            case binderEntity name of
              Just (EntityGlobal global) -> globalNameText global
              _ -> unqualifiedNameText name
    ]
  where
    recordFields constructor =
      case constructor of
        DataConAnn _ inner -> recordFields inner
        RecordCon _ _ _ fields -> [fields]
        GadtCon _ _ _ (GadtRecordBody fields _) -> [fields]
        _ -> []

-- | Resolve the constructor names and the field selectors of a constructor
-- declaration, after 'resolveDataConDecl' resolved its types.
resolveDataConDefinitions :: Scope -> Maybe Text -> R.DataConDecl -> ResolveM R.DataConDecl
resolveDataConDefinitions scope firstConstructor =
  go Nothing
  where
    go ambient current =
      case current of
        R.DataConAnn ann inner -> R.DataConAnn ann <$> go (pushSpanFromAnn ambient ann) inner
        R.DataConResolved result inner -> R.DataConResolved result <$> go ambient inner
        R.PrefixCon forallVars context name bangTypes ->
          (\name' -> R.PrefixCon forallVars context name' bangTypes) <$> resolveConstructor ambient name
        R.RecordCon forallVars context name fields ->
          R.RecordCon forallVars context <$> resolveConstructor ambient name <*> pure (map resolveFieldSelectors fields)
        R.InfixCon forallVars context lhs name rhs ->
          (\name' -> R.InfixCon forallVars context lhs name' rhs) <$> resolveConstructor ambient name
        R.GadtCon forallVars context names body ->
          (\names' -> R.GadtCon forallVars context names' (resolveGadtFieldSelectors body)) <$> mapM (resolveConstructor ambient) names
        R.TupleCon {} -> pure current
        R.UnboxedSumCon {} -> pure current
        R.ListCon {} -> do
          result <- resolution ambient IdentifierList ResolutionNamespaceTerm (Found EntitySyntax)
          pure (R.DataConResolved result current)

    resolveConstructor span' name =
      let rendered = R.renderUnqualifiedName name
       in withResolution
            (spanStartNameSpan span' rendered)
            (IdentifierNamed rendered)
            ResolutionNamespaceTerm
            (lookupTerm rendered scope)
            (\result -> name {R.unqualifiedNameResolution = Just result})

    resolveGadtFieldSelectors body =
      case body of
        R.GadtRecordBody fields result -> R.GadtRecordBody (map resolveFieldSelectors fields) result
        _ -> body

    -- A duplicate record field has a selector with its own name. The field
    -- binder gets that selector, so the type checker and the desugarer use
    -- the same name. Other field binders keep no resolution.
    resolveFieldSelectors field =
      field {R.fieldNames = map (resolveFieldSelector (sourceSpanFromAnns (R.fieldAnns field))) (R.fieldNames field)}

    resolveFieldSelector span' name =
      let rendered = R.renderUnqualifiedName name
       in case firstConstructor >>= \constructor -> Map.lookup (fieldSelectorName rendered constructor) (scopeTerms scope) of
            Just entity ->
              name {R.unqualifiedNameResolution = Just (ResolutionOk (Resolved (spanStartNameSpan span' rendered) (IdentifierNamed rendered) ResolutionNamespaceTerm entity))}
            Nothing -> name
