{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Resolve
  ( pattern DeclResolution,
    pattern EResolution,
    pattern PResolution,
    pattern TResolution,
    resolve,
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
  ( BangType (..),
    ClassDecl (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr (..),
    FieldDecl (..),
    GadtBody (..),
    GuardQualifier (..),
    GuardedRhs (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TyVarBinder (..),
    Type (..),
    UnqualifiedName,
    ValueDecl (..),
    mkAnnotation,
    mkQualifiedName,
    mkUnqualifiedName,
    moduleName,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Types
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName
  }

type ModuleExports = Map.Map Text Scope

resolve :: [Module] -> ResolveResult
resolve modules =
  ResolveResult
    { resolvedModules = modules',
      resolvedAnnotations = extraAnnotations,
      resolveErrors = []
    }
  where
    step currentNextLocal modu =
      let (nextLocal', annotations, modu') = resolveModule exports currentNextLocal modu
       in (nextLocal', (annotations, modu'))
    (_, resolved) = mapAccumL step 0 modules
    modules' = map snd resolved
    extraAnnotations = map (\(annotations, modu) -> (moduleKey modu, annotations)) resolved
    exports = collectModuleExports modules

resolveModule :: ModuleExports -> Int -> Module -> (Int, [ResolutionAnnotation], Module)
resolveModule exports nextLocal modu =
  let scope = moduleScope exports modu
      (nextLocal', resolvedDecls) = resolveTopLevelDecls scope nextLocal Map.empty (moduleDecls modu)
      decls' = map snd resolvedDecls
      annotations = concatMap fst resolvedDecls
   in (nextLocal', annotations, modu {moduleDecls = decls'})

resolveTopLevelDecls :: Scope -> Int -> Map.Map Text Scope -> [Decl] -> (Int, [([ResolutionAnnotation], Decl)])
resolveTopLevelDecls _ nextLocal _ [] = (nextLocal, [])
resolveTopLevelDecls scope nextLocal signatureScopes (decl : rest) =
  let (nextLocal', signatureScopes', resolvedDecl) = resolveTopLevelDecl scope nextLocal signatureScopes decl
      (nextLocal'', resolvedDecls) = resolveTopLevelDecls scope nextLocal' signatureScopes' rest
   in (nextLocal'', resolvedDecl : resolvedDecls)

resolveTopLevelDecl :: Scope -> Int -> Map.Map Text Scope -> Decl -> (Int, Map.Map Text Scope, ([ResolutionAnnotation], Decl))
resolveTopLevelDecl scope nextLocal signatureScopes decl =
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
      (nextLocal', signatureScopes', decl') = resolveDeclWithSignatureScope scoped nextLocal signatureScopes decl
      extras = topLevelDeclAnnotations decl scope
      decl'' = maybe decl' (`annotateDecl` decl') (topLevelBinderAnnotation decl scope)
   in (nextLocal', signatureScopes', (extras, decl''))

resolveDeclWithSignatureScope :: Scope -> Int -> Map.Map Text Scope -> Decl -> (Int, Map.Map Text Scope, Decl)
resolveDeclWithSignatureScope scope nextLocal signatureScopes decl =
  case decl of
    DeclTypeSig span' names ty ->
      let (nextLocal', binderScope, ty') = resolveTypeSignature scope nextLocal ty
          signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
       in (nextLocal', signatureScopes', DeclTypeSig span' names ty')
    _ ->
      let (nextLocal', decl') = resolveDecl scope nextLocal decl
          signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
       in (nextLocal', signatureScopes', decl')

resolveDecl :: Scope -> Int -> Decl -> (Int, Decl)
resolveDecl scope nextLocal decl =
  case decl of
    DeclAnn _ inner -> resolveDecl scope nextLocal inner
    DeclValue span' valueDecl ->
      case valueDecl of
        FunctionBind bindSpan name matches ->
          let (nextLocal', matches') = mapAccumL (resolveMatch scope) nextLocal matches
           in (nextLocal', DeclValue span' (FunctionBind bindSpan name matches'))
        PatternBind bindSpan pat rhs ->
          let (nextLocal', rhs') = resolveRhs scope nextLocal rhs
           in (nextLocal', DeclValue span' (PatternBind bindSpan pat rhs'))
    DeclTypeSig span' names ty ->
      let ty' = resolveType scope ty
       in (nextLocal, DeclTypeSig span' names ty')
    DeclTypeData span' dataDecl ->
      (nextLocal, DeclTypeData span' (resolveDataDecl scope dataDecl))
    DeclData span' dataDecl ->
      (nextLocal, DeclData span' (resolveDataDecl scope dataDecl))
    DeclSplice span' expr ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
       in (nextLocal', DeclSplice span' expr')
    DeclNewtype span' newtypeDecl ->
      ( nextLocal,
        DeclNewtype
          span'
          ( newtypeDecl
              { newtypeDeclKind = fmap (resolveType scope) (newtypeDeclKind newtypeDecl),
                newtypeDeclConstructor = fmap (resolveDataConDecl scope) (newtypeDeclConstructor newtypeDecl)
              }
          )
      )
    _ -> (nextLocal, decl)

resolveMatch :: Scope -> Int -> Match -> (Int, Match)
resolveMatch scope nextLocal match =
  let (nextLocal', patScope, pats') = bindPatterns scope nextLocal (matchPats match)
      scoped = unionScope patScope scope
      (nextLocal'', rhs') = resolveRhs scoped nextLocal' (matchRhs match)
   in (nextLocal'', match {matchPats = pats', matchRhs = rhs'})

resolveRhs :: Scope -> Int -> Rhs -> (Int, Rhs)
resolveRhs scope nextLocal rhs =
  case rhs of
    UnguardedRhs span' expr mDecls ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
          (nextLocal'', mDecls') = resolveWhereDecls scope nextLocal' mDecls
       in (nextLocal'', UnguardedRhs span' expr' mDecls')
    GuardedRhss span' guardedRhss mDecls ->
      let (nextLocal', guardedRhss') = mapAccumL (resolveGuardedRhs scope) nextLocal guardedRhss
          (nextLocal'', mDecls') = resolveWhereDecls scope nextLocal' mDecls
       in (nextLocal'', GuardedRhss span' guardedRhss' mDecls')

resolveWhereDecls :: Scope -> Int -> Maybe [Decl] -> (Int, Maybe [Decl])
resolveWhereDecls _ nextLocal Nothing = (nextLocal, Nothing)
resolveWhereDecls scope nextLocal (Just decls) =
  let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
      scoped = unionScope localScope scope
      (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
   in (nextLocal'', Just decls')

resolveGuardedRhs :: Scope -> Int -> GuardedRhs -> (Int, GuardedRhs)
resolveGuardedRhs scope nextLocal guardedRhs =
  let (nextLocal', scope', guards') = resolveGuardQualifiers scope nextLocal (guardedRhsGuards guardedRhs)
      (nextLocal'', body') = resolveExpr scope' nextLocal' (guardedRhsBody guardedRhs)
   in (nextLocal'', guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'})

resolveGuardQualifiers :: Scope -> Int -> [GuardQualifier] -> (Int, Scope, [GuardQualifier])
resolveGuardQualifiers scope nextLocal qualifiers =
  case qualifiers of
    [] -> (nextLocal, scope, [])
    qualifier : rest ->
      let (nextLocal', scope', qualifier') = resolveGuardQualifier scope nextLocal qualifier
          (nextLocal'', scope'', rest') = resolveGuardQualifiers scope' nextLocal' rest
       in (nextLocal'', scope'', qualifier' : rest')

resolveGuardQualifier :: Scope -> Int -> GuardQualifier -> (Int, Scope, GuardQualifier)
resolveGuardQualifier scope nextLocal qualifier =
  case qualifier of
    GuardExpr span' expr ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
       in (nextLocal', scope, GuardExpr span' expr')
    GuardPat span' pat expr ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
          (nextLocal'', patScope, pat') = bindPattern scope nextLocal' pat
       in (nextLocal'', unionScope patScope scope, GuardPat span' pat' expr')
    GuardLet span' decls ->
      let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
          scoped = unionScope localScope scope
          (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
       in (nextLocal'', scoped, GuardLet span' decls')

resolveExpr :: Scope -> Int -> Expr -> (Int, Expr)
resolveExpr scope nextLocal expr =
  case expr of
    EAnn _ inner -> resolveExpr scope nextLocal inner
    EVar span' name ->
      ( nextLocal,
        annotateExpr
          (ResolutionAnnotation span' (nameText name) ResolutionNamespaceTerm (resolveTermName scope name))
          (EVar span' name)
      )
    EIf span' cond trueBranch falseBranch ->
      let (nextLocal', cond') = resolveExpr scope nextLocal cond
          (nextLocal'', trueBranch') = resolveExpr scope nextLocal' trueBranch
          (nextLocal''', falseBranch') = resolveExpr scope nextLocal'' falseBranch
       in (nextLocal''', EIf span' cond' trueBranch' falseBranch')
    EInfix span' left op right ->
      let (nextLocal', left') = resolveExpr scope nextLocal left
          (nextLocal'', right') = resolveExpr scope nextLocal' right
       in (nextLocal'', EInfix span' left' op right')
    ENegate span' inner ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', ENegate span' inner')
    ESectionL span' inner op ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', ESectionL span' inner' op)
    ESectionR span' op inner ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', ESectionR span' op inner')
    ELetDecls span' decls body ->
      let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
          scoped = unionScope localScope scope
          (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
          (nextLocal''', body') = resolveExpr scoped nextLocal'' body
       in (nextLocal''', ELetDecls span' decls' body')
    ETypeSig span' inner ty ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', ETypeSig span' inner' (resolveType scope ty))
    EParen span' inner ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', EParen span' inner')
    ETypeApp span' fun ty ->
      let (nextLocal', fun') = resolveExpr scope nextLocal fun
       in (nextLocal', ETypeApp span' fun' ty)
    EApp span' fun arg ->
      let (nextLocal', fun') = resolveExpr scope nextLocal fun
          (nextLocal'', arg') = resolveExpr scope nextLocal' arg
       in (nextLocal'', EApp span' fun' arg')
    _ -> (nextLocal, expr)

resolveBoundDecl :: Scope -> Map.Map Text ResolutionAnnotation -> Int -> Decl -> (Int, Decl)
resolveBoundDecl scope binderAnnotations nextLocal decl =
  let (nextLocal', decl') = resolveDecl scope nextLocal decl
   in (nextLocal', maybe decl' (`annotateDecl` decl') (declBinderAnnotation decl binderAnnotations))

resolveBoundDecls :: Scope -> Map.Map Text ResolutionAnnotation -> Int -> Map.Map Text Scope -> [Decl] -> (Int, [Decl])
resolveBoundDecls _ _ nextLocal _ [] = (nextLocal, [])
resolveBoundDecls scope binderAnnotations nextLocal signatureScopes (decl : rest) =
  let scoped = maybe scope (`unionScope` scope) (declSignatureScope decl signatureScopes)
      (nextLocal', signatureScopes', decl') = resolveBoundDeclWithSignatureScope scoped binderAnnotations nextLocal signatureScopes decl
      (nextLocal'', decls') = resolveBoundDecls scope binderAnnotations nextLocal' signatureScopes' rest
   in (nextLocal'', decl' : decls')

resolveBoundDeclWithSignatureScope :: Scope -> Map.Map Text ResolutionAnnotation -> Int -> Map.Map Text Scope -> Decl -> (Int, Map.Map Text Scope, Decl)
resolveBoundDeclWithSignatureScope scope binderAnnotations nextLocal signatureScopes decl =
  case decl of
    DeclTypeSig span' names ty ->
      let (nextLocal', binderScope, ty') = resolveTypeSignature scope nextLocal ty
          signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
          resolvedDecl = DeclTypeSig span' names ty'
          annotatedDecl = maybe resolvedDecl (`annotateDecl` resolvedDecl) (declBinderAnnotation decl binderAnnotations)
       in (nextLocal', signatureScopes', annotatedDecl)
    _ ->
      let (nextLocal', decl') = resolveBoundDecl scope binderAnnotations nextLocal decl
          signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
       in (nextLocal', signatureScopes', decl')

declSignatureScope :: Decl -> Map.Map Text Scope -> Maybe Scope
declSignatureScope decl signatureScopes =
  case declBinderCandidate decl of
    Just (_, name) -> Map.lookup (renderUnqualifiedName name) signatureScopes
    Nothing -> Nothing

bindPatterns :: Scope -> Int -> [Pattern] -> (Int, Scope, [Pattern])
bindPatterns typeScope nextLocal pats =
  let (nextLocal', scopedEntries, pats') = foldl' step (nextLocal, [], []) pats
   in (nextLocal', Scope (Map.fromList scopedEntries) Map.empty, reverse pats')
  where
    step (currentId, entries, acc) pat =
      let (nextId, scope, pat') = bindPattern typeScope currentId pat
       in (nextId, Map.toList (scopeTerms scope) <> entries, pat' : acc)

bindPattern :: Scope -> Int -> Pattern -> (Int, Scope, Pattern)
bindPattern typeScope nextLocal pat =
  case pat of
    PAnn _ inner -> bindPattern typeScope nextLocal inner
    PVar span' name ->
      let resolvedName = ResolvedLocal nextLocal name
          annotation = ResolutionAnnotation span' (renderUnqualifiedName name) ResolutionNamespaceTerm resolvedName
       in (nextLocal + 1, Scope (Map.singleton (renderUnqualifiedName name) resolvedName) Map.empty, annotatePattern annotation (PVar span' name))
    PTuple span' flavor pats ->
      let (nextLocal', scope, pats') = bindPatterns typeScope nextLocal pats
       in (nextLocal', scope, PTuple span' flavor pats')
    PList span' pats ->
      let (nextLocal', scope, pats') = bindPatterns typeScope nextLocal pats
       in (nextLocal', scope, PList span' pats')
    PCon span' name pats ->
      let (nextLocal', scope, pats') = bindPatterns typeScope nextLocal pats
       in (nextLocal', scope, PCon span' name pats')
    PInfix span' left name right ->
      let (nextLocal', leftScope, left') = bindPattern typeScope nextLocal left
          (nextLocal'', rightScope, right') = bindPattern typeScope nextLocal' right
       in (nextLocal'', unionScope rightScope leftScope, PInfix span' left' name right')
    PView span' expr inner ->
      let (nextLocal', scope, inner') = bindPattern typeScope nextLocal inner
       in (nextLocal', scope, PView span' expr inner')
    PAs span' alias inner ->
      let aliasName = mkUnqualifiedName NameVarId alias
          aliasResolved = ResolvedLocal nextLocal aliasName
          aliasAnnotation = ResolutionAnnotation (spanStartNameSpan span' alias) alias ResolutionNamespaceTerm aliasResolved
          (nextLocal', innerScope, inner') = bindPattern typeScope (nextLocal + 1) inner
          aliasScope = Scope (Map.singleton alias aliasResolved) Map.empty
       in (nextLocal', unionScope innerScope aliasScope, annotatePattern aliasAnnotation (PAs span' alias inner'))
    PStrict span' inner ->
      let (nextLocal', scope, inner') = bindPattern typeScope nextLocal inner
       in (nextLocal', scope, PStrict span' inner')
    PIrrefutable span' inner ->
      let (nextLocal', scope, inner') = bindPattern typeScope nextLocal inner
       in (nextLocal', scope, PIrrefutable span' inner')
    PParen span' inner ->
      let (nextLocal', scope, inner') = bindPattern typeScope nextLocal inner
       in (nextLocal', scope, PParen span' inner')
    PRecord span' name fields wildcard ->
      let (nextLocal', entries, fields') =
            foldl'
              ( \(currentId, currentEntries, acc) (fieldName, fieldPat) ->
                  let (nextId, fieldScope, fieldPat') = bindPattern typeScope currentId fieldPat
                   in (nextId, Map.toList (scopeTerms fieldScope) <> currentEntries, (fieldName, fieldPat') : acc)
              )
              (nextLocal, [], [])
              fields
       in (nextLocal', Scope (Map.fromList entries) Map.empty, PRecord span' name (reverse fields') wildcard)
    PTypeSig span' inner ty ->
      let (nextLocal', scope, inner') = bindPattern typeScope nextLocal inner
       in (nextLocal', scope, PTypeSig span' inner' (resolveType typeScope ty))
    _ -> (nextLocal, emptyScope, pat)

resolveDataDecl :: Scope -> DataDecl -> DataDecl
resolveDataDecl scope dataDecl =
  dataDecl
    { dataDeclContext = map (resolveType scope) (dataDeclContext dataDecl),
      dataDeclKind = fmap (resolveType scope) (dataDeclKind dataDecl),
      dataDeclConstructors = map (resolveDataConDecl scope) (dataDeclConstructors dataDecl)
    }

resolveDataConDecl :: Scope -> DataConDecl -> DataConDecl
resolveDataConDecl scope dataConDecl =
  case dataConDecl of
    PrefixCon span' forallVars context name bangTypes ->
      PrefixCon span' forallVars (map (resolveType scope) context) name (map resolveBangType bangTypes)
    InfixCon span' forallVars context lhs name rhs ->
      InfixCon span' forallVars (map (resolveType scope) context) (resolveBangType lhs) name (resolveBangType rhs)
    RecordCon span' forallVars context name fields ->
      RecordCon span' forallVars (map (resolveType scope) context) name (map resolveFieldDecl fields)
    GadtCon span' forallVars context names body ->
      GadtCon span' forallVars (map (resolveType scope) context) names (resolveGadtBody scope body)
  where
    resolveBangType bt = bt {bangType = resolveType scope (bangType bt)}
    resolveFieldDecl fieldDecl = fieldDecl {fieldType = resolveBangType (fieldType fieldDecl)}

resolveGadtBody :: Scope -> GadtBody -> GadtBody
resolveGadtBody scope body =
  case body of
    GadtPrefixBody bangTypes ty ->
      GadtPrefixBody (map resolveBangType bangTypes) (resolveType scope ty)
    GadtRecordBody fields ty ->
      GadtRecordBody (map resolveFieldDecl fields) (resolveType scope ty)
  where
    resolveBangType bt = bt {bangType = resolveType scope (bangType bt)}
    resolveFieldDecl fieldDecl = fieldDecl {fieldType = resolveBangType (fieldType fieldDecl)}

resolveType :: Scope -> Type -> Type
resolveType scope ty =
  case ty of
    TAnn _ inner -> resolveType scope inner
    TVar span' name ->
      let resolvedTyVar = TVar span' name
       in maybe resolvedTyVar (`annotateType` resolvedTyVar) (resolveScopedTypeVarAnnotation scope span' name)
    TCon span' name promoted ->
      annotateType
        (ResolutionAnnotation span' (nameText name) ResolutionNamespaceType (resolveTypeName scope name))
        (TCon span' name promoted)
    TImplicitParam span' name inner ->
      TImplicitParam span' name (resolveType scope inner)
    TForall span' binders inner ->
      let (binderScope, binders') = bindTyVarBinders scope binders
       in TForall span' binders' (resolveType (unionScope binderScope scope) inner)
    TApp span' left right ->
      TApp span' (resolveType scope left) (resolveType scope right)
    TFun span' left right ->
      TFun span' (resolveType scope left) (resolveType scope right)
    TTuple span' flavor promoted items ->
      TTuple span' flavor promoted (map (resolveType scope) items)
    TUnboxedSum span' items ->
      TUnboxedSum span' (map (resolveType scope) items)
    TList span' promoted items ->
      TList span' promoted (map (resolveType scope) items)
    TParen span' inner ->
      TParen span' (resolveType scope inner)
    TKindSig span' inner kind ->
      TKindSig span' (resolveType scope inner) (resolveType scope kind)
    TContext span' constraints inner ->
      TContext span' (map (resolveType scope) constraints) (resolveType scope inner)
    TSplice span' expr ->
      TSplice span' (snd (resolveExpr scope 0 expr))
    _ -> ty

resolveScopedTypeVarAnnotation :: Scope -> SourceSpan -> UnqualifiedName -> Maybe ResolutionAnnotation
resolveScopedTypeVarAnnotation scope span' name =
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
   in case resolved of
        ResolvedError _ -> Nothing
        _ -> Just (ResolutionAnnotation span' rendered ResolutionNamespaceType resolved)

resolveTypeSignature :: Scope -> Int -> Type -> (Int, Scope, Type)
resolveTypeSignature scope nextLocal ty =
  case ty of
    TForall span' binders inner ->
      let (nextLocal', binderScope, binders') = bindTyVarBindersWithIds scope nextLocal binders
          scoped = unionScope binderScope scope
       in (nextLocal', binderScope, TForall span' binders' (resolveType scoped inner))
    _ -> (nextLocal, emptyScope, resolveType scope ty)

bindTyVarBinders :: Scope -> [TyVarBinder] -> (Scope, [TyVarBinder])
bindTyVarBinders scope binders =
  let (_, binderScope, binders') = bindTyVarBindersWithIds scope 0 binders
   in (binderScope, binders')

bindTyVarBindersWithIds :: Scope -> Int -> [TyVarBinder] -> (Int, Scope, [TyVarBinder])
bindTyVarBindersWithIds outerScope nextLocal =
  foldl' step (nextLocal, emptyScope, [])
  where
    step (currentId, boundScope, acc) binder =
      let scoped = unionScope boundScope outerScope
          binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
          resolvedName = ResolvedLocal currentId binderName
          boundScope' = insertType (tyVarBinderName binder) resolvedName boundScope
          binder' = binder {tyVarBinderKind = fmap (resolveType scoped) (tyVarBinderKind binder)}
       in (currentId + 1, boundScope', acc <> [binder'])

allocateLocalDeclBinders :: Int -> [Decl] -> (Int, Map.Map Text ResolutionAnnotation, Scope)
allocateLocalDeclBinders nextLocal =
  foldl' step (nextLocal, Map.empty, emptyScope)
  where
    step (currentId, annotations, scope) decl =
      case declBinderCandidate decl of
        Just (span', name) ->
          let resolvedName = ResolvedLocal currentId name
              key = renderUnqualifiedName name
              annotation = ResolutionAnnotation span' (renderUnqualifiedName name) ResolutionNamespaceTerm resolvedName
           in ( currentId + 1,
                Map.insert key annotation annotations,
                insertTerm key resolvedName scope
              )
        Nothing -> (currentId, annotations, scope)

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
  case decl of
    DeclValue _ valueDecl ->
      case valueDecl of
        FunctionBind span' name _ -> Just (spanStartNameSpan span' (renderUnqualifiedName name), name)
        PatternBind _ pat _ ->
          case pat of
            PVar span' name -> Just (span', name)
            _ -> Nothing
    DeclTypeSig span' [name] _ -> Just (spanStartNameSpan span' (renderUnqualifiedName name), name)
    _ -> Nothing

topLevelDeclAnnotations :: Decl -> Scope -> [ResolutionAnnotation]
topLevelDeclAnnotations decl scope =
  case decl of
    DeclClass _ classDecl -> [classAnnotation scope classDecl]
    DeclTypeData _ dataDecl -> dataDeclAnnotations "type data " dataDecl
    DeclData _ dataDecl -> dataDeclAnnotations "data " dataDecl
    DeclNewtype _ newtypeDecl ->
      let typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan "newtype " (newtypeDeclSpan newtypeDecl) (renderUnqualifiedName (newtypeDeclName newtypeDecl)))
              (renderUnqualifiedName (newtypeDeclName newtypeDecl))
              ResolutionNamespaceType
              (resolveTopLevelType scope (newtypeDeclName newtypeDecl))
          constructorAnnotations =
            maybe [] (\ctor -> [dataConAnnotation scope ctor]) (newtypeDeclConstructor newtypeDecl)
       in typeAnnotation : constructorAnnotations
    _ -> []
  where
    dataDeclAnnotations keyword dataDecl =
      let typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan keyword (dataDeclSpan dataDecl) (renderUnqualifiedName (dataDeclName dataDecl)))
              (renderUnqualifiedName (dataDeclName dataDecl))
              ResolutionNamespaceType
              (resolveTopLevelType scope (dataDeclName dataDecl))
       in typeAnnotation : map (dataConAnnotation scope) (dataDeclConstructors dataDecl)

classAnnotation :: Scope -> ClassDecl -> ResolutionAnnotation
classAnnotation scope classDecl =
  let className = mkUnqualifiedName NameConId (classDeclName classDecl)
   in ResolutionAnnotation
        (declKeywordNameSpan "class " (classDeclSpan classDecl) (classDeclName classDecl))
        (classDeclName classDecl)
        ResolutionNamespaceType
        (resolveTopLevelType scope className)

resolveTopLevelType :: Scope -> UnqualifiedName -> ResolvedName
resolveTopLevelType scope name = lookupType (renderUnqualifiedName name) scope

resolveTopLevelTerm :: Scope -> UnqualifiedName -> ResolvedName
resolveTopLevelTerm scope name = lookupTerm (renderUnqualifiedName name) scope

dataConAnnotation :: Scope -> DataConDecl -> ResolutionAnnotation
dataConAnnotation scope dataConDecl =
  case dataConDecl of
    PrefixCon span' _ _ name _ ->
      topLevelNameAnnotation scope span' name
    RecordCon span' _ _ name _ ->
      topLevelNameAnnotation scope span' name
    InfixCon span' _ _ _ name _ ->
      topLevelNameAnnotation scope span' name
    GadtCon span' _ _ names _ ->
      case names of
        name : _ -> topLevelNameAnnotation scope span' name
        [] -> ResolutionAnnotation NoSourceSpan "" ResolutionNamespaceTerm (ResolvedError "missing GADT constructor name")

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
      let (termNames, typeNames) = declExportedNames decl
          scope' = foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
       in foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames

declExportedNames :: Decl -> ([UnqualifiedName], [UnqualifiedName])
declExportedNames decl =
  case decl of
    DeclValue _ valueDecl ->
      case valueDecl of
        FunctionBind _ name _ -> ([name], [])
        PatternBind _ pat _ ->
          case pat of
            PVar _ name -> ([name], [])
            _ -> ([], [])
    DeclTypeSig _ names _ -> (names, [])
    DeclClass _ classDecl -> ([], [mkUnqualifiedName NameConId (classDeclName classDecl)])
    DeclTypeData _ dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [dataDeclName dataDecl])
    DeclData _ dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [dataDeclName dataDecl])
    DeclNewtype _ newtypeDecl ->
      ( maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl),
        [newtypeDeclName newtypeDecl]
      )
    _ -> ([], [])

dataDeclConstructorNames :: [DataConDecl] -> [UnqualifiedName]
dataDeclConstructorNames = concatMap dataConDeclNames

dataConDeclNames :: DataConDecl -> [UnqualifiedName]
dataConDeclNames dataConDecl =
  case dataConDecl of
    PrefixCon _ _ _ name _ -> [name]
    InfixCon _ _ _ _ name _ -> [name]
    RecordCon _ _ _ name _ -> [name]
    GadtCon _ _ _ names _ -> names

moduleScope :: ModuleExports -> Module -> Scope
moduleScope exports modu =
  ownScope `unionScope` importedScope exports modu
  where
    ownScope = Map.findWithDefault emptyScope (moduleKey modu) exports

importedScope :: ModuleExports -> Module -> Scope
importedScope exports modu =
  foldl' addImport emptyScope (moduleImports modu)
  where
    addImport acc importDecl
      | importDeclQualified importDecl || importDeclQualifiedPost importDecl = acc
      | otherwise =
          let imported = Map.findWithDefault emptyScope (importDeclModule importDecl) exports
           in unionScope acc (filterImportSpec (importDeclSpec importDecl) imported)

filterImportSpec :: Maybe ImportSpec -> Scope -> Scope
filterImportSpec maybeSpec scope =
  case maybeSpec of
    Nothing -> scope
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      filterScopeByNames (`elem` allowedNames importSpecItems) scope
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      filterScopeByNames (`notElem` allowedNames importSpecItems) scope

allowedNames :: [ImportItem] -> [Text]
allowedNames items =
  [ name
  | item <- items,
    name <- case item of
      ImportItemVar _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAbs _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAll _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemWith _ _ itemName _ -> [renderUnqualifiedName itemName]
      ImportItemAllWith _ _ itemName _ -> [renderUnqualifiedName itemName]
  ]

resolveTermName :: Scope -> Name -> ResolvedName
resolveTermName scope name =
  case nameQualifier name of
    Just qualifier ->
      ResolvedTopLevel (name {nameQualifier = Just qualifier})
    Nothing ->
      lookupTerm (nameText name) scope

resolveTypeName :: Scope -> Name -> ResolvedName
resolveTypeName scope name =
  case nameQualifier name of
    Just qualifier ->
      ResolvedTopLevel (name {nameQualifier = Just qualifier})
    Nothing ->
      lookupType (nameText name) scope

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty

unionScope :: Scope -> Scope -> Scope
unionScope left right =
  Scope
    { scopeTerms = scopeTerms left `Map.union` scopeTerms right,
      scopeTypes = scopeTypes left `Map.union` scopeTypes right
    }

insertTerm :: Text -> ResolvedName -> Scope -> Scope
insertTerm name resolved scope = scope {scopeTerms = Map.insert name resolved (scopeTerms scope)}

insertType :: Text -> ResolvedName -> Scope -> Scope
insertType name resolved scope = scope {scopeTypes = Map.insert name resolved (scopeTypes scope)}

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
      scopeTypes = Map.filterWithKey (\name _ -> keep name) (scopeTypes scope)
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
