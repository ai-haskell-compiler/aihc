{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

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
  ( Annotation,
    BangType (..),
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
    fromAnnotation,
    mkAnnotation,
    mkQualifiedName,
    mkUnqualifiedName,
    moduleName,
    peelGuardQualifierAnn,
    peelPatternAnn,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Types
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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

-- | Resolver-owned span tracking for nodes that now store source spans only in
-- annotations.
peelDeclSpan :: SourceSpan -> Decl -> (SourceSpan, Decl)
peelDeclSpan ambient (DeclAnn ann inner) = peelDeclSpan (pushSpanFromAnn ambient ann) inner
peelDeclSpan ambient decl = (ambient, decl)

peelExprSpan :: SourceSpan -> Expr -> SourceSpan
peelExprSpan ambient (EAnn ann inner) = peelExprSpan (pushSpanFromAnn ambient ann) inner
peelExprSpan ambient _ = ambient

peelPatternSpan :: SourceSpan -> Pattern -> SourceSpan
peelPatternSpan ambient (PAnn ann inner) = peelPatternSpan (pushSpanFromAnn ambient ann) inner
peelPatternSpan ambient _ = ambient

peelGuardQualifierSpan :: SourceSpan -> GuardQualifier -> SourceSpan
peelGuardQualifierSpan ambient (GuardAnn ann inner) = peelGuardQualifierSpan (pushSpanFromAnn ambient ann) inner
peelGuardQualifierSpan ambient _ = ambient

peelDataConSpan :: SourceSpan -> DataConDecl -> SourceSpan
peelDataConSpan ambient (DataConAnn ann inner) = peelDataConSpan (pushSpanFromAnn ambient ann) inner
peelDataConSpan ambient _ = ambient

rhsSpan :: Rhs -> SourceSpan
rhsSpan rhs =
  case rhs of
    UnguardedRhs span' _ _ -> span'
    GuardedRhss span' _ _ -> span'

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeQualifiedModules :: Map.Map Text Scope
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
    DeclAnn ann inner ->
      let (nextLocal', signatureScopes', inner') =
            resolveDeclWithSignatureScope scope nextLocal signatureScopes inner
       in (nextLocal', signatureScopes', DeclAnn ann inner')
    DeclTypeSig names ty ->
      let (nextLocal', binderScope, ty') = resolveTypeSignature scope nextLocal ty
          signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
       in (nextLocal', signatureScopes', DeclTypeSig names ty')
    _ ->
      let (nextLocal', decl') = resolveDecl scope nextLocal decl
          signatureScopes' =
            case declBinderCandidate decl of
              Just (_, name) -> Map.delete (renderUnqualifiedName name) signatureScopes
              Nothing -> signatureScopes
       in (nextLocal', signatureScopes', decl')

resolveDecl :: Scope -> Int -> Decl -> (Int, Decl)
resolveDecl scope nextLocal =
  resolveDeclGo scope nextLocal NoSourceSpan

resolveDeclGo :: Scope -> Int -> SourceSpan -> Decl -> (Int, Decl)
resolveDeclGo scope nextLocal lastSeen (DeclAnn ann inner) =
  resolveDeclGo scope nextLocal (pushSpanFromAnn lastSeen ann) inner
resolveDeclGo scope nextLocal lastSeen decl =
  resolveDeclCore scope nextLocal lastSeen decl

resolveDeclCore :: Scope -> Int -> SourceSpan -> Decl -> (Int, Decl)
resolveDeclCore scope nextLocal lastSeen decl =
  case decl of
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind bindSpan name matches ->
          let ambient = effectiveResolutionSpan lastSeen bindSpan
              (nextLocal', matches') = mapAccumL (resolveMatch scope ambient) nextLocal matches
           in (nextLocal', DeclValue (FunctionBind bindSpan name matches'))
        PatternBind bindSpan pat rhs ->
          let ambient = effectiveResolutionSpan lastSeen bindSpan
              (nextLocal', rhs') = resolveRhs scope nextLocal ambient rhs
           in (nextLocal', DeclValue (PatternBind bindSpan pat rhs'))
    DeclTypeSig names ty ->
      let ty' = resolveTypeAt scope lastSeen ty
       in (nextLocal, DeclTypeSig names ty')
    DeclTypeData dataDecl ->
      (nextLocal, DeclTypeData (resolveDataDecl scope dataDecl))
    DeclData dataDecl ->
      (nextLocal, DeclData (resolveDataDecl scope dataDecl))
    DeclSplice expr ->
      let (nextLocal', expr') = resolveExprAt scope nextLocal lastSeen expr
       in (nextLocal', DeclSplice expr')
    DeclNewtype newtypeDecl ->
      ( nextLocal,
        DeclNewtype
          ( newtypeDecl
              { newtypeDeclKind = fmap (resolveType scope) (newtypeDeclKind newtypeDecl),
                newtypeDeclConstructor = fmap (resolveDataConDecl scope) (newtypeDeclConstructor newtypeDecl)
              }
          )
      )
    _ -> (nextLocal, decl)

resolveMatch :: Scope -> SourceSpan -> Int -> Match -> (Int, Match)
resolveMatch scope ambient nextLocal match =
  let here = effectiveResolutionSpan ambient (matchSpan match)
      (nextLocal', patScope, pats') = bindPatterns scope here nextLocal (matchPats match)
      scoped = unionScope patScope scope
      rhsHere = effectiveResolutionSpan here (rhsSpan (matchRhs match))
      (nextLocal'', rhs') = resolveRhs scoped nextLocal' rhsHere (matchRhs match)
   in (nextLocal'', match {matchPats = pats', matchRhs = rhs'})

resolveRhs :: Scope -> Int -> SourceSpan -> Rhs -> (Int, Rhs)
resolveRhs scope nextLocal ambient rhs =
  case rhs of
    UnguardedRhs span' expr mDecls ->
      let bodyHere = effectiveResolutionSpan ambient span'
          (nextLocal', expr') = resolveExprAt scope nextLocal bodyHere expr
          (nextLocal'', mDecls') = resolveWhereDecls scope nextLocal' mDecls
       in (nextLocal'', UnguardedRhs span' expr' mDecls')
    GuardedRhss span' guardedRhss mDecls ->
      let here = effectiveResolutionSpan ambient span'
          (nextLocal', guardedRhss') = mapAccumL (resolveGuardedRhs scope here) nextLocal guardedRhss
          (nextLocal'', mDecls') = resolveWhereDecls scope nextLocal' mDecls
       in (nextLocal'', GuardedRhss span' guardedRhss' mDecls')

resolveWhereDecls :: Scope -> Int -> Maybe [Decl] -> (Int, Maybe [Decl])
resolveWhereDecls _ nextLocal Nothing = (nextLocal, Nothing)
resolveWhereDecls scope nextLocal (Just decls) =
  let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
      scoped = unionScope localScope scope
      (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
   in (nextLocal'', Just decls')

resolveGuardedRhs :: Scope -> SourceSpan -> Int -> GuardedRhs -> (Int, GuardedRhs)
resolveGuardedRhs scope ambient nextLocal guardedRhs =
  let here = effectiveResolutionSpan ambient (guardedRhsSpan guardedRhs)
      (nextLocal', scope', guards') = resolveGuardQualifiers scope nextLocal here (guardedRhsGuards guardedRhs)
      (nextLocal'', body') = resolveExprAt scope' nextLocal' here (guardedRhsBody guardedRhs)
   in (nextLocal'', guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'})

resolveGuardQualifiers :: Scope -> Int -> SourceSpan -> [GuardQualifier] -> (Int, Scope, [GuardQualifier])
resolveGuardQualifiers scope nextLocal ambient qualifiers =
  case qualifiers of
    [] -> (nextLocal, scope, [])
    qualifier : rest ->
      let (nextLocal', scope', qualifier') = resolveGuardQualifier scope nextLocal ambient qualifier
          (nextLocal'', scope'', rest') = resolveGuardQualifiers scope' nextLocal' ambient rest
       in (nextLocal'', scope'', qualifier' : rest')

resolveGuardQualifier :: Scope -> Int -> SourceSpan -> GuardQualifier -> (Int, Scope, GuardQualifier)
resolveGuardQualifier scope nextLocal ambient qualifier =
  let qualifierSpan = peelGuardQualifierSpan NoSourceSpan qualifier
      here = effectiveResolutionSpan ambient qualifierSpan
      wrap = GuardAnn (mkAnnotation qualifierSpan)
   in case peelGuardQualifierAnn qualifier of
        GuardExpr expr ->
          let (nextLocal', expr') = resolveExprAt scope nextLocal here expr
           in (nextLocal', scope, wrap (GuardExpr expr'))
        GuardPat pat expr ->
          let (nextLocal', expr') = resolveExprAt scope nextLocal here expr
              (nextLocal'', patScope, pat') = bindPattern scope here nextLocal' pat
           in (nextLocal'', unionScope patScope scope, wrap (GuardPat pat' expr'))
        GuardLet decls ->
          let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
              scoped = unionScope localScope scope
              (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
           in (nextLocal'', scoped, wrap (GuardLet decls'))
        GuardAnn _ _ -> (nextLocal, scope, qualifier)

resolveExprAt :: Scope -> Int -> SourceSpan -> Expr -> (Int, Expr)
resolveExprAt scope nextLocal lastSeen expr =
  case expr of
    EAnn ann inner ->
      resolveExprAt scope nextLocal (pushSpanFromAnn lastSeen ann) inner
    EVar name ->
      let sp = peelExprSpan lastSeen expr
       in ( nextLocal,
            annotateExpr
              (ResolutionAnnotation sp (nameText name) ResolutionNamespaceTerm (resolveTermName scope name))
              (EVar name)
          )
    EIf cond trueBranch falseBranch ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', cond') = resolveExprAt scope nextLocal here cond
          (nextLocal'', trueBranch') = resolveExprAt scope nextLocal' here trueBranch
          (nextLocal''', falseBranch') = resolveExprAt scope nextLocal'' here falseBranch
       in (nextLocal''', EIf cond' trueBranch' falseBranch')
    EInfix left op right ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', left') = resolveExprAt scope nextLocal here left
          (nextLocal'', right') = resolveExprAt scope nextLocal' here right
       in (nextLocal'', EInfix left' op right')
    ENegate inner ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', ENegate inner')
    ESectionL inner op ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', ESectionL inner' op)
    ESectionR op inner ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', ESectionR op inner')
    ELetDecls decls body ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
          scoped = unionScope localScope scope
          (nextLocal'', decls') = resolveBoundDecls scoped binderAnnotations nextLocal' Map.empty decls
          (nextLocal''', body') = resolveExprAt scoped nextLocal'' here body
       in (nextLocal''', ELetDecls decls' body')
    ETypeSig inner ty ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', ETypeSig inner' (resolveTypeAt scope here ty))
    EParen inner ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', EParen inner')
    ETypeApp fun ty ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', fun') = resolveExprAt scope nextLocal here fun
       in (nextLocal', ETypeApp fun' ty)
    EApp fun arg ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', fun') = resolveExprAt scope nextLocal here fun
          (nextLocal'', arg') = resolveExprAt scope nextLocal' here arg
       in (nextLocal'', EApp fun' arg')
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
    DeclTypeSig names ty ->
      let (nextLocal', binderScope, ty') = resolveTypeSignature scope nextLocal ty
          signatureScopes' =
            foldl'
              (\acc name -> Map.insert (renderUnqualifiedName name) binderScope acc)
              signatureScopes
              names
          resolvedDecl = DeclTypeSig names ty'
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

bindPatterns :: Scope -> SourceSpan -> Int -> [Pattern] -> (Int, Scope, [Pattern])
bindPatterns typeScope ambient nextLocal pats =
  let (nextLocal', scopedEntries, pats') = foldl' step (nextLocal, [], []) pats
   in (nextLocal', Scope (Map.fromList scopedEntries) Map.empty Map.empty, reverse pats')
  where
    step (currentId, entries, acc) pat =
      let (nextId, scope, pat') = bindPattern typeScope ambient currentId pat
       in (nextId, Map.toList (scopeTerms scope) <> entries, pat' : acc)

bindPattern :: Scope -> SourceSpan -> Int -> Pattern -> (Int, Scope, Pattern)
bindPattern typeScope lastSeen nextLocal pat =
  case pat of
    PAnn ann inner ->
      bindPattern typeScope (pushSpanFromAnn lastSeen ann) nextLocal inner
    PVar name ->
      let sp = peelPatternSpan lastSeen pat
          resolvedName = ResolvedLocal nextLocal name
          annotation = ResolutionAnnotation sp (renderUnqualifiedName name) ResolutionNamespaceTerm resolvedName
       in (nextLocal + 1, Scope (Map.singleton (renderUnqualifiedName name) resolvedName) Map.empty Map.empty, annotatePattern annotation (PVar name))
    PTuple flavor pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PTuple flavor pats')
    PList pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PList pats')
    PCon name pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PCon name pats')
    PInfix left name right ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', leftScope, left') = bindPattern typeScope here nextLocal left
          (nextLocal'', rightScope, right') = bindPattern typeScope here nextLocal' right
       in (nextLocal'', unionScope rightScope leftScope, PInfix left' name right')
    PView expr inner ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, inner') = bindPattern typeScope here nextLocal inner
       in (nextLocal', scope, PView expr inner')
    PAs alias inner ->
      let here = peelPatternSpan lastSeen pat
          aliasName = mkUnqualifiedName NameVarId alias
          aliasResolved = ResolvedLocal nextLocal aliasName
          aliasAnnotation =
            ResolutionAnnotation (spanStartNameSpan here alias) alias ResolutionNamespaceTerm aliasResolved
          (nextLocal', innerScope, inner') = bindPattern typeScope here (nextLocal + 1) inner
          aliasScope = Scope (Map.singleton alias aliasResolved) Map.empty Map.empty
       in (nextLocal', unionScope innerScope aliasScope, annotatePattern aliasAnnotation (PAs alias inner'))
    PStrict inner ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, inner') = bindPattern typeScope here nextLocal inner
       in (nextLocal', scope, PStrict inner')
    PIrrefutable inner ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, inner') = bindPattern typeScope here nextLocal inner
       in (nextLocal', scope, PIrrefutable inner')
    PParen inner ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, inner') = bindPattern typeScope here nextLocal inner
       in (nextLocal', scope, PParen inner')
    PRecord name fields wildcard ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', entries, fields') =
            foldl'
              ( \(currentId, currentEntries, acc) (fieldName, fieldPat) ->
                  let (nextId, fieldScope, fieldPat') = bindPattern typeScope here currentId fieldPat
                   in (nextId, Map.toList (scopeTerms fieldScope) <> currentEntries, (fieldName, fieldPat') : acc)
              )
              (nextLocal, [], [])
              fields
       in (nextLocal', Scope (Map.fromList entries) Map.empty Map.empty, PRecord name (reverse fields') wildcard)
    PTypeSig inner ty ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, inner') = bindPattern typeScope here nextLocal inner
       in (nextLocal', scope, PTypeSig inner' (resolveTypeAt typeScope here ty))
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
    DataConAnn ann inner -> DataConAnn ann (resolveDataConDecl scope inner)
    PrefixCon forallVars context name bangTypes ->
      PrefixCon forallVars (map (resolveType scope) context) name (map resolveBangType bangTypes)
    InfixCon forallVars context lhs name rhs ->
      InfixCon forallVars (map (resolveType scope) context) (resolveBangType lhs) name (resolveBangType rhs)
    RecordCon forallVars context name fields ->
      RecordCon forallVars (map (resolveType scope) context) name (map resolveFieldDecl fields)
    GadtCon forallVars context names body ->
      GadtCon forallVars (map (resolveType scope) context) names (resolveGadtBody scope body)
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
resolveType scope = resolveTypeAt scope NoSourceSpan

resolveTypeAt :: Scope -> SourceSpan -> Type -> Type
resolveTypeAt scope lastSeen ty =
  case ty of
    TAnn ann inner -> resolveTypeAt scope (pushSpanFromAnn lastSeen ann) inner
    TVar name ->
      let sp = lastSeen
          resolvedTyVar = TVar name
       in maybe resolvedTyVar (`annotateType` resolvedTyVar) (resolveScopedTypeVarAnnotation scope sp name)
    TCon name promoted ->
      let sp = lastSeen
       in annotateType
            (ResolutionAnnotation sp (nameText name) ResolutionNamespaceType (resolveTypeName scope name))
            (TCon name promoted)
    TImplicitParam name inner ->
      let here = lastSeen
       in TImplicitParam name (resolveTypeAt scope here inner)
    TForall binders inner ->
      let here = lastSeen
          (binderScope, binders') = bindTyVarBinders scope binders
          scoped = unionScope binderScope scope
       in TForall binders' (resolveTypeAt scoped here inner)
    TApp left right ->
      let here = lastSeen
       in TApp (resolveTypeAt scope here left) (resolveTypeAt scope here right)
    TFun left right ->
      let here = lastSeen
       in TFun (resolveTypeAt scope here left) (resolveTypeAt scope here right)
    TTuple flavor promoted items ->
      let here = lastSeen
       in TTuple flavor promoted (map (resolveTypeAt scope here) items)
    TUnboxedSum items ->
      let here = lastSeen
       in TUnboxedSum (map (resolveTypeAt scope here) items)
    TList promoted items ->
      let here = lastSeen
       in TList promoted (map (resolveTypeAt scope here) items)
    TParen inner ->
      let here = lastSeen
       in TParen (resolveTypeAt scope here inner)
    TKindSig inner kind ->
      let here = lastSeen
       in TKindSig (resolveTypeAt scope here inner) (resolveTypeAt scope here kind)
    TContext constraints inner ->
      let here = lastSeen
       in TContext (map (resolveTypeAt scope here) constraints) (resolveTypeAt scope here inner)
    TSplice expr ->
      let here = lastSeen
       in TSplice (snd (resolveExprAt scope 0 here expr))
    _ -> ty

resolveScopedTypeVarAnnotation :: Scope -> SourceSpan -> UnqualifiedName -> Maybe ResolutionAnnotation
resolveScopedTypeVarAnnotation scope span' name =
  let rendered = renderUnqualifiedName name
      resolved = lookupType rendered scope
   in case resolved of
        ResolvedError _ -> Nothing
        _ -> Just (ResolutionAnnotation span' rendered ResolutionNamespaceType resolved)

resolveTypeSignature :: Scope -> Int -> Type -> (Int, Scope, Type)
resolveTypeSignature scope nextLocal = resolveTypeSignatureAt scope nextLocal NoSourceSpan

-- | Like 'resolveTypeSignature' but threads an ambient span from peeled 'TAnn' wrappers.
resolveTypeSignatureAt :: Scope -> Int -> SourceSpan -> Type -> (Int, Scope, Type)
resolveTypeSignatureAt scope nextLocal ambient ty =
  case ty of
    -- Type signatures may carry span-only 'TAnn' wrappers (see 'typeAnnSpan'); peel
    -- them so we still allocate scoped type variables and advance 'nextLocal'.
    TAnn ann sub -> resolveTypeSignatureAt scope nextLocal (pushSpanFromAnn ambient ann) sub
    TForall binders inner ->
      let (nextLocal', binderScope, binders') = bindTyVarBindersWithIds scope nextLocal binders
          scoped = unionScope binderScope scope
       in (nextLocal', binderScope, TForall binders' (resolveTypeAt scoped ambient inner))
    _ -> (nextLocal, emptyScope, resolveTypeAt scope ambient ty)

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
          binder' = binder {tyVarBinderKind = fmap (resolveTypeAt scoped NoSourceSpan) (tyVarBinderKind binder)}
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
  let (outerSp, innerDecl) = peelDeclSpan NoSourceSpan decl
   in case innerDecl of
        DeclValue valueDecl ->
          case valueDecl of
            FunctionBind span' name _ ->
              let loc = effectiveResolutionSpan outerSp span'
               in Just (spanStartNameSpan loc (renderUnqualifiedName name), name)
            PatternBind bindSp pat _ ->
              case peelPatternAnn pat of
                PVar name ->
                  let loc =
                        effectiveResolutionSpan
                          (effectiveResolutionSpan outerSp bindSp)
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
          typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan "newtype " span' (renderUnqualifiedName (newtypeDeclName newtypeDecl)))
              (renderUnqualifiedName (newtypeDeclName newtypeDecl))
              ResolutionNamespaceType
              (resolveTopLevelType scope (newtypeDeclName newtypeDecl))
          constructorAnnotations =
            maybe [] (\ctor -> [dataConAnnotation scope ctor]) (newtypeDeclConstructor newtypeDecl)
       in typeAnnotation : constructorAnnotations
    _ -> []
  where
    dataDeclAnnotations declSpan keyword dataDecl =
      let span' = declSpan
          typeAnnotation =
            ResolutionAnnotation
              (declKeywordNameSpan keyword span' (renderUnqualifiedName (dataDeclName dataDecl)))
              (renderUnqualifiedName (dataDeclName dataDecl))
              ResolutionNamespaceType
              (resolveTopLevelType scope (dataDeclName dataDecl))
       in typeAnnotation : map (dataConAnnotation scope) (dataDeclConstructors dataDecl)

classAnnotation :: Scope -> SourceSpan -> ClassDecl -> ResolutionAnnotation
classAnnotation scope declSpan classDecl =
  let className = mkUnqualifiedName NameConId (classDeclName classDecl)
      span' = declSpan
   in ResolutionAnnotation
        (declKeywordNameSpan "class " span' (classDeclName classDecl))
        (classDeclName classDecl)
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
          PrefixCon _ _ name _ ->
            topLevelNameAnnotation scope span' name
          RecordCon _ _ name _ ->
            topLevelNameAnnotation scope span' name
          InfixCon _ _ _ name _ ->
            topLevelNameAnnotation scope span' name
          GadtCon _ _ names _ ->
            case names of
              name : _ -> topLevelNameAnnotation scope span' name
              [] -> ResolutionAnnotation NoSourceSpan "" ResolutionNamespaceTerm (ResolvedError "missing GADT constructor name")
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
      let (termNames, typeNames) = declExportedNames decl
          scope' = foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
       in foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames

declExportedNames :: Decl -> ([UnqualifiedName], [UnqualifiedName])
declExportedNames decl =
  case decl of
    DeclAnn _ inner -> declExportedNames inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind _ name _ -> ([name], [])
        PatternBind _ pat _ ->
          case peelPatternAnn pat of
            PVar name -> ([name], [])
            _ -> ([], [])
    DeclTypeSig names _ -> (names, [])
    DeclClass classDecl -> ([], [mkUnqualifiedName NameConId (classDeclName classDecl)])
    DeclTypeData dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [dataDeclName dataDecl])
    DeclData dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [dataDeclName dataDecl])
    DeclNewtype newtypeDecl ->
      ( maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl),
        [newtypeDeclName newtypeDecl]
      )
    _ -> ([], [])

dataDeclConstructorNames :: [DataConDecl] -> [UnqualifiedName]
dataDeclConstructorNames = concatMap dataConDeclNames

dataConDeclNames :: DataConDecl -> [UnqualifiedName]
dataConDeclNames dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          PrefixCon _ _ name _ -> [name]
          InfixCon _ _ _ name _ -> [name]
          RecordCon _ _ name _ -> [name]
          GadtCon _ _ names _ -> names
   in go dataConDecl

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
      filterScopeByNames (`elem` allowedNames importSpecItems) scope
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      filterScopeByNames (`notElem` allowedNames importSpecItems) scope

allowedNames :: [ImportItem] -> [Text]
allowedNames items =
  [ name
  | item <- items,
    name <- case item of
      ImportItemVar _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAbs _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAll _ itemName -> [renderUnqualifiedName itemName]
      ImportItemWith _ itemName _ -> [renderUnqualifiedName itemName]
      ImportItemAllWith _ itemName _ -> [renderUnqualifiedName itemName]
      ImportAnn _ sub -> allowedNames [sub]
  ]

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
emptyScope = Scope Map.empty Map.empty Map.empty

unionScope :: Scope -> Scope -> Scope
unionScope left right =
  Scope
    { scopeTerms = scopeTerms left `Map.union` scopeTerms right,
      scopeTypes = scopeTypes left `Map.union` scopeTypes right,
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
