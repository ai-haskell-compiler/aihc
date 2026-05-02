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
    BangType (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr (..),
    FieldDecl (..),
    ForallTelescope (..),
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
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Types
import Data.Bifunctor
import Data.Data (Data, cast, gmapQ)
import Data.List (mapAccumL)
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

rhsSpan :: Rhs body -> SourceSpan
rhsSpan rhs =
  case rhs of
    UnguardedRhs anns _ _ -> sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeQualifiedModules :: Map.Map Text Scope
  }

type ModuleExports = Map.Map Text Scope

resolve :: [Module] -> ResolveResult
resolve = resolveWithDeps Map.empty

collectResolveErrors :: (Data a) => a -> [ResolveError]
collectResolveErrors node =
  ownResolveErrors node <> concat (gmapQ collectResolveErrors node)

ownResolveErrors :: (Data a) => a -> [ResolveError]
ownResolveErrors node =
  declResolutionErrors (cast node)
    <> patternResolutionErrors (cast node)
    <> typeResolutionErrors (cast node)
    <> exprResolutionErrors (cast node)

declResolutionErrors :: Maybe Decl -> [ResolveError]
declResolutionErrors maybeDecl =
  case maybeDecl of
    Just (DeclResolution resolution) -> maybeToList (annotationResolveError resolution)
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
        FunctionBind name matches ->
          let ambient = effectiveResolutionSpan lastSeen NoSourceSpan
              (nextLocal', matches') = mapAccumL (resolveMatch scope ambient) nextLocal matches
           in (nextLocal', DeclValue (FunctionBind name matches'))
        PatternBind multTag pat rhs ->
          let ambient = effectiveResolutionSpan lastSeen NoSourceSpan
              (nextLocal', rhs') = resolveRhs scope nextLocal ambient rhs
           in (nextLocal', DeclValue (PatternBind multTag pat rhs'))
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
  let here = effectiveResolutionSpan ambient (sourceSpanFromAnns (matchAnns match))
      (nextLocal', patScope, pats') = bindPatterns scope here nextLocal (matchPats match)
      scoped = unionScope patScope scope
      rhsHere = effectiveResolutionSpan here (rhsSpan (matchRhs match))
      (nextLocal'', rhs') = resolveRhs scoped nextLocal' rhsHere (matchRhs match)
   in (nextLocal'', match {matchPats = pats', matchRhs = rhs'})

resolveRhs :: Scope -> Int -> SourceSpan -> Rhs Expr -> (Int, Rhs Expr)
resolveRhs scope nextLocal ambient rhs =
  case rhs of
    UnguardedRhs anns expr mDecls ->
      let bodyHere = effectiveResolutionSpan ambient (sourceSpanFromAnns anns)
          -- Pre-allocate where-clause binders so the body can reference them.
          whereDecls = fromMaybe [] mDecls
          (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal whereDecls
          scoped = unionScope localScope scope
          (nextLocal'', expr') = resolveExprAt scoped nextLocal' bodyHere expr
          (nextLocal''', mDecls') = case mDecls of
            Nothing -> (nextLocal'', Nothing)
            Just decls ->
              let (nl, ds) = resolveBoundDecls scoped binderAnnotations nextLocal'' Map.empty decls
               in (nl, Just ds)
       in (nextLocal''', UnguardedRhs anns expr' mDecls')
    GuardedRhss anns guardedRhss mDecls ->
      let here = effectiveResolutionSpan ambient (sourceSpanFromAnns anns)
          -- Pre-allocate where-clause binders so guards can reference them.
          whereDecls = fromMaybe [] mDecls
          (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal whereDecls
          scoped = unionScope localScope scope
          (nextLocal'', guardedRhss') = mapAccumL (resolveGuardedRhs scoped here) nextLocal' guardedRhss
          (nextLocal''', mDecls') = case mDecls of
            Nothing -> (nextLocal'', Nothing)
            Just decls ->
              let (nl, ds) = resolveBoundDecls scoped binderAnnotations nextLocal'' Map.empty decls
               in (nl, Just ds)
       in (nextLocal''', GuardedRhss anns guardedRhss' mDecls')

resolveGuardedRhs :: Scope -> SourceSpan -> Int -> GuardedRhs Expr -> (Int, GuardedRhs Expr)
resolveGuardedRhs scope ambient nextLocal guardedRhs =
  let here = effectiveResolutionSpan ambient (sourceSpanFromAnns (guardedRhsAnns guardedRhs))
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
    ETypeSyntax form ty ->
      let here = peelExprSpan lastSeen expr
       in (nextLocal, ETypeSyntax form (resolveTypeAt scope here ty))
    EParen inner ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', inner') = resolveExprAt scope nextLocal here inner
       in (nextLocal', EParen inner')
    ETypeApp fun ty ->
      let here = peelExprSpan lastSeen expr
          (nextLocal', fun') = resolveExprAt scope nextLocal here fun
       in (nextLocal', ETypeApp fun' (resolveTypeAt scope here ty))
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
    PTypeBinder binder ->
      let scoped = unionScope emptyScope typeScope
          binderName = mkUnqualifiedName NameVarId (tyVarBinderName binder)
          resolvedName = ResolvedLocal nextLocal binderName
          binder' = binder {tyVarBinderKind = fmap (resolveTypeAt scoped NoSourceSpan) (tyVarBinderKind binder)}
          binderScope = Scope Map.empty (Map.singleton (tyVarBinderName binder) resolvedName) Map.empty
       in (nextLocal + 1, binderScope, PTypeBinder binder')
    PTypeSyntax form ty ->
      let here = peelPatternSpan lastSeen pat
       in (nextLocal, emptyScope, PTypeSyntax form (resolveTypeAt typeScope here ty))
    PTuple flavor pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PTuple flavor pats')
    PList pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PList pats')
    PCon name typeArgs pats ->
      let here = peelPatternSpan lastSeen pat
          (nextLocal', scope, pats') = bindPatterns typeScope here nextLocal pats
       in (nextLocal', scope, PCon name (map (resolveTypeAt typeScope here) typeArgs) pats')
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
          aliasKey = renderUnqualifiedName alias
          aliasResolved = ResolvedLocal nextLocal alias
          aliasAnnotation =
            ResolutionAnnotation (spanStartNameSpan here aliasKey) aliasKey ResolutionNamespaceTerm aliasResolved
          (nextLocal', innerScope, inner') = bindPattern typeScope here (nextLocal + 1) inner
          aliasScope = Scope (Map.singleton aliasKey aliasResolved) Map.empty Map.empty
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
              ( \(currentId, currentEntries, acc) field ->
                  let (nextId, fieldScope, fieldPat') = bindPattern typeScope here currentId (recordFieldValue field)
                   in (nextId, Map.toList (scopeTerms fieldScope) <> currentEntries, field {recordFieldValue = fieldPat'} : acc)
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
    TupleCon forallVars context flavor fields ->
      TupleCon forallVars (map (resolveType scope) context) flavor (map resolveBangType fields)
    UnboxedSumCon forallVars context pos arity field ->
      UnboxedSumCon forallVars (map (resolveType scope) context) pos arity (resolveBangType field)
    ListCon forallVars context ->
      ListCon forallVars (map (resolveType scope) context)
  where
    resolveBangType bt = bt {bangType = resolveType scope (bangType bt)}
    resolveFieldDecl fieldDecl = fieldDecl {fieldType = resolveBangType (fieldType fieldDecl)}

resolveGadtBody :: Scope -> GadtBody -> GadtBody
resolveGadtBody scope body =
  case body of
    GadtPrefixBody bangTypes ty ->
      GadtPrefixBody (map (Data.Bifunctor.first resolveBangType) bangTypes) (resolveType scope ty)
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
    TForall telescope inner ->
      let here = lastSeen
          (binderScope, binders') = bindTyVarBinders scope (forallTelescopeBinders telescope)
          scoped = unionScope binderScope scope
       in TForall (telescope {forallTelescopeBinders = binders'}) (resolveTypeAt scoped here inner)
    TApp left right ->
      let here = lastSeen
       in TApp (resolveTypeAt scope here left) (resolveTypeAt scope here right)
    TFun arrowKind left right ->
      let here = lastSeen
       in TFun arrowKind (resolveTypeAt scope here left) (resolveTypeAt scope here right)
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
    TForall telescope inner ->
      let (nextLocal', binderScope, binders') = bindTyVarBindersWithIds scope nextLocal (forallTelescopeBinders telescope)
          scoped = unionScope binderScope scope
       in (nextLocal', binderScope, TForall (telescope {forallTelescopeBinders = binders'}) (resolveTypeAt scoped ambient inner))
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
    step acc decl = foldl' addBinder acc (declBinderCandidates decl)
    addBinder (currentId, annotations, scope) (span', name) =
      let resolvedName = ResolvedLocal currentId name
          key = renderUnqualifiedName name
          annotation = ResolutionAnnotation span' key ResolutionNamespaceTerm resolvedName
       in (currentId + 1, Map.insert key annotation annotations, insertTerm key resolvedName scope)

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
      let (termNames, typeNames) = declExportedNames decl
          scope' = foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
       in foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames

declExportedNames :: Decl -> ([UnqualifiedName], [UnqualifiedName])
declExportedNames decl =
  case decl of
    DeclAnn _ inner -> declExportedNames inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name _ -> ([name], [])
        PatternBind _ pat _ ->
          (map snd (collectPatVarBinders NoSourceSpan pat), [])
    DeclTypeSig names _ -> (names, [])
    DeclClass classDecl ->
      ( classDeclMethodNames (classDeclItems classDecl),
        [binderHeadName (classDeclHead classDecl)]
      )
    DeclTypeData dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [binderHeadName (dataDeclHead dataDecl)])
    DeclData dataDecl -> (dataDeclConstructorNames (dataDeclConstructors dataDecl), [binderHeadName (dataDeclHead dataDecl)])
    DeclNewtype newtypeDecl ->
      ( maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl),
        [binderHeadName (newtypeDeclHead newtypeDecl)]
      )
    DeclTypeSyn typeSynDecl -> ([], [binderHeadName (typeSynHead typeSynDecl)])
    _ -> ([], [])

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
      let allowed = allowedNames importSpecItems
          -- When any T(..) import is present, all terms are allowed (we can't
          -- enumerate class methods / constructors without full type info).
          allTerms = any isAllImport importSpecItems
       in Scope
            { scopeTerms =
                if allTerms
                  then scopeTerms scope
                  else Map.filterWithKey (\n _ -> n `elem` allowed) (scopeTerms scope),
              scopeTypes = Map.filterWithKey (\n _ -> n `elem` allowed) (scopeTypes scope),
              scopeQualifiedModules = scopeQualifiedModules scope
            }
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      filterScopeByNames (`notElem` allowedNames importSpecItems) scope

isAllImport :: ImportItem -> Bool
isAllImport (ImportItemAll {}) = True
isAllImport (ImportItemAllWith {}) = True
isAllImport (ImportAnn _ sub) = isAllImport sub
isAllImport _ = False

allowedNames :: [ImportItem] -> [Text]
allowedNames items =
  [ name
  | item <- items,
    name <- case item of
      ImportItemVar _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAbs _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAll _ itemName -> [renderUnqualifiedName itemName]
      ImportItemWith _ itemName _ -> [renderUnqualifiedName itemName]
      ImportItemAllWith _ itemName _ _ -> [renderUnqualifiedName itemName]
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
