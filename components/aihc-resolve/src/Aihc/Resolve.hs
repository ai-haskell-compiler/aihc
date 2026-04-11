{-# LANGUAGE NamedFieldPuns #-}

module Aihc.Resolve
  ( resolve,
    ResolveError (..),
    ResolveResult (..),
    ResolvedName (..),
    ResolutionAnnotation (..),
    renderResolveResult,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
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

type Scope = Map.Map Text ResolvedName

type ModuleExports = Map.Map Text Scope

resolve :: [Module] -> ResolveResult
resolve modules =
  ResolveResult
    { resolvedModules = snd (mapAccumL (resolveModule exports) 0 modules),
      resolveErrors = []
    }
  where
    exports = collectModuleExports modules

resolveModule :: ModuleExports -> Int -> Module -> (Int, Module)
resolveModule exports nextLocal modu =
  let scope = moduleScope exports modu
      (nextLocal', decls') = mapAccumL (resolveTopLevelDecl scope) nextLocal (moduleDecls modu)
   in (nextLocal', modu {moduleDecls = decls'})

resolveTopLevelDecl :: Scope -> Int -> Decl -> (Int, Decl)
resolveTopLevelDecl scope nextLocal decl =
  let (nextLocal', decl') = resolveDecl scope nextLocal decl
   in (nextLocal', maybe decl' (`annotateDecl` decl') (topLevelBinderAnnotation decl scope))

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
    DeclSplice span' expr ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
       in (nextLocal', DeclSplice span' expr')
    _ -> (nextLocal, decl)

resolveMatch :: Scope -> Int -> Match -> (Int, Match)
resolveMatch scope nextLocal match =
  let (nextLocal', patScope, pats') = bindPatterns nextLocal (matchPats match)
      scoped = patScope `Map.union` scope
      (nextLocal'', rhs') = resolveRhs scoped nextLocal' (matchRhs match)
   in (nextLocal'', match {matchPats = pats', matchRhs = rhs'})

resolveRhs :: Scope -> Int -> Rhs -> (Int, Rhs)
resolveRhs scope nextLocal rhs =
  case rhs of
    UnguardedRhs span' expr ->
      let (nextLocal', expr') = resolveExpr scope nextLocal expr
       in (nextLocal', UnguardedRhs span' expr')
    GuardedRhss span' guardedRhss ->
      let (nextLocal', guardedRhss') = mapAccumL (resolveGuardedRhs scope) nextLocal guardedRhss
       in (nextLocal', GuardedRhss span' guardedRhss')

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
          (nextLocal'', patScope, pat') = bindPattern nextLocal' pat
       in (nextLocal'', patScope `Map.union` scope, GuardPat span' pat' expr')
    GuardLet span' decls ->
      let (nextLocal', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal decls
          scoped = localScope `Map.union` scope
          (nextLocal'', decls') = mapAccumL (resolveBoundDecl scoped binderAnnotations) nextLocal' decls
       in (nextLocal'', scoped, GuardLet span' decls')

resolveExpr :: Scope -> Int -> Expr -> (Int, Expr)
resolveExpr scope nextLocal expr =
  case expr of
    EAnn _ inner -> resolveExpr scope nextLocal inner
    EVar span' name ->
      ( nextLocal,
        annotateExpr
          (ResolutionAnnotation span' (nameText name) (resolveName scope name))
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
          scoped = localScope `Map.union` scope
          (nextLocal'', decls') = mapAccumL (resolveBoundDecl scoped binderAnnotations) nextLocal' decls
          (nextLocal''', body') = resolveExpr scoped nextLocal'' body
       in (nextLocal''', ELetDecls span' decls' body')
    ETypeSig span' inner ty ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', ETypeSig span' inner' ty)
    EParen span' inner ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
       in (nextLocal', EParen span' inner')
    EWhereDecls span' inner decls ->
      let (nextLocal', inner') = resolveExpr scope nextLocal inner
          (nextLocal'', binderAnnotations, localScope) = allocateLocalDeclBinders nextLocal' decls
          scoped = localScope `Map.union` scope
          (nextLocal''', decls') = mapAccumL (resolveBoundDecl scoped binderAnnotations) nextLocal'' decls
       in (nextLocal''', EWhereDecls span' inner' decls')
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

bindPatterns :: Int -> [Pattern] -> (Int, Scope, [Pattern])
bindPatterns nextLocal pats =
  let (nextLocal', scopedEntries, pats') = foldl' step (nextLocal, [], []) pats
   in (nextLocal', Map.fromList scopedEntries, reverse pats')
  where
    step (currentId, entries, acc) pat =
      let (nextId, scope, pat') = bindPattern currentId pat
       in (nextId, Map.toList scope <> entries, pat' : acc)

bindPattern :: Int -> Pattern -> (Int, Scope, Pattern)
bindPattern nextLocal pat =
  case pat of
    PAnn _ inner -> bindPattern nextLocal inner
    PVar span' name ->
      let resolvedName = ResolvedLocal nextLocal name
          annotation = ResolutionAnnotation span' (renderUnqualifiedName name) resolvedName
       in (nextLocal + 1, Map.singleton (renderUnqualifiedName name) resolvedName, annotatePattern annotation (PVar span' name))
    PTuple span' flavor pats ->
      let (nextLocal', scope, pats') = bindPatterns nextLocal pats
       in (nextLocal', scope, PTuple span' flavor pats')
    PList span' pats ->
      let (nextLocal', scope, pats') = bindPatterns nextLocal pats
       in (nextLocal', scope, PList span' pats')
    PCon span' name pats ->
      let (nextLocal', scope, pats') = bindPatterns nextLocal pats
       in (nextLocal', scope, PCon span' name pats')
    PInfix span' left name right ->
      let (nextLocal', leftScope, left') = bindPattern nextLocal left
          (nextLocal'', rightScope, right') = bindPattern nextLocal' right
       in (nextLocal'', rightScope `Map.union` leftScope, PInfix span' left' name right')
    PView span' expr inner ->
      let (nextLocal', scope, inner') = bindPattern nextLocal inner
       in (nextLocal', scope, PView span' expr inner')
    PAs span' alias inner ->
      let aliasName = mkUnqualifiedName NameVarId alias
          aliasResolved = ResolvedLocal nextLocal aliasName
          aliasAnnotation = ResolutionAnnotation (spanStartNameSpan span' alias) alias aliasResolved
          (nextLocal', innerScope, inner') = bindPattern (nextLocal + 1) inner
          aliasScope = Map.singleton alias aliasResolved
       in (nextLocal', innerScope `Map.union` aliasScope, annotatePattern aliasAnnotation (PAs span' alias inner'))
    PStrict span' inner ->
      let (nextLocal', scope, inner') = bindPattern nextLocal inner
       in (nextLocal', scope, PStrict span' inner')
    PIrrefutable span' inner ->
      let (nextLocal', scope, inner') = bindPattern nextLocal inner
       in (nextLocal', scope, PIrrefutable span' inner')
    PParen span' inner ->
      let (nextLocal', scope, inner') = bindPattern nextLocal inner
       in (nextLocal', scope, PParen span' inner')
    PRecord span' name fields wildcard ->
      let (nextLocal', entries, fields') =
            foldl'
              ( \(currentId, currentEntries, acc) (fieldName, fieldPat) ->
                  let (nextId, fieldScope, fieldPat') = bindPattern currentId fieldPat
                   in (nextId, Map.toList fieldScope <> currentEntries, (fieldName, fieldPat') : acc)
              )
              (nextLocal, [], [])
              fields
       in (nextLocal', Map.fromList entries, PRecord span' name (reverse fields') wildcard)
    PTypeSig span' inner ty ->
      let (nextLocal', scope, inner') = bindPattern nextLocal inner
       in (nextLocal', scope, PTypeSig span' inner' ty)
    _ -> (nextLocal, Map.empty, pat)

allocateLocalDeclBinders :: Int -> [Decl] -> (Int, Map.Map Text ResolutionAnnotation, Scope)
allocateLocalDeclBinders nextLocal =
  foldl' step (nextLocal, Map.empty, Map.empty)
  where
    step (currentId, annotations, scope) decl =
      case declBinderCandidate decl of
        Just (span', name) ->
          let resolvedName = ResolvedLocal currentId name
              key = renderUnqualifiedName name
              annotation = ResolutionAnnotation span' key resolvedName
           in ( currentId + 1,
                Map.insert key annotation annotations,
                Map.insert key resolvedName scope
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
      let key = renderUnqualifiedName name
       in Just (ResolutionAnnotation span' key (fromMaybe (ResolvedError ("missing top-level binding for " <> T.unpack key)) (Map.lookup key scope)))
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
    _ -> Nothing

collectModuleExports :: [Module] -> ModuleExports
collectModuleExports modules =
  Map.fromList
    [ (moduleKey modu, topLevelScope modu)
    | modu <- modules
    ]

topLevelScope :: Module -> Scope
topLevelScope modu =
  Map.fromList
    [ (key, ResolvedTopLevel (mkQualifiedName name (Just moduleKeyText)))
    | decl <- moduleDecls modu,
      Just (_, name) <- [declBinderCandidate decl],
      let key = renderUnqualifiedName name,
      let moduleKeyText = moduleKey modu
    ]

moduleScope :: ModuleExports -> Module -> Scope
moduleScope exports modu =
  ownScope `Map.union` importedScope exports modu
  where
    ownScope = Map.findWithDefault Map.empty (moduleKey modu) exports

importedScope :: ModuleExports -> Module -> Scope
importedScope exports modu =
  foldl' addImport Map.empty (moduleImports modu)
  where
    addImport acc importDecl
      | importDeclQualified importDecl || importDeclQualifiedPost importDecl = acc
      | otherwise =
          let imported = Map.findWithDefault Map.empty (importDeclModule importDecl) exports
           in Map.union acc (filterImportSpec (importDeclSpec importDecl) imported)

filterImportSpec :: Maybe ImportSpec -> Scope -> Scope
filterImportSpec maybeSpec scope =
  case maybeSpec of
    Nothing -> scope
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      Map.filterWithKey (\name _ -> name `elem` allowedNames importSpecItems) scope
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      Map.filterWithKey (\name _ -> name `notElem` allowedNames importSpecItems) scope

allowedNames :: [ImportItem] -> [Text]
allowedNames items =
  [ name
  | item <- items,
    name <- case item of
      ImportItemVar _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAbs _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemAll _ _ itemName -> [renderUnqualifiedName itemName]
      ImportItemWith _ _ itemName _ -> [renderUnqualifiedName itemName]
  ]

resolveName :: Scope -> Name -> ResolvedName
resolveName scope name =
  case nameQualifier name of
    Just qualifier ->
      ResolvedTopLevel (name {nameQualifier = Just qualifier})
    Nothing ->
      Map.findWithDefault
        (ResolvedError ("unbound name: " <> T.unpack (nameText name)))
        (nameText name)
        scope

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

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
