{-# LANGUAGE OverloadedStrings #-}

module Aihc.Resolve.Scope
  ( Scope (..),
    ModuleExports,
    collectModuleExports,
    moduleScope,
    moduleKey,
    emptyScope,
    unionScope,
    insertTerm,
    insertType,
    lookupTerm,
    lookupType,
    resolveTermName,
    resolveTypeName,
    collectPatVarBinders,
    tupleConName,
    unboxedSumConName,
    listConName,
    importItemTypeName,
  )
where

import Aihc.Parser.Syntax
  ( BinderHead,
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    FieldDecl (..),
    GadtBody (..),
    IEBundledMember (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    Pattern (..),
    RecordField (..),
    SourceSpan (..),
    TupleFlavor (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    binderHeadName,
    mkUnqualifiedName,
    moduleName,
    peelPatternAnn,
    qualifyName,
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Span (spanStartNameSpan)
import Aihc.Resolve.Types
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeConstructors :: Map.Map Text [Text],
    scopeRecordFields :: Map.Map Text [Text],
    scopeMethods :: Map.Map Text [Text],
    scopeQualifiedModules :: Map.Map Text Scope
  }

type ModuleExports = Map.Map Text Scope

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
    qualify = ResolvedTopLevel . qualifyName (Just moduleKeyText)
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
