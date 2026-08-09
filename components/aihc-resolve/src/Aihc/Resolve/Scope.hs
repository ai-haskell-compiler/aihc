{-# LANGUAGE OverloadedStrings #-}

module Aihc.Resolve.Scope
  ( Scope (..),
    OperatorFixity (..),
    ModuleExports,
    ModuleKey (..),
    collectModuleExports,
    collectModuleExportsWithDeps,
    moduleScope,
    moduleKey,
    matchingModuleScopes,
    lookupImportedModule,
    emptyScope,
    unionScope,
    insertTerm,
    insertType,
    lookupTerm,
    lookupType,
    lookupFixity,
    resolveTermName,
    resolveTypeName,
    resolveFixityName,
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
    DataFamilyDecl (..),
    DataFamilyInst (..),
    Decl (..),
    ExportSpec (..),
    FieldDecl (..),
    FixityAssoc (..),
    ForeignDecl (..),
    ForeignDirection (..),
    GadtBody (..),
    IEBundledMember (..),
    IEEntityNamespace (..),
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
    Type (..),
    TypeSynDecl (..),
    UnqualifiedName,
    ValueDecl (..),
    binderHeadName,
    mkUnqualifiedName,
    moduleExports,
    moduleName,
    peelPatternAnn,
    peelTypeHead,
    qualifyName,
    recordFieldValue,
    renderUnqualifiedName,
  )
import Aihc.Resolve.Span (spanStartNameSpan)
import Aihc.Resolve.Types
import Data.List qualified as List
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
    scopeFixities :: Map.Map Text OperatorFixity,
    scopeQualifiedModules :: Map.Map Text Scope
  }
  deriving (Eq)

data OperatorFixity = OperatorFixity
  { operatorFixityAssoc :: !FixityAssoc,
    operatorFixityPrecedence :: !Int
  }
  deriving (Eq, Show, Read)

data ModuleKey = ModuleKey
  { moduleKeyPackage :: !Package,
    moduleKeyName :: !Text
  }
  deriving (Eq, Ord, Show)

type ModuleExports = Map.Map ModuleKey Scope

collectModuleExports :: [(Package, Module)] -> ModuleExports
collectModuleExports = collectModuleExportsWithDeps Map.empty

-- | Extract interfaces for a compilation unit while allowing its explicit
-- export lists to re-export names supplied by predecessor units.
collectModuleExportsWithDeps :: ModuleExports -> [(Package, Module)] -> ModuleExports
collectModuleExportsWithDeps depExports packageModules = Map.restrictKeys (closeExports initialExports) moduleKeys
  where
    moduleKeys = Map.keysSet localExports
    localExports =
      Map.fromList
        [ (exportKey package modu, emptyScope)
        | (package, modu) <- packageModules
        ]
    initialExports =
      localExports `Map.union` depExports

    closeExports exports =
      let exports' =
            Map.fromList [(exportKey package modu, exportedScope package exports modu) | (package, modu) <- packageModules]
              `Map.union` depExports
       in if exports' == exports then exports else closeExports exports'
    exportKey package modu = ModuleKey package (moduleKey modu)

exportedScope :: Package -> ModuleExports -> Module -> Scope
exportedScope package exports modu =
  case moduleExports modu of
    Nothing -> topLevelScope package modu
    Just specs -> List.foldl' unionScope emptyScope (map exportSpecScope specs)
  where
    availableScope = topLevelScope package modu `unionScope` importedScope package exports modu

    exportSpecScope spec =
      case spec of
        ExportAnn _ inner -> exportSpecScope inner
        ExportModule _ exportModuleName -> lookupImportedModule package Nothing exportModuleName exports
        ExportVar _ _ name -> selectTerm (nameText name) availableScope
        ExportAbs _ _ name -> selectType (nameText name) availableScope
        ExportAll _ _ name -> selectTypeWithMembers (nameText name) availableScope (allTypeMembers (nameText name) availableScope)
        ExportWith _ _ name members -> selectTypeWithMembers (nameText name) availableScope (map exportBundledMemberName members)
        ExportWithAll _ _ name _ members ->
          selectTypeWithMembers (nameText name) availableScope (map exportBundledMemberName members <> allTypeMembers (nameText name) availableScope)

selectTerm :: Text -> Scope -> Scope
selectTerm name scope =
  emptyScope
    { scopeTerms = Map.filterWithKey (\n _ -> n == name) (scopeTerms scope),
      scopeFixities = Map.filterWithKey (\n _ -> n == name) (scopeFixities scope)
    }

selectType :: Text -> Scope -> Scope
selectType name scope =
  emptyScope
    { scopeTypes = Map.filterWithKey (\n _ -> n == name) (scopeTypes scope)
    }

selectTypeWithMembers :: Text -> Scope -> [Text] -> Scope
selectTypeWithMembers name scope members =
  selectType name scope
    `unionScope` emptyScope
      { scopeTerms = Map.filterWithKey (\n _ -> n `elem` members) (scopeTerms scope),
        scopeConstructors = Map.filterWithKey (\n _ -> n == name) (scopeConstructors scope),
        scopeRecordFields = Map.filterWithKey (\n _ -> n `elem` members) (scopeRecordFields scope),
        scopeMethods = Map.filterWithKey (\n _ -> n == name) (scopeMethods scope),
        scopeFixities = Map.filterWithKey (\n _ -> n `elem` members) (scopeFixities scope)
      }

allTypeMembers :: Text -> Scope -> [Text]
allTypeMembers name scope =
  constructors <> recordFields <> methods
  where
    constructors = Map.findWithDefault [] name (scopeConstructors scope)
    recordFields = concatMap (\constructor -> Map.findWithDefault [] constructor (scopeRecordFields scope)) constructors
    methods = Map.findWithDefault [] name (scopeMethods scope)

exportBundledMemberName :: IEBundledMember -> Text
exportBundledMemberName = nameText . ieBundledMemberName

topLevelScope :: Package -> Module -> Scope
topLevelScope package modu =
  List.foldl' addDecl emptyScope (moduleDecls modu)
  where
    moduleKeyText = moduleKey modu
    qualify = ResolvedTopLevel (packageId package) . qualifyName (Just moduleKeyText)
    addDecl scope decl =
      let DeclExports termNames typeNames constructors recordFields methods fixities = declExportedNames decl
          scope' = List.foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
          scope'' = List.foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames
          scope''' = scope'' {scopeConstructors = constructors `Map.union` scopeConstructors scope''}
          scope'''' = scope''' {scopeRecordFields = recordFields `Map.union` scopeRecordFields scope'''}
          scope''''' = scope'''' {scopeMethods = methods `Map.union` scopeMethods scope''''}
       in scope''''' {scopeFixities = fixities `Map.union` scopeFixities scope'''''}

data DeclExports = DeclExports [UnqualifiedName] [UnqualifiedName] (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text OperatorFixity)

declExportedNames :: Decl -> DeclExports
declExportedNames decl =
  case decl of
    DeclAnn _ inner -> declExportedNames inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name _ -> DeclExports [name] [] Map.empty Map.empty Map.empty Map.empty
        PatternBind _ pat _ ->
          DeclExports (map snd (collectPatVarBinders NoSourceSpan pat)) [] Map.empty Map.empty Map.empty Map.empty
    DeclTypeSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty Map.empty
    DeclForeign foreignDecl
      | foreignDirection foreignDecl == ForeignImport ->
          DeclExports [foreignName foreignDecl] [] Map.empty Map.empty Map.empty Map.empty
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty
    DeclFixity assoc mNamespace mPrec ops
      | mNamespace /= Just IEEntityNamespaceType ->
          DeclExports
            []
            []
            Map.empty
            Map.empty
            Map.empty
            (Map.fromList [(renderUnqualifiedName op, OperatorFixity assoc (fromMaybe 9 mPrec)) | op <- ops])
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty
    DeclClass classDecl ->
      let className = binderHeadName (classDeclHead classDecl)
          methodNames = classDeclMethodNames (classDeclItems classDecl)
       in DeclExports
            methodNames
            [className]
            Map.empty
            Map.empty
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName methodNames))
            Map.empty
    DeclTypeData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      let typeName = binderHeadName (newtypeDeclHead newtypeDecl)
          termNames = maybe [] dataConDeclNames (newtypeDeclConstructor newtypeDecl)
          constructorNames = maybe [] dataConDeclConstructorNames (newtypeDeclConstructor newtypeDecl)
       in DeclExports termNames [typeName] (constructorMap typeName constructorNames) (maybe Map.empty (recordFieldMap . (: [])) (newtypeDeclConstructor newtypeDecl)) Map.empty Map.empty
    DeclDataFamilyDecl familyDecl ->
      DeclExports [] [binderHeadName (dataFamilyDeclHead familyDecl)] Map.empty Map.empty Map.empty Map.empty
    DeclDataFamilyInst familyInst -> dataFamilyInstExports familyInst
    DeclTypeSyn typeSynDecl -> DeclExports [] [binderHeadName (typeSynHead typeSynDecl)] Map.empty Map.empty Map.empty Map.empty
    _ -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty

dataFamilyInstExports :: DataFamilyInst -> DeclExports
dataFamilyInstExports familyInst =
  case dataFamilyHeadName (dataFamilyInstHead familyInst) of
    Nothing -> DeclExports termNames [] Map.empty recordFields Map.empty Map.empty
    Just familyName ->
      DeclExports
        termNames
        []
        (constructorMap familyName constructorNames)
        recordFields
        Map.empty
        Map.empty
  where
    constructors = dataFamilyInstConstructors familyInst
    termNames = dataDeclConstructorNames constructors
    constructorNames = concatMap dataConDeclConstructorNames constructors
    recordFields = recordFieldMap constructors

dataFamilyHeadName :: Type -> Maybe UnqualifiedName
dataFamilyHeadName ty =
  case peelTypeHead ty of
    TCon name _ -> Just (mkUnqualifiedName (nameType name) (nameText name))
    TApp function _ -> dataFamilyHeadName function
    TTypeApp function _ -> dataFamilyHeadName function
    _ -> Nothing

dataDeclExports :: BinderHead UnqualifiedName -> [DataConDecl] -> DeclExports
dataDeclExports headBinder constructors =
  let typeName = binderHeadName headBinder
   in DeclExports
        (dataDeclConstructorNames constructors)
        [typeName]
        (constructorMap typeName (concatMap dataConDeclConstructorNames constructors))
        (recordFieldMap constructors)
        Map.empty
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

moduleScope :: Package -> ModuleExports -> Module -> Scope
moduleScope packageId exports modu =
  ownScope `unionScope` importedScope packageId exports modu `unionScope` implicitPrelude `unionScope` builtinScope
  where
    ownScope = topLevelScope packageId modu
    preludeScope = lookupImportedModule packageId Nothing "Prelude" exports
    -- Implicit Prelude: names available unqualified AND as Prelude.xxx
    implicitPrelude = preludeScope {scopeQualifiedModules = Map.singleton "Prelude" preludeScope}

importedScope :: Package -> ModuleExports -> Module -> Scope
importedScope packageId exports modu =
  List.foldl' addImport emptyScope (moduleImports modu)
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
        imported = filterImportSpec (importDeclSpec importDecl) (lookupImportedModule packageId (importDeclPackage importDecl) originModule exports)

lookupImportedModule :: Package -> Maybe Text -> Text -> ModuleExports -> Scope
lookupImportedModule currentPackage requestedPackage moduleName' exports =
  case matchingScopes of
    [scope] -> scope
    _ -> emptyScope
  where
    matchingScopes = matchingModuleScopes currentPackage requestedPackage moduleName' exports

matchingModuleScopes :: Package -> Maybe Text -> Text -> ModuleExports -> [Scope]
matchingModuleScopes currentPackage requestedPackage moduleName' exports =
  [ scope
  | (ModuleKey package name, scope) <- Map.toList exports,
    name == moduleName',
    packageMatches package
  ]
  where
    packageMatches package = case requestedPackage of
      Nothing -> True
      Just "this" -> package == currentPackage
      Just requested -> requested == packageName package

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
              scopeFixities = Map.filterWithKey (\n _ -> n `elem` allowedTerms) (scopeFixities scope),
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
          members = allTypeMembers parentName scope
       in if null members then [parentName] else members

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
        ResolvedTopLevel packageId resolved -> ResolvedTopLevel packageId resolved
        other -> other

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

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
      scopeFixities = Map.empty,
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
    "Int8#",
    "Int16#",
    "Int32#",
    "Int64#",
    "Word#",
    "Word8#",
    "Word16#",
    "Word32#",
    "Word64#",
    "Char#",
    "Float#",
    "Double#",
    "Addr#",
    "Array#",
    "ByteArray#",
    "MutableArray#",
    "MutableByteArray#",
    "RealWorld",
    "State#",
    "ThreadId#",
    "TYPE",
    "RuntimeRep",
    "TupleRep",
    "[]",
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
      scopeFixities = scopeFixities left `Map.union` scopeFixities right,
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

lookupFixity :: Text -> Scope -> OperatorFixity
lookupFixity name scope =
  Map.findWithDefault defaultOperatorFixity name (scopeFixities scope)

defaultOperatorFixity :: OperatorFixity
defaultOperatorFixity = OperatorFixity InfixL 9

filterScopeByNames :: (Text -> Bool) -> Scope -> Scope
filterScopeByNames keep scope =
  Scope
    { scopeTerms = Map.filterWithKey (\name _ -> keep name) (scopeTerms scope),
      scopeTypes = Map.filterWithKey (\name _ -> keep name) (scopeTypes scope),
      scopeConstructors = Map.filterWithKey (\name _ -> keep name) (scopeConstructors scope),
      scopeRecordFields = Map.filterWithKey (\name _ -> keep name) (scopeRecordFields scope),
      scopeMethods = Map.filterWithKey (\name _ -> keep name) (scopeMethods scope),
      scopeFixities = Map.filterWithKey (\name _ -> keep name) (scopeFixities scope),
      scopeQualifiedModules = scopeQualifiedModules scope
    }

resolveFixityName :: Scope -> Name -> OperatorFixity
resolveFixityName scope name =
  case nameQualifier name of
    Just qualifier ->
      case Map.lookup qualifier (scopeQualifiedModules scope) of
        Nothing -> defaultOperatorFixity
        Just qualifiedScope -> lookupFixity (nameText name) qualifiedScope
    Nothing ->
      lookupFixity (nameText name) scope

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
