{-# LANGUAGE OverloadedStrings #-}

module Aihc.Resolve.Scope
  ( Scope (..),
    OperatorFixity (..),
    ModuleExports,
    moduleExportsFromList,
    moduleExportKeys,
    lookupModuleExport,
    filterModuleExports,
    isTermNamespace,
    ModuleKey (..),
    collectModuleExports,
    collectModuleExportsWithDeps,
    exportedLocalNames,
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
    recordWildcardFieldNames,
    importItemTypeName,
    allTypeMembers,
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
    Extension (..),
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
    InstanceDecl (..),
    InstanceDeclItem (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    PatSynArgs (..),
    PatSynDecl (..),
    Pattern (..),
    RecordField (..),
    SourceSpan,
    Type (..),
    TypeFamilyDecl (..),
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
import Control.DeepSeq (NFData)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data Scope = Scope
  { scopeTerms :: Map.Map Text ResolvedName,
    scopeTypes :: Map.Map Text ResolvedName,
    scopeConstructors :: Map.Map Text [Text],
    scopeRecordFields :: Map.Map Text [Text],
    scopeMethods :: Map.Map Text [Text],
    -- | Associated type families of a class, keyed by the class name.
    scopeAssociatedTypes :: Map.Map Text [Text],
    scopeFixities :: Map.Map Text OperatorFixity,
    scopeQualifiedModules :: Map.Map Text Scope
  }
  deriving (Eq, Generic)

instance NFData Scope

data OperatorFixity = OperatorFixity
  { operatorFixityAssoc :: !FixityAssoc,
    operatorFixityPrecedence :: !Int
  }
  deriving (Eq, Show, Read, Generic)

instance NFData OperatorFixity

data ModuleKey = ModuleKey
  { moduleKeyPackage :: !Package,
    moduleKeyName :: !Text
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData ModuleKey

-- | The scope each module makes visible to the modules that import it.
--
-- An import names a module and, at most, the package it wants the module
-- from, so the entries are keyed by module name first: an import looks at
-- the packages that define that name rather than at every module of every
-- package. Several packages can define a module of the same name, and an
-- import that does not say which one it means resolves only when exactly
-- one package is in play.
newtype ModuleExports = ModuleExports (Map.Map Text (Map.Map Package Scope))
  deriving (Eq, Generic)

instance NFData ModuleExports

-- | Left-biased, as 'Map.union' is: where both sides hold a scope for the
-- same module of the same package, the left one wins.
instance Semigroup ModuleExports where
  ModuleExports left <> ModuleExports right =
    ModuleExports (Map.unionWith Map.union left right)

instance Monoid ModuleExports where
  mempty = ModuleExports Map.empty

moduleExportsFromList :: [(ModuleKey, Scope)] -> ModuleExports
moduleExportsFromList entries =
  ModuleExports
    ( Map.fromListWith
        Map.union
        [(name, Map.singleton package scope) | (ModuleKey package name, scope) <- entries]
    )

-- | Every module the map holds a scope for.
moduleExportKeys :: ModuleExports -> [ModuleKey]
moduleExportKeys (ModuleExports byName) =
  [ModuleKey package name | (name, byPackage) <- Map.toList byName, package <- Map.keys byPackage]

lookupModuleExport :: ModuleKey -> ModuleExports -> Maybe Scope
lookupModuleExport (ModuleKey package name) (ModuleExports byName) =
  Map.lookup name byName >>= Map.lookup package

filterModuleExports :: (ModuleKey -> Bool) -> ModuleExports -> ModuleExports
filterModuleExports keep (ModuleExports byName) =
  ModuleExports
    ( Map.filter
        (not . Map.null)
        (Map.mapWithKey (\name -> Map.filterWithKey (\package _ -> keep (ModuleKey package name))) byName)
    )

collectModuleExports :: [ModuleUnit] -> ModuleExports
collectModuleExports = collectModuleExportsWithDeps mempty

-- | Extract interfaces for a compilation unit while allowing its explicit
-- export lists to re-export names supplied by predecessor units.
--
-- A module's exported scope is built from what it declares and what it
-- imports. When no module of the unit imports another, one pass over the
-- modules is exact, because every import is already in @depExports@.
--
-- A unit whose modules do import each other needs a fixed point: the first
-- pass sees a sibling's scope as empty. The fixed point only changes the
-- scopes of the modules of this unit. The scopes of the dependencies stay
-- as the caller gave them. The loop thus holds the local scopes alone and
-- compares only those. A comparison of the dependency scopes finds no
-- difference, and they are much more numerous than the local scopes.
collectModuleExportsWithDeps :: ModuleExports -> [ModuleUnit] -> ModuleExports
collectModuleExportsWithDeps depExports packageModules
  | any importsSibling packageModules = closeExports emptyLocalScopes
  | otherwise = exportScopes emptyLocalScopes
  where
    siblingNames =
      Set.fromList [moduleKey modu | ModuleUnit {moduleUnitAst = modu} <- packageModules]
    importsSibling ModuleUnit {moduleUnitExtensions = extensions, moduleUnitAst = modu} =
      any ((`Set.member` siblingNames) . importDeclModule) (moduleImports modu)
        || (moduleImportsImplicitPrelude extensions modu && Set.member "Prelude" siblingNames)

    emptyLocalScopes =
      moduleExportsFromList
        [ (exportKey package modu, emptyScope)
        | ModuleUnit {moduleUnitPackage = package, moduleUnitAst = modu} <- packageModules
        ]

    -- The exported scope of every module of the unit, each read against the
    -- scopes in hand for its siblings.
    exportScopes localScopes =
      let exports = localScopes <> depExports
       in moduleExportsFromList
            [ (exportKey package modu, exportedScope package exports extensions modu)
            | ModuleUnit {moduleUnitPackage = package, moduleUnitExtensions = extensions, moduleUnitAst = modu} <- packageModules
            ]

    closeExports localScopes =
      let localScopes' = exportScopes localScopes
       in if localScopes' == localScopes then localScopes else closeExports localScopes'

    exportKey package modu = ModuleKey package (moduleKey modu)

-- | The top-level names that one module makes visible to other modules,
-- each tagged with the namespace that an export item would have to pick to
-- name it: its values, data constructors, record selectors and class
-- methods in the term namespace, its type constructors, classes and
-- synonyms in the type namespace.
--
-- The tag matters because an export item names one namespace and not the
-- other: in @module M (X) where data X = X@ the item names the type
-- constructor alone, and @module M (X(X))@ is what also names the data
-- constructor.
--
-- A name counts when the export list of the module resolves it to a
-- definition in that same module; a module without an export list exports
-- every top-level name, which 'exportedScope' has already worked out. The
-- result is 'Nothing' when the map holds no scope for the module, which a
-- caller must read as \"assume every name is exported\".
exportedLocalNames :: Package -> Text -> ModuleExports -> Maybe (Set (ResolutionNamespace, Text))
exportedLocalNames package name exports =
  localNames <$> lookupModuleExport (ModuleKey package name) exports
  where
    localNames scope =
      Set.fromList
        [ (namespace, nameText resolved)
        | (namespace, entries) <-
            [ (ResolutionNamespaceTerm, scopeTerms scope),
              (ResolutionNamespaceType, scopeTypes scope)
            ],
          ResolvedTopLevel resolvedPackage resolvedModule resolved <- Map.elems entries,
          resolvedPackage == packageId package,
          resolvedModule == name
        ]

exportedScope :: Package -> ModuleExports -> [Extension] -> Module -> Scope
exportedScope package exports extensions modu =
  case moduleExports modu of
    Nothing -> ownScope
    Just specs -> withSeparatelyExportedMethods (List.foldl' unionScope emptyScope (map exportSpecScope specs))
  where
    (ownScope, imported) = ownAndImportedScopes package exports extensions modu
    availableScope = ownScope `unionScope` imported

    -- A class item without members exports the class alone, but a
    -- separate item can export a method of it. The method stays a method
    -- of the class: an import item @C(..)@ names it (Haskell 2010 5.3.1),
    -- and an instance of the class can bind it.
    withSeparatelyExportedMethods scope =
      scope {scopeMethods = Map.unionWith (\bundled separate -> List.nub (bundled <> separate)) (scopeMethods scope) separateMethods}
      where
        separateMethods =
          Map.fromList
            [ (className, exportedMethods)
            | (className, resolvedClass) <- Map.toList (scopeTypes scope),
              candidate <- availableScope : Map.elems (scopeQualifiedModules availableScope),
              lookupType className candidate == resolvedClass,
              Just methods <- [Map.lookup className (scopeMethods candidate)],
              let exportedMethods =
                    [ method
                    | method <- methods,
                      Just resolvedMethod <- [Map.lookup method (scopeTerms candidate)],
                      Map.lookup method (scopeTerms scope) == Just resolvedMethod
                    ],
              not (null exportedMethods)
            ]

    exportSpecScope spec =
      case spec of
        ExportAnn _ inner -> exportSpecScope inner
        ExportModule _ exportModuleName
          | exportModuleName == moduleKey modu -> ownScope `unionScope` reexportedQualifier exportModuleName
          | otherwise -> reexportedQualifier exportModuleName
        ExportVar _ _ name -> selectTerm (nameText name) (exportSource name)
        ExportAbs _ (Just namespace) name
          | isTermNamespace namespace -> selectTerm (nameText name) (exportSource name)
        ExportAbs _ _ name -> selectType (nameText name) (exportSource name)
        ExportAll _ _ name -> selectTypeWithMembers (nameText name) (bundledMemberScope name) (allTypeMembers (nameText name) (bundledMemberScope name))
        ExportWith _ _ name members -> selectTypeWithMembers (nameText name) (bundledMemberScope name) (map exportBundledMemberName members)
        ExportWithAll _ _ name _ members ->
          selectTypeWithMembers (nameText name) (bundledMemberScope name) (map exportBundledMemberName members <> allTypeMembers (nameText name) (bundledMemberScope name))

    -- @module X@ re-exports every name that the module has in scope both
    -- unqualified and qualified by @X@, where both spellings name the same
    -- entity (Haskell 2010 5.5.2). @X@ is an import qualifier rather than a
    -- module name: several imports can share one alias, and an import list
    -- or a @hiding@ clause on them narrows what the alias re-exports.
    reexportedQualifier qualifier =
      reexportedModuleScope
        (Map.findWithDefault emptyScope qualifier (scopeQualifiedModules availableScope))
        availableScope

    -- A qualified export item such as @L.smallChunkSize@ names an entity
    -- of the module that the qualifier imports. The module can also use
    -- its own name as the qualifier.
    exportSource name =
      case nameQualifier name of
        Nothing -> availableScope
        Just qualifier
          | qualifier == moduleKey modu -> availableScope
          | otherwise -> Map.findWithDefault availableScope qualifier (scopeQualifiedModules availableScope)

    -- The members that a type item bundles are entities of the type, not
    -- of the qualifier. A data instance of an imported family binds its
    -- constructors in this module, so @U.Vector (BitVec)@ names the family
    -- through the qualifier and the constructor unqualified. The scope of
    -- the qualifier comes first, so the type resolves as in 'exportSource'.
    bundledMemberScope name =
      case nameQualifier name of
        Nothing -> availableScope
        Just qualifier
          | qualifier == moduleKey modu -> availableScope
          | otherwise -> exportSource name `unionScope` availableScope

-- | The part of a qualified scope that a @module X@ export item names: the
-- entries that the unqualified scope resolves to the same entity. A name
-- that the qualifier brought in but that no unqualified import made visible
-- is not re-exported, and neither is one that some other entity shadows
-- unqualified.
reexportedModuleScope :: Scope -> Scope -> Scope
reexportedModuleScope qualified available =
  (filterScopeByNames reexported qualified) {scopeQualifiedModules = Map.empty}
  where
    reexported name =
      sameEntity (Map.lookup name (scopeTerms qualified)) (Map.lookup name (scopeTerms available))
        || sameEntity (Map.lookup name (scopeTypes qualified)) (Map.lookup name (scopeTypes available))
    sameEntity (Just qualifiedName) (Just availableName) = qualifiedName == availableName
    sameEntity _ _ = False

selectTerm :: Text -> Scope -> Scope
selectTerm name scope =
  emptyScope
    { scopeTerms = restrictToKey name (scopeTerms scope),
      scopeFixities = restrictToKey name (scopeFixities scope)
    }

selectType :: Text -> Scope -> Scope
selectType name scope =
  emptyScope
    { scopeTypes = restrictToKey name (scopeTypes scope)
    }

-- | The entry a map holds for one key, as a map of its own. An export or
-- import item names one entity, so this is a lookup rather than a walk over
-- the whole scope.
restrictToKey :: Text -> Map.Map Text value -> Map.Map Text value
restrictToKey name entries =
  case Map.lookup name entries of
    Nothing -> Map.empty
    Just value -> Map.singleton name value

-- | Select a type with its bundled members. A bundled member that is a
-- term but not a constructor, a record field, or a method of the type is a
-- pattern synonym. It becomes a constructor of the exported type.
selectTypeWithMembers :: Text -> Scope -> [Text] -> Scope
selectTypeWithMembers name scope members =
  selectType name scope
    `unionScope` emptyScope
      { scopeTerms = Map.restrictKeys (scopeTerms scope) memberSet,
        scopeTypes = Map.restrictKeys (scopeTypes scope) (Set.fromList bundledAssociatedTypes),
        scopeConstructors = bundledConstructors,
        scopeRecordFields = Map.restrictKeys (scopeRecordFields scope) memberSet,
        scopeMethods = restrictToKey name (scopeMethods scope),
        scopeAssociatedTypes =
          if null bundledAssociatedTypes then Map.empty else Map.singleton name bundledAssociatedTypes,
        scopeFixities = Map.restrictKeys (scopeFixities scope) memberSet
      }
  where
    memberSet = Set.fromList members
    existingConstructors = Map.findWithDefault [] name (scopeConstructors scope)
    bundledAssociatedTypes =
      [member | member <- associatedTypeMembers name scope, member `Set.member` memberSet]
    knownMembers =
      Set.fromList
        ( existingConstructors
            <> concat (Map.elems (scopeRecordFields scope))
            <> concat (Map.elems (scopeMethods scope))
            <> bundledAssociatedTypes
        )
    bundledPatternSynonyms =
      List.nub [member | member <- members, member `Set.notMember` knownMembers, Map.member member (scopeTerms scope)]
    bundledConstructors
      | Map.member name (scopeConstructors scope) || not (null bundledPatternSynonyms) =
          Map.singleton name (existingConstructors <> bundledPatternSynonyms)
      | otherwise = Map.empty

allTypeMembers :: Text -> Scope -> [Text]
allTypeMembers name scope =
  constructors <> recordFields <> methods <> associatedTypeMembers name scope
  where
    constructors = Map.findWithDefault [] name (scopeConstructors scope)
    recordFields = concatMap (\constructor -> Map.findWithDefault [] constructor (scopeRecordFields scope)) constructors
    methods = Map.findWithDefault [] name (scopeMethods scope)

-- | The associated type families that a class bundles in an export or
-- import item such as @C(..)@.
associatedTypeMembers :: Text -> Scope -> [Text]
associatedTypeMembers name scope = Map.findWithDefault [] name (scopeAssociatedTypes scope)

-- | The @pattern@ and @data@ namespace keywords select a term in an import
-- or export list.
isTermNamespace :: IEEntityNamespace -> Bool
isTermNamespace namespace =
  case namespace of
    IEEntityNamespaceType -> False
    IEEntityNamespacePattern -> True
    IEEntityNamespaceData -> True

exportBundledMemberName :: IEBundledMember -> Text
exportBundledMemberName = nameText . ieBundledMemberName

-- | The top-level scope of one module. The first argument holds the record
-- fields of each constructor that the module imports. A record wildcard in a
-- top-level pattern binding binds one name for each such field.
topLevelScope :: Map.Map Text [Text] -> Package -> Module -> Scope
topLevelScope importedFields package modu =
  List.foldl' addDecl emptyScope (moduleDecls modu)
  where
    moduleKeyText = moduleKey modu
    qualify = ResolvedTopLevel (packageId package) moduleKeyText . qualifyName Nothing
    -- A pattern binding can come before the data declaration that gives it
    -- the record fields, so collect all fields of the module first.
    visibleFields =
      Map.unions
        ( [fields | decl <- moduleDecls modu, let DeclExports _ _ _ fields _ _ _ = declExportedNames Map.empty decl]
            <> [importedFields]
        )
    addDecl scope decl =
      let DeclExports termNames typeNames constructors recordFields methods associatedTypes fixities = declExportedNames visibleFields decl
          scope' = List.foldl' (\acc name -> insertTerm (renderUnqualifiedName name) (qualify name) acc) scope termNames
          scope'' = List.foldl' (\acc name -> insertType (renderUnqualifiedName name) (qualify name) acc) scope' typeNames
          -- Each data instance of a data family adds constructors to the
          -- entry of the family, so keep the constructors of all instances.
          scope''' = scope'' {scopeConstructors = Map.unionWith (<>) (scopeConstructors scope'') constructors}
          scope'''' = scope''' {scopeRecordFields = recordFields `Map.union` scopeRecordFields scope'''}
          scope''''' = scope'''' {scopeMethods = methods `Map.union` scopeMethods scope''''}
          scope'''''' = scope''''' {scopeAssociatedTypes = associatedTypes `Map.union` scopeAssociatedTypes scope'''''}
       in scope'''''' {scopeFixities = fixities `Map.union` scopeFixities scope''''''}

-- | The names that one declaration adds to the module scope: terms, types,
-- constructors by type, record fields by constructor, methods by class,
-- associated type families by class, and operator fixities.
data DeclExports = DeclExports [UnqualifiedName] [UnqualifiedName] (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text [Text]) (Map.Map Text OperatorFixity)

declExportedNames :: Map.Map Text [Text] -> Decl -> DeclExports
declExportedNames recordFields decl =
  case decl of
    DeclAnn _ inner -> declExportedNames recordFields inner
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name _ -> DeclExports [name] [] Map.empty Map.empty Map.empty Map.empty Map.empty
        PatternBind _ pat _ ->
          DeclExports (map snd (collectPatVarBinders recordFields Nothing pat)) [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclTypeSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclForeign foreignDecl
      | foreignDirection foreignDecl == ForeignImport ->
          DeclExports [foreignName foreignDecl] [] Map.empty Map.empty Map.empty Map.empty Map.empty
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclFixity assoc mNamespace mPrec ops
      | mNamespace /= Just IEEntityNamespaceType ->
          DeclExports
            []
            []
            Map.empty
            Map.empty
            Map.empty
            Map.empty
            (Map.fromList [(renderUnqualifiedName op, OperatorFixity assoc (fromMaybe 9 mPrec)) | op <- ops])
      | otherwise -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclClass classDecl ->
      let className = binderHeadName (classDeclHead classDecl)
          methodNames = classDeclMethodNames (classDeclItems classDecl)
          associatedNames = classDeclAssociatedTypeNames (classDeclItems classDecl)
       in DeclExports
            methodNames
            (className : associatedNames)
            Map.empty
            Map.empty
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName methodNames))
            (Map.singleton (renderUnqualifiedName className) (map renderUnqualifiedName associatedNames))
            Map.empty
    DeclTypeData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclData dataDecl ->
      dataDeclExports (dataDeclHead dataDecl) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      let typeName = binderHeadName (newtypeDeclHead newtypeDecl)
          constructors = maybeToList (newtypeDeclConstructor newtypeDecl)
       in DeclExports
            (dataDeclTermNames constructors)
            [typeName]
            (constructorMap typeName (concatMap dataConDeclConstructors constructors))
            (recordFieldMap constructors)
            Map.empty
            Map.empty
            Map.empty
    DeclDataFamilyDecl familyDecl ->
      DeclExports [] [binderHeadName (dataFamilyDeclHead familyDecl)] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclDataFamilyInst familyInst -> dataFamilyInstExports familyInst
    DeclTypeFamilyDecl familyDecl ->
      case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
        Just name -> DeclExports [] [name] Map.empty Map.empty Map.empty Map.empty Map.empty
        Nothing -> DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclTypeSyn typeSynDecl -> DeclExports [] [binderHeadName (typeSynHead typeSynDecl)] Map.empty Map.empty Map.empty Map.empty Map.empty
    DeclPatSyn patSyn ->
      let name = patSynDeclName patSyn
          fields = patSynFieldNames patSyn
       in DeclExports (name : fields) [] Map.empty (patSynRecordFieldMap name fields) Map.empty Map.empty Map.empty
    DeclPatSynSig names _ -> DeclExports names [] Map.empty Map.empty Map.empty Map.empty Map.empty
    -- The data family instances of a class instance bind their constructors
    -- and record fields at the top level, as @data instance@ declarations do.
    DeclInstance instanceDecl ->
      foldr unionDeclExports noDeclExports (mapMaybe instanceItemDataFamilyExports (instanceDeclItems instanceDecl))
    _ -> noDeclExports

noDeclExports :: DeclExports
noDeclExports = DeclExports [] [] Map.empty Map.empty Map.empty Map.empty Map.empty

unionDeclExports :: DeclExports -> DeclExports -> DeclExports
unionDeclExports (DeclExports leftTerms leftTypes leftConstructors leftFields leftMethods leftAssociated leftFixities) (DeclExports rightTerms rightTypes rightConstructors rightFields rightMethods rightAssociated rightFixities) =
  DeclExports
    (leftTerms <> rightTerms)
    (leftTypes <> rightTypes)
    (Map.unionWith (<>) leftConstructors rightConstructors)
    (Map.union leftFields rightFields)
    (Map.union leftMethods rightMethods)
    (Map.union leftAssociated rightAssociated)
    (Map.union leftFixities rightFixities)

-- | The names a data family instance inside a class instance binds.
instanceItemDataFamilyExports :: InstanceDeclItem -> Maybe DeclExports
instanceItemDataFamilyExports item =
  case item of
    InstanceItemAnn _ inner -> instanceItemDataFamilyExports inner
    InstanceItemDataFamilyInst familyInst -> Just (dataFamilyInstExports familyInst)
    _ -> Nothing

-- | The field selectors of a record pattern synonym.
patSynFieldNames :: PatSynDecl -> [UnqualifiedName]
patSynFieldNames patSyn =
  case patSynDeclArgs patSyn of
    PatSynRecordArgs fields -> map (mkUnqualifiedName NameVarId) fields
    _ -> []

patSynRecordFieldMap :: UnqualifiedName -> [UnqualifiedName] -> Map.Map Text [Text]
patSynRecordFieldMap name fields
  | null fields = Map.empty
  | otherwise = Map.singleton (renderUnqualifiedName name) (map renderUnqualifiedName fields)

dataFamilyInstExports :: DataFamilyInst -> DeclExports
dataFamilyInstExports familyInst =
  case typeFamilyHeadName (dataFamilyInstHead familyInst) of
    Nothing -> DeclExports termNames [] Map.empty recordFields Map.empty Map.empty Map.empty
    Just familyName ->
      DeclExports
        termNames
        []
        (constructorMap familyName constructorNames)
        recordFields
        Map.empty
        Map.empty
        Map.empty
  where
    constructors = dataFamilyInstConstructors familyInst
    termNames = dataDeclTermNames constructors
    constructorNames = concatMap dataConDeclConstructors constructors
    recordFields = recordFieldMap constructors

typeFamilyHeadName :: Type -> Maybe UnqualifiedName
typeFamilyHeadName ty =
  case peelTypeHead ty of
    TCon name _ -> Just (mkUnqualifiedName (nameType name) (nameText name))
    TInfix _ name _ _ -> Just (mkUnqualifiedName (nameType name) (nameText name))
    TApp function _ -> typeFamilyHeadName function
    TTypeApp function _ -> typeFamilyHeadName function
    _ -> Nothing

dataDeclExports :: BinderHead UnqualifiedName -> [DataConDecl] -> DeclExports
dataDeclExports headBinder constructors =
  let typeName = binderHeadName headBinder
   in DeclExports
        (dataDeclTermNames constructors)
        [typeName]
        (constructorMap typeName (concatMap dataConDeclConstructors constructors))
        (recordFieldMap constructors)
        Map.empty
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

-- | The associated type and data families that a class declares.
classDeclAssociatedTypeNames :: [ClassDeclItem] -> [UnqualifiedName]
classDeclAssociatedTypeNames = concatMap go
  where
    go (ClassItemAnn _ inner) = go inner
    go (ClassItemTypeFamilyDecl familyDecl) = maybeToList (typeFamilyHeadName (typeFamilyDeclHead familyDecl))
    go (ClassItemDataFamilyDecl familyDecl) = [binderHeadName (dataFamilyDeclHead familyDecl)]
    go _ = []

-- | The term names that constructor declarations bind: the constructors
-- that the source names and their record field selectors.
dataDeclTermNames :: [DataConDecl] -> [UnqualifiedName]
dataDeclTermNames constructors =
  concatMap dataConDeclConstructors constructors
    <> concatMap (concatMap fieldNames . snd) (concatMap dataConDeclRecordFields constructors)

-- | The constructors that one declaration names. Built-in syntax such as
-- @(,)@, @(# | #)@ and @[]@ binds no name: a use of it resolves to
-- 'ResolvedSyntax' without consulting the scope, so the scope holds no
-- entry for it.
dataConDeclConstructors :: DataConDecl -> [UnqualifiedName]
dataConDeclConstructors dataConDecl =
  case dataConDecl of
    DataConAnn _ inner -> dataConDeclConstructors inner
    PrefixCon _ _ name _ -> [name]
    InfixCon _ _ _ name _ -> [name]
    RecordCon _ _ name _ -> [name]
    GadtCon _ _ names _ -> names
    TupleCon {} -> []
    UnboxedSumCon {} -> []
    ListCon {} -> []

dataConDeclRecordFields :: DataConDecl -> [(UnqualifiedName, [FieldDecl])]
dataConDeclRecordFields dataConDecl =
  let go d =
        case d of
          DataConAnn _ inner -> go inner
          RecordCon _ _ name fields -> [(name, fields)]
          GadtCon _ _ names (GadtRecordBody fields _) -> [(name, fields) | name <- names]
          _ -> []
   in go dataConDecl

-- | The names in scope inside one module. The extensions are the ones the
-- driver decided for the module; 'moduleImportsImplicitPrelude' reads them
-- rather than the module's pragmas.
--
-- The builtin scope comes from the caller. The resolver does not know which
-- module defines a builtin name.
moduleScope :: Scope -> Package -> ModuleExports -> [Extension] -> Module -> Scope
moduleScope builtins packageId exports extensions modu =
  ownScope
    `unionScope` imported
    `unionScope` implicitSyntaxScope builtins
    `unionScope` builtinScope
  where
    (unqualifiedOwnScope, imported) = ownAndImportedScopes packageId exports extensions modu
    -- A module's own top-level names are also in scope qualified by the
    -- module name, so @M.x@ inside module @M@ names the local @x@.
    ownScope = insertQualifiedModule (moduleKey modu) unqualifiedOwnScope unqualifiedOwnScope

-- | The names from the builtin scope that the syntax reaches without an
-- import.
--
-- The list constructor @:@ is an ordinary infix constructor. The empty list
-- is built-in syntax and needs no scope entry. Equality syntax uses the type
-- @~@. Imported and local names shadow these names.
implicitSyntaxScope :: Scope -> Scope
implicitSyntaxScope builtins =
  selectTerm ":" builtins `unionScope` selectType "~" builtins

-- | What a module's own declarations bind, and what its imports bring in.
--
-- The two come together because the own scope needs the imported one: a
-- record wildcard in a top-level pattern binding binds one name for each
-- field of the constructor, and the constructor can be an imported one.
-- Both callers need both scopes, and the imports are the expensive half.
ownAndImportedScopes :: Package -> ModuleExports -> [Extension] -> Module -> (Scope, Scope)
ownAndImportedScopes packageId exports extensions modu = (ownScope, imported)
  where
    imported = importedScope packageId exports modu `unionScope` implicitPrelude
    ownScope = topLevelScope (scopeRecordFields imported) packageId modu
    preludeScope = lookupImportedModule packageId Nothing "Prelude" exports
    -- Implicit imports supply both unqualified names and the Prelude qualifier.
    -- Export lists must see the same imported names as module bodies.
    implicitPrelude
      | moduleImportsImplicitPrelude extensions modu =
          preludeScope {scopeQualifiedModules = Map.singleton "Prelude" preludeScope}
      | otherwise = emptyScope

-- | Whether the module gets the implicit Prelude import.
--
-- NoImplicitPrelude removes the implicit import.
-- RebindableSyntax implies NoImplicitPrelude.
-- An explicit Prelude import replaces the implicit import.
moduleImportsImplicitPrelude :: [Extension] -> Module -> Bool
moduleImportsImplicitPrelude extensions modu =
  ImplicitPrelude `elem` extensions && not explicitPreludeImport
  where
    explicitPreludeImport = any ((== "Prelude") . importDeclModule) (moduleImports modu)

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
matchingModuleScopes currentPackage requestedPackage moduleName' (ModuleExports byName) =
  case Map.lookup moduleName' byName of
    Nothing -> []
    Just byPackage ->
      case requestedPackage of
        Nothing -> Map.elems byPackage
        Just "this" -> maybeToList (Map.lookup currentPackage byPackage)
        Just requested ->
          [scope | (package, scope) <- Map.toList byPackage, packageName package == requested]

filterImportSpec :: Maybe ImportSpec -> Scope -> Scope
filterImportSpec maybeSpec scope =
  case maybeSpec of
    Nothing -> scope
    Just ImportSpec {importSpecHiding = False, importSpecItems} ->
      let allowedTypes = Set.fromList (allowedTypeNames scope importSpecItems)
          allowedTerms = Set.fromList (allowedTermNames scope importSpecItems)
       in Scope
            { scopeTerms = Map.restrictKeys (scopeTerms scope) allowedTerms,
              scopeTypes = Map.restrictKeys (scopeTypes scope) allowedTypes,
              scopeConstructors = Map.restrictKeys (scopeConstructors scope) allowedTypes,
              scopeRecordFields = Map.restrictKeys (scopeRecordFields scope) allowedTerms,
              scopeMethods = Map.restrictKeys (scopeMethods scope) allowedTypes,
              scopeAssociatedTypes =
                Map.map (filter (`Set.member` allowedTypes)) (Map.restrictKeys (scopeAssociatedTypes scope) allowedTypes),
              scopeFixities = Map.restrictKeys (scopeFixities scope) allowedTerms,
              scopeQualifiedModules = scopeQualifiedModules scope
            }
    Just ImportSpec {importSpecHiding = True, importSpecItems} ->
      let hidden = Set.fromList (allowedTypeNames scope importSpecItems <> allowedTermNames scope importSpecItems)
       in scope
            { scopeTerms = Map.withoutKeys (scopeTerms scope) hidden,
              scopeTypes = Map.withoutKeys (scopeTypes scope) hidden,
              scopeConstructors = Map.withoutKeys (scopeConstructors scope) hidden,
              scopeRecordFields = Map.withoutKeys (scopeRecordFields scope) hidden,
              scopeMethods = Map.withoutKeys (scopeMethods scope) hidden,
              scopeAssociatedTypes = Map.withoutKeys (scopeAssociatedTypes scope) hidden,
              scopeFixities = Map.withoutKeys (scopeFixities scope) hidden
            }

-- | The type names that an import list admits. A bundled member of a class
-- item that is an associated type family of the class is a type name.
allowedTypeNames :: Scope -> [ImportItem] -> [Text]
allowedTypeNames scope = concatMap allowedTypeNamesForItem
  where
    allowedTypeNamesForItem item =
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let parentName = renderUnqualifiedName itemName
              associated = associatedTypeMembers parentName scope
           in parentName : filter (`elem` associated) (bundledImportMembers scope item)

allowedTermNames :: Scope -> [ImportItem] -> [Text]
allowedTermNames scope = concatMap (allowedTermNamesForItem scope)

allowedTermNamesForItem :: Scope -> ImportItem -> [Text]
allowedTermNamesForItem scope item =
  case item of
    ImportAnn _ sub -> allowedTermNamesForItem scope sub
    ImportItemVar _ itemName -> [renderUnqualifiedName itemName]
    ImportItemAbs (Just namespace) itemName
      | isTermNamespace namespace -> [renderUnqualifiedName itemName]
    ImportItemAbs {} -> []
    _ ->
      case importItemTypeName item of
        Nothing -> []
        Just itemName ->
          let associated = associatedTypeMembers (renderUnqualifiedName itemName) scope
           in filter (`notElem` associated) (bundledImportMembers scope item)

-- | The bundled members that an import item names, with @(..)@ expanded
-- to every member of the parent. A parent without members stands for
-- itself.
bundledImportMembers :: Scope -> ImportItem -> [Text]
bundledImportMembers scope item =
  case item of
    ImportAnn _ sub -> bundledImportMembers scope sub
    ImportItemAll _ itemName -> allBundledMembers itemName
    ImportItemWith _ _ members -> map bundledMemberName members
    ImportItemAllWith _ itemName _ members -> map bundledMemberName members <> allBundledMembers itemName
    _ -> []
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
    ImportItemAbs (Just namespace) _
      | isTermNamespace namespace -> Nothing
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
        resolved@ResolvedTopLevel {} -> resolved
        other -> other

moduleKey :: Module -> Text
moduleKey modu = fromMaybe (T.pack "Main") (moduleName modu)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Scope containing fixed Haskell names that are always available.
--
-- The term namespace is empty.
-- Promoted constructors from @aihc-prim@ use ordinary name resolution.
-- The type namespace contains only the function arrow.
-- Types from @aihc-prim@ use ordinary name resolution.
--
-- This scope is merged into every module's scope unconditionally (lowest
-- priority — user-defined and imported names shadow it).
builtinScope :: Scope
builtinScope =
  Scope
    { scopeTerms = Map.empty,
      scopeTypes = Map.fromList (map mkBuiltinType builtinTypeNames),
      scopeConstructors = Map.empty,
      scopeRecordFields = Map.empty,
      scopeMethods = Map.empty,
      scopeAssociatedTypes = Map.empty,
      scopeFixities = Map.empty,
      scopeQualifiedModules = Map.empty
    }
  where
    mkBuiltinType n = (n, ResolvedSyntax)

-- | Wired-in type-namespace names.
--
-- Note: names here must match exactly what the parser emits as the 'Name'
-- text inside 'TCon'.  For example, the function arrow appears as @"->"@
-- (not @"(->)"@).
builtinTypeNames :: [T.Text]
builtinTypeNames =
  ["->"]

unionScope :: Scope -> Scope -> Scope
unionScope left right =
  Scope
    { scopeTerms = scopeTerms left `Map.union` scopeTerms right,
      scopeTypes = scopeTypes left `Map.union` scopeTypes right,
      scopeConstructors = Map.unionWithKey sameTypeConstructors (scopeConstructors left) (scopeConstructors right),
      scopeRecordFields = scopeRecordFields left `Map.union` scopeRecordFields right,
      scopeMethods = scopeMethods left `Map.union` scopeMethods right,
      scopeAssociatedTypes = scopeAssociatedTypes left `Map.union` scopeAssociatedTypes right,
      scopeFixities = scopeFixities left `Map.union` scopeFixities right,
      scopeQualifiedModules = scopeQualifiedModules left `Map.union` scopeQualifiedModules right
    }
  where
    -- Two scopes can have constructors for the same data family, for
    -- example from two modules that each declare a data instance. A module
    -- that declares a data instance of an imported family has constructors
    -- for the family but no type entry for it. If the two type entries are
    -- different types, the left scope shadows the right scope. Otherwise,
    -- keep the constructors of both scopes.
    sameTypeConstructors name leftConstructors rightConstructors =
      case (Map.lookup name (scopeTypes left), Map.lookup name (scopeTypes right)) of
        (Just leftType, Just rightType)
          | leftType /= rightType -> leftConstructors
        _ -> List.nub (leftConstructors <> rightConstructors)

insertTerm :: Text -> ResolvedName -> Scope -> Scope
insertTerm name resolved scope = scope {scopeTerms = Map.insert name resolved (scopeTerms scope)}

insertType :: Text -> ResolvedName -> Scope -> Scope
insertType name resolved scope = scope {scopeTypes = Map.insert name resolved (scopeTypes scope)}

-- | Add names from one qualified import. Combine scopes that share an alias.
insertQualifiedModule :: Text -> Scope -> Scope -> Scope
insertQualifiedModule qualifier imported scope =
  scope
    { scopeQualifiedModules =
        Map.insertWith unionScope qualifier imported (scopeQualifiedModules scope)
    }

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
      scopeAssociatedTypes = Map.filterWithKey (\name _ -> keep name) (scopeAssociatedTypes scope),
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

-- | The term variables that a pattern binds.
--
-- This is the definition side of the same walk that @bindPattern@ does in
-- "Aihc.Resolve". The two walks must agree. Thus this match gives one case
-- for each pattern form and has no catch-all case. If the pattern syntax
-- gets a new form, the compiler reports this function.
--
-- The first argument holds the record fields of each constructor. A record
-- wildcard binds one variable for each field that the pattern does not
-- list, and only the constructor gives those field names.
collectPatVarBinders :: Map.Map Text [Text] -> Maybe SourceSpan -> Pattern -> [(Maybe SourceSpan, UnqualifiedName)]
collectPatVarBinders recordFields ambient pat =
  case peelPatternAnn pat of
    PVar name -> [binderAt name]
    PAs alias inner -> binderAt alias : go inner
    PRecord conName fields wildcard ->
      concatMap (go . recordFieldValue) fields
        <> map (binderAt . mkUnqualifiedName NameVarId) (recordWildcardFieldNames recordFields conName fields wildcard)
    PTuple _ pats -> concatMap go pats
    PUnboxedSum _ _ inner -> go inner
    PList pats -> concatMap go pats
    PParen inner -> go inner
    PTypeSig inner _ -> go inner
    PView _ inner -> go inner
    PStrict inner -> go inner
    PIrrefutable inner -> go inner
    PInfix left _ right -> go left <> go right
    PCon _ _ pats -> concatMap go pats
    PBuiltinCon _ _ pats -> concatMap go pats
    -- 'peelPatternAnn' removes each annotation, but the match must name
    -- this form to stay exhaustive.
    PAnn _ inner -> go inner
    -- These forms bind no term variable.
    PTypeBinder _ -> []
    PTypeSyntax _ _ -> []
    PWildcard -> []
    PLit _ -> []
    PNegLit _ -> []
    PQuasiQuote _ _ -> []
    PSplice _ -> []
  where
    go = collectPatVarBinders recordFields ambient
    binderAt name = (spanStartNameSpan ambient (renderUnqualifiedName name), name)

-- | The fields that a record wildcard @..@ stands for: each field of the
-- constructor that the pattern or the construction does not list.
recordWildcardFieldNames :: Map.Map Text [Text] -> Name -> [RecordField a] -> Bool -> [Text]
recordWildcardFieldNames recordFields conName fields wildcard
  | not wildcard = []
  | otherwise = filter (`notElem` explicitFields) (Map.findWithDefault [] (nameText conName) recordFields)
  where
    explicitFields = map (nameText . recordFieldName) fields
