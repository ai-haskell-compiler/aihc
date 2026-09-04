{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST
-- and produces the same AST annotated with typing information. It does
-- not transform the tree structure.
--
-- The implementation follows the OutsideIn(X) algorithm:
--
-- 1. Generate wanted constraints by walking the AST.
-- 2. Solve the constraints using the worklist/inert-set architecture.
-- 3. Zonk meta-variables.
-- 4. Attach type annotations to AST nodes.
module Aihc.Tc
  ( -- * Entry point
    typecheckExpr,
    typecheckModulesWithInterface,
    typecheckModuleSccWithInterface,

    -- * Result types
    TcResult (..),
    TcConfig,
    tcConfig,
    TcBindingResult (..),
    defaultMethodName,
    TcTermKey (..),
    tcTermKeyIdentifier,
    TcInterface (..),
    emptyTcInterface,
    mergeTcInterfaces,
    restrictTcInterfaceToModules,
    tcInterfaceBindings,

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleClasses,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    TcTypeKey,
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (..),
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    TyVarId (..),
    tvKind,
    TypeScheme (..),
    boxedTupleTyConName,
    Pred (..),
    InstanceInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    dataTypeKey,
    DataConInfo (..),
    PatSynDirection (..),
    PatSynInfo (..),
    patSynKey,
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConSourceForm (..),
    dataConArgTypes,
    dataFamilyAxiomKey,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
    TypeFamilyInstanceInfo (..),
    typeFamilyAxiomKey,
    typeFamilyAxiomName,
    ClassInfo (..),
    TyConFlavor (..),
    TyConInfo (..),
    Unique (..),
    typeKindType,
    typeKindInEnv,
    runtimeRepOfTypeInEnv,
    isLiftedTypeInEnv,
    isUnliftedTypeInEnv,
    TcAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcStockDerivingPlan (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderPred,
    renderTcSignature,
    renderTcType,
    renderTcTypeInModule,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    Extension (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (PackageId (..), ResolutionNamespace)
import Aihc.Tc.Annotations (TcAnnotation (..), TcDerivingAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDerivingStrategy (..), TcStockDerivingPlan (..), renderPred, renderTcSignature, renderTcType, renderTcTypeInModule)
import Aihc.Tc.Env (ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), InstanceInfo (..), PatSynDirection (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), classInfoKey, dataConArgTypes, dataFamilyAxiomKey, dataFamilyAxiomName, dataFamilyRepresentationName, dataTypeKey, instanceEnvFromList, instanceEnvList, instanceInfoKey, typeFamilyAxiomKey, typeFamilyAxiomName)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), defaultMethodName, moduleBindings, moduleClasses, moduleInstances, tcModule, tcModuleScc)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Generic (everything, everywhereM)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (finalizeDiagnostics, zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)

-- | Result of type checking.
data TcResult = TcResult
  { -- | The inferred type of the top-level expression or binding.
    tcResultType :: !TcType,
    -- | Diagnostics (errors and warnings) produced.
    tcResultDiagnostics :: ![TcDiagnostic],
    -- | Whether type checking succeeded (no errors).
    tcResultSuccess :: !Bool
  }
  deriving (Show)

-- | The complete semantic interface shared between independently checked
-- module groups. Implementations never cross this boundary: only the facts
-- needed to type-check downstream source are retained.
data TcInterface = TcInterface
  { tcInterfaceTerms :: ![(TcTermKey, TypeScheme)],
    tcInterfaceTyCons :: ![TyConInfo],
    tcInterfaceDataTypes :: ![DataTypeInfo],
    tcInterfaceClasses :: ![ClassInfo],
    tcInterfaceInstances :: ![InstanceInfo],
    tcInterfaceDataFamilyInstances :: ![DataFamilyInstanceInfo],
    tcInterfaceTypeFamilyInstances :: ![TypeFamilyInstanceInfo],
    tcInterfacePatSyns :: ![PatSynInfo]
  }
  deriving (Eq, Show, Read)

emptyTcInterface :: TcInterface
emptyTcInterface =
  TcInterface
    { tcInterfaceTerms = [],
      tcInterfaceTyCons = [],
      tcInterfaceDataTypes = [],
      tcInterfaceClasses = [],
      tcInterfaceInstances = [],
      tcInterfaceDataFamilyInstances = [],
      tcInterfaceTypeFamilyInstances = [],
      tcInterfacePatSyns = []
    }

instance Semigroup TcInterface where
  left <> right = mergeTcInterfaces [left, right]

instance Monoid TcInterface where
  mempty = emptyTcInterface

mergeTcInterfaces :: [TcInterface] -> TcInterface
mergeTcInterfaces interfaces =
  TcInterface
    { tcInterfaceTerms = mergeInterfaceEntries "term interface" (termMergeKey . fst) (concatMap tcInterfaceTerms interfaces),
      tcInterfaceTyCons = mergeTyConInfos (concatMap tcInterfaceTyCons interfaces),
      tcInterfaceDataTypes = mergeInterfaceEntries "data type interface" (tyConMergeKey . dtiTyCon) (concatMap tcInterfaceDataTypes interfaces),
      tcInterfaceClasses = mergeInterfaceEntries "class interface" (tyConMergeKey . ciTyCon) (concatMap tcInterfaceClasses interfaces),
      tcInterfaceInstances = mergeInstanceInfos (concatMap tcInterfaceInstances interfaces),
      tcInterfaceDataFamilyInstances = mergeInterfaceEntries "data family instance interface" (axiomMergeKey . dataFamilyAxiomKey) (concatMap tcInterfaceDataFamilyInstances interfaces),
      tcInterfaceTypeFamilyInstances = mergeInterfaceEntries "type family instance interface" (axiomMergeKey . typeFamilyAxiomKey) (concatMap tcInterfaceTypeFamilyInstances interfaces),
      tcInterfacePatSyns = mergeInterfaceEntries "pattern synonym interface" (termMergeKey . patSynKey) (concatMap tcInterfacePatSyns interfaces)
    }

-- The merge keys put the most distinct component of an identity first, so a
-- map comparison stops early. Most entries share a package and a module. The
-- keys do not change the order of the merged entries.

termMergeKey :: TcTermKey -> Either Int (Text, Text, PackageId)
termMergeKey key =
  case key of
    TcTermLocal unique -> Left unique
    TcTermGlobal package moduleName' identifier -> Right (identifier, moduleName', package)

tyConMergeKey :: TyCon -> (Text, ResolutionNamespace, Text, PackageId)
tyConMergeKey tyCon = (tyConName tyCon, tyConNamespace tyCon, tyConModuleName tyCon, tyConPackageId tyCon)

axiomMergeKey :: TcAxiomKey -> (Text, Text, PackageId)
axiomMergeKey key = (axiomKeyName key, axiomKeyModule key, axiomKeyPackage key)

instanceMergeKey :: InstanceInfo -> (Text, (Text, Text))
instanceMergeKey info = (iiDictName info, iiDictOrigin info)

mergeInstanceInfos :: [InstanceInfo] -> [InstanceInfo]
mergeInstanceInfos = mergeInterfaceEntries "instance interface" instanceMergeKey

-- | Keep only facts that the selected modules define.
restrictTcInterfaceToModules :: PackageId -> [Text] -> TcInterface -> TcInterface
restrictTcInterfaceToModules package names interface =
  TcInterface
    { tcInterfaceTerms = filter localTerm (tcInterfaceTerms interface),
      tcInterfaceTyCons = filter (localTyCon . tciTyCon) (tcInterfaceTyCons interface),
      tcInterfaceDataTypes = filter (localTyCon . dtiTyCon) (tcInterfaceDataTypes interface),
      tcInterfaceClasses = filter (localTyCon . ciTyCon) (tcInterfaceClasses interface),
      tcInterfaceInstances = filter localInstance (tcInterfaceInstances interface),
      tcInterfaceDataFamilyInstances = filter (localTyCon . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstances interface),
      tcInterfaceTypeFamilyInstances = filter localTypeFamilyInstance (tcInterfaceTypeFamilyInstances interface),
      tcInterfacePatSyns = filter (localTerm . (,()) . patSynKey) (tcInterfacePatSyns interface)
    }
  where
    selected = Map.fromList [(name, ()) | name <- names]
    localModule moduleName' = Map.member moduleName' selected
    localTyCon tyCon = tyConPackageId tyCon == package && localModule (tyConModuleName tyCon)
    localTerm (key, _) =
      case key of
        TcTermGlobal package' moduleName' _ -> package' == package && localModule moduleName'
        TcTermLocal {} -> False
    localInstance info =
      let (packageName, moduleName') = iiDictOrigin info
       in packageName == packageIdText package && localModule moduleName'
    localTypeFamilyInstance info =
      let (originPackage, originModule) = tfiiOrigin info
       in originPackage == package && localModule originModule

mergeInterfaceEntries :: (Ord key, Show key, Eq value) => String -> (value -> key) -> [value] -> [value]
mergeInterfaceEntries label key values = reverse ordered
  where
    (_, ordered) = List.foldl' insertEntry (Map.empty, []) values
    insertEntry (entries, previousValues) value =
      case insertNewEntry (key value) value entries of
        Right entries' -> (entries', value : previousValues)
        Left previous
          | previous == value -> (entries, previousValues)
          | otherwise -> error ("conflicting " <> label <> " key: " <> show (key value))

mapFromListNoDuplicates :: (Ord key, Show key) => String -> [(key, value)] -> Map.Map key value
mapFromListNoDuplicates label = List.foldl' insertEntry Map.empty
  where
    insertEntry entries (key, value) =
      case insertNewEntry key value entries of
        Right entries' -> entries'
        Left _ -> error ("duplicate " <> label <> " key: " <> show key)

-- | Insert a value under a new key with one map lookup. Give back the
-- existing value when the key is already present.
insertNewEntry :: (Ord key) => key -> value -> Map.Map key value -> Either value (Map.Map key value)
insertNewEntry key value entries =
  case Map.insertLookupWithKey (\_ _ existing -> existing) key value entries of
    (Nothing, entries') -> Right entries'
    (Just existing, _) -> Left existing

mergeTyConInfos :: [TyConInfo] -> [TyConInfo]
mergeTyConInfos = mergeInterfaceEntries "type constructor interface" (tyConMergeKey . tciTyCon)

tcTermKeyIdentifier :: TcTermKey -> Maybe Text
tcTermKeyIdentifier key =
  case key of
    TcTermLocal {} -> Nothing
    TcTermGlobal _ _ identifier -> Just identifier

-- | Convert stored type facts to the binding view required by System FC.
tcInterfaceBindings :: TcInterface -> [TcBindingResult]
tcInterfaceBindings interface =
  mapMaybe termBinding (tcInterfaceTerms interface)
    <> map instanceBinding (tcInterfaceInstances interface)
    <> concatMap classBindings (tcInterfaceClasses interface)
  where
    termBinding (TcTermGlobal _ _ identifier, scheme) = Just (TcBindingResult identifier identifier (interfaceSchemeType scheme))
    termBinding (TcTermLocal {}, _) = Nothing
    instanceBinding info = TcBindingResult (iiDictName info) (iiDictName info) (iiDictType info)
    classBindings info =
      [ TcBindingResult workerName workerName (interfaceSchemeType workerScheme)
      | methodName <- ciDefaultMethods info,
        Just methodScheme <- [lookup methodName (ciMethods info)],
        let workerName = defaultMethodName methodName
            workerScheme = maybe methodScheme (defaultWorkerScheme methodScheme) (lookup methodName (ciDefaultSignatures info))
      ]
    defaultWorkerScheme ordinaryScheme (ForAll variables predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll variables (classPredicate : predicates) body
        _ -> ForAll variables predicates body

interfaceSchemeType :: TypeScheme -> TcType
interfaceSchemeType (ForAll [] [] ty) = ty
interfaceSchemeType (ForAll variables [] ty) = foldr TcForAllTy ty variables
interfaceSchemeType (ForAll [] predicates ty) = TcQualTy predicates ty
interfaceSchemeType (ForAll variables predicates ty) = foldr TcForAllTy (TcQualTy predicates ty) variables

-- | Type-check a single expression in an empty environment.
--
-- This is the primary entry point for testing. For modules, use
-- `typecheckModulesWithInterface`.
typecheckExpr :: TcConfig -> Expr -> TcResult
typecheckExpr config expr =
  case runTcM (emptyTcEnv config) initTcState (typecheckExprM expr <* finalizeDiagnostics) of
    Left _abort ->
      TcResult
        { tcResultType = TcMetaTv (Unique (-1)),
          tcResultDiagnostics = [],
          tcResultSuccess = False
        }
    Right (ty, st) ->
      let diags = reverse (tcsDiagnostics st)
          hasErrors = any isError diags
       in TcResult
            { tcResultType = ty,
              tcResultDiagnostics = diags,
              tcResultSuccess = not hasErrors
            }
  where
    isError d = diagSeverity d == TcError

-- | Internal: type-check an expression in TcM.
typecheckExprM :: Expr -> TcM TcType
typecheckExprM expr = do
  -- 1. Generate constraints.
  (_expr', ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: Module -> [TcBindingResult]
tcModuleBindings =
  moduleBindings

-- | Class instances recovered from a type-checked module's annotations.
tcModuleInstances :: Module -> [InstanceInfo]
tcModuleInstances =
  moduleInstances

-- | Type classes recovered from a type-checked module's annotations.
tcModuleClasses :: Module -> [ClassInfo]
tcModuleClasses = moduleClasses

-- | Diagnostics recovered from type-checker annotations in a module.
tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectTcDiagnostics

-- | Whether an annotated module contains no type-checker errors.
tcModuleSuccess :: Module -> Bool
tcModuleSuccess =
  not . any isError . tcModuleDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Type-check dependency-ordered modules with an imported semantic interface.
-- Return only facts that the specified modules define.
typecheckModulesWithInterface :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModulesWithInterface config imported modules =
  let initialState = initialTcState imported
      persistentUnqualifiedTerms = Map.keys (Map.filterWithKey (\key _ -> isUnqualifiedTermKey key) (tcsGlobalTerms initialState))
      (checkedModules, finalState) = go persistentUnqualifiedTerms initialState modules
   in (checkedModules, tcInterfaceDifference initialState finalState)
  where
    go _ st [] = ([], st)
    go persistentUnqualifiedTerms st (m : ms) =
      let (result, st') = typecheckModuleWithState config st m
          nextState = removeTransientUnqualifiedTerms persistentUnqualifiedTerms st'
          (results, finalState) = go persistentUnqualifiedTerms nextState ms
       in (result : results, finalState)

removeTransientUnqualifiedTerms :: [TcTermKey] -> TcState -> TcState
removeTransientUnqualifiedTerms persistent state =
  state
    { tcsGlobalTerms =
        Map.filterWithKey
          (\key _ -> not (isUnqualifiedTermKey key) || key `elem` persistent)
          (tcsGlobalTerms state)
    }

isUnqualifiedTermKey :: TcTermKey -> Bool
isUnqualifiedTermKey key =
  case key of
    TcTermGlobal packageId moduleName _ -> T.null (packageIdText packageId) && T.null moduleName
    TcTermLocal {} -> False

-- | Type-check one strongly connected module component using only the
-- supplied imported interface.
typecheckModuleSccWithInterface :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModuleSccWithInterface config imported modules =
  let initialState = initialTcState imported
      (checkedModules, finalState) = typecheckModuleSccWithState config initialState modules
   in (checkedModules, tcInterfaceDifference initialState finalState)

initialTcState :: TcInterface -> TcState
initialTcState imported =
  initTcState
    { tcsGlobalTerms =
        mapFromListNoDuplicates
          "imported term state"
          [ (key, TcIdBinder scheme Closed)
          | (key, scheme) <- tcInterfaceTerms imported
          ]
          <> tcsGlobalTerms initTcState,
      tcsGlobalTyCons =
        Map.fromList
          [ (tyConKey (tciTyCon tyCon), tyCon)
          | tyCon <- mergeTyConInfos (tcInterfaceTyCons imported)
          ]
          <> tcsGlobalTyCons initTcState,
      tcsDataTypes = mapFromListNoDuplicates "imported data type state" [(dataTypeKey dataType, dataType) | dataType <- tcInterfaceDataTypes imported],
      tcsClasses = mapFromListNoDuplicates "imported class state" [(classInfoKey classInfo, classInfo) | classInfo <- tcInterfaceClasses imported],
      tcsInstances = instanceEnvFromList (mergeInstanceInfos (tcInterfaceInstances imported)),
      tcsDataFamilyInstances =
        mapFromListNoDuplicates
          "imported data family instance state"
          [(dataFamilyAxiomKey info, info) | info <- tcInterfaceDataFamilyInstances imported],
      tcsTypeFamilyInstances =
        mapFromListNoDuplicates
          "imported type family instance state"
          [(typeFamilyAxiomKey info, info) | info <- tcInterfaceTypeFamilyInstances imported],
      tcsPatSyns = mapFromListNoDuplicates "imported pattern synonym state" [(patSynKey info, info) | info <- tcInterfacePatSyns imported]
    }

tcInterfaceDifference :: TcState -> TcState -> TcInterface
tcInterfaceDifference initial state =
  TcInterface
    { tcInterfaceTerms = exportedGlobalTerms (Map.difference (tcsGlobalTerms state) (tcsGlobalTerms initial)),
      tcInterfaceTyCons = Map.elems (Map.difference (tcsGlobalTyCons state) (tcsGlobalTyCons initial)),
      tcInterfaceDataTypes = Map.elems (Map.difference (tcsDataTypes state) (tcsDataTypes initial)),
      tcInterfaceClasses = Map.elems (Map.difference (tcsClasses state) (tcsClasses initial)),
      tcInterfaceInstances = filter ((`Set.notMember` initialInstanceKeys) . instanceInfoKey) (instanceEnvList (tcsInstances state)),
      tcInterfaceDataFamilyInstances = Map.elems (Map.difference (tcsDataFamilyInstances state) (tcsDataFamilyInstances initial)),
      tcInterfaceTypeFamilyInstances = Map.elems (Map.difference (tcsTypeFamilyInstances state) (tcsTypeFamilyInstances initial)),
      tcInterfacePatSyns = Map.elems (Map.difference (tcsPatSyns state) (tcsPatSyns initial))
    }
  where
    initialInstanceKeys = Set.fromList (map instanceInfoKey (instanceEnvList (tcsInstances initial)))

exportedGlobalTerms :: Map.Map TcTermKey TcBinder -> [(TcTermKey, TypeScheme)]
exportedGlobalTerms globalTerms =
  filter (not . isRedundantUnqualifiedAlias . fst) terms
  where
    terms =
      [ (key, scheme)
      | (key, TcIdBinder scheme _) <- Map.toList globalTerms
      ]
    isRedundantUnqualifiedAlias key =
      case key of
        TcTermGlobal _ _ identifier
          | isUnqualifiedTermKey key -> any (isQualifiedIdentity identifier . fst) terms
        _ -> False
    isQualifiedIdentity identifier key =
      case key of
        TcTermGlobal packageId moduleName name ->
          name == identifier && (not (T.null (packageIdText packageId)) || not (T.null moduleName))
        TcTermLocal {} -> False

typecheckModuleSccWithState :: TcConfig -> TcState -> [Module] -> ([Module], TcState)
typecheckModuleSccWithState config st modules =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModuleScc modules <* finalizeDiagnostics) of
    Left abort ->
      ( case modules of
          [] -> []
          first : rest -> annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] first : rest,
        st
      )
    Right (annotatedModules, st') ->
      let diags = reverse (tcsDiagnostics st')
          results = attachSccDiagnostics diags annotatedModules
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (results, nextState)
  where
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = any (elem MonoLocalBinds . moduleExtensions) modules,
          tcEnvMonomorphismRestriction = any (elem MonomorphismRestriction . moduleExtensions) modules
        }
    moduleExtensions m =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules = foldl attachOne modules diagnostics
  where
    attachOne [] _ = []
    attachOne current@(first : rest) diagnostic =
      case diagLoc diagnostic of
        Nothing -> annotateModuleDiagnostics [diagnostic] first : rest
        Just span' ->
          let sourceName = sourceSpanSourceName span'
              matches m = sourceName `elem` moduleSourceNames m
           in if any matches current
                then map (\m -> if matches m then annotateModuleDiagnostics [diagnostic] m else m) current
                else annotateModuleDiagnostics [internalAbortDiagnostic "SCC diagnostic source did not match a module"] first : rest

moduleSourceNames :: Module -> [FilePath]
moduleSourceNames modu =
  case spanFromAnnotations (moduleAnns modu) of
    SourceSpan {sourceSpanSourceName = sourceName} -> [sourceName]
    NoSourceSpan -> []

typecheckModuleWithState :: TcConfig -> TcState -> Module -> (Module, TcState)
typecheckModuleWithState config st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule m <* finalizeDiagnostics) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] m,
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          result = annotateModuleDiagnostics diags annotatedModule
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (result, nextState)
  where
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions
        }
    enabledExtensions =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = foldl attachLocatedDiagnostic m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

attachLocatedDiagnostic :: Module -> (SourceSpan, TcDiagnostic) -> Module
attachLocatedDiagnostic m (sp, diagnostic) =
  case runState (attachDiagnosticAt sp diagnostic m) False of
    (m', True) -> m'
    (_, False) ->
      error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticAt :: (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt sp diagnostic =
  everywhereM attachHere
  where
    attachHere :: forall node. (Data node) => node -> State Bool node
    attachHere value = do
      alreadyAttached <- get
      if alreadyAttached
        then pure value
        else case attachDiagnosticHere sp diagnostic value of
          Just value' -> do
            put True
            pure value'
          Nothing ->
            pure value

attachDiagnosticHere :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> Maybe a
attachDiagnosticHere sp diagnostic value =
  attachAnnotationList
    <|> attachExpr
    <|> attachPattern
    <|> attachType
    <|> attachDecl
    <|> attachDataConDecl
    <|> attachLiteral
    <|> attachGuardQualifier
    <|> attachDoStmtExpr
    <|> attachDoStmtCmd
    <|> attachCompStmt
    <|> attachArithSeq
    <|> attachClassDeclItem
    <|> attachInstanceDeclItem
    <|> attachCmd
    <|> attachExportSpec
    <|> attachImportItem
  where
    diagnosticAnn = mkAnnotation diagnostic
    atExactSpan span' wrap =
      if span' == sp
        then cast wrap
        else Nothing
    attachTyped :: forall node. (Data node) => (node -> Maybe node) -> Maybe a
    attachTyped f = do
      node <- cast value
      node' <- f node
      cast node'
    attachAnnotationList =
      attachTyped $ \(anns :: [Annotation]) ->
        atExactSpan (spanFromAnnotations anns) (anns <> [diagnosticAnn])
    attachExpr =
      attachTyped $ \(expr :: Expr) ->
        atExactSpan (wrappedSpan peelExprAnnOnce expr) (EAnn diagnosticAnn expr)
    attachPattern =
      attachTyped $ \(pat :: Pattern) ->
        atExactSpan (wrappedSpan peelPatternAnnOnce pat) (PAnn diagnosticAnn pat)
    attachType =
      attachTyped $ \(ty :: Type) ->
        atExactSpan (wrappedSpan peelTypeAnnOnce ty) (TAnn diagnosticAnn ty)
    attachDecl =
      attachTyped $ \(decl :: Decl) ->
        atExactSpan (wrappedSpan peelDeclAnnOnce decl) (DeclAnn diagnosticAnn decl)
    attachDataConDecl =
      attachTyped $ \(decl :: DataConDecl) ->
        atExactSpan (wrappedSpan peelDataConAnnOnce decl) (DataConAnn diagnosticAnn decl)
    attachLiteral =
      attachTyped $ \(lit :: Literal) ->
        atExactSpan (wrappedSpan peelLiteralAnnOnce lit) (LitAnn diagnosticAnn lit)
    attachGuardQualifier =
      attachTyped $ \(qualifier :: GuardQualifier) ->
        atExactSpan (wrappedSpan peelGuardAnnOnce qualifier) (GuardAnn diagnosticAnn qualifier)
    attachDoStmtExpr =
      attachTyped $ \(stmt :: DoStmt Expr) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachDoStmtCmd =
      attachTyped $ \(stmt :: DoStmt Cmd) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachCompStmt =
      attachTyped $ \(stmt :: CompStmt) ->
        atExactSpan (wrappedSpan peelCompAnnOnce stmt) (CompAnn diagnosticAnn stmt)
    attachArithSeq =
      attachTyped $ \(seq' :: ArithSeq) ->
        atExactSpan (wrappedSpan peelArithSeqAnnOnce seq') (ArithSeqAnn diagnosticAnn seq')
    attachClassDeclItem =
      attachTyped $ \(item :: ClassDeclItem) ->
        atExactSpan (wrappedSpan peelClassItemAnnOnce item) (ClassItemAnn diagnosticAnn item)
    attachInstanceDeclItem =
      attachTyped $ \(item :: InstanceDeclItem) ->
        atExactSpan (wrappedSpan peelInstanceItemAnnOnce item) (InstanceItemAnn diagnosticAnn item)
    attachCmd =
      attachTyped $ \(cmd :: Cmd) ->
        atExactSpan (wrappedSpan peelCmdAnnOnce cmd) (CmdAnn diagnosticAnn cmd)
    attachExportSpec =
      attachTyped $ \(spec :: ExportSpec) ->
        atExactSpan (wrappedSpan peelExportAnnOnce spec) (ExportAnn diagnosticAnn spec)
    attachImportItem =
      attachTyped $ \(item :: ImportItem) ->
        atExactSpan (wrappedSpan peelImportAnnOnce item) (ImportAnn diagnosticAnn item)

wrappedSpan :: (node -> Maybe (Annotation, node)) -> node -> SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe (Annotation, Expr)
peelExprAnnOnce (EAnn ann inner) = Just (ann, inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe (Annotation, Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just (ann, inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe (Annotation, Type)
peelTypeAnnOnce (TAnn ann inner) = Just (ann, inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe (Annotation, Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just (ann, inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe (Annotation, DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just (ann, inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe (Annotation, Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just (ann, inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe (Annotation, GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just (ann, inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe (Annotation, DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just (ann, inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe (Annotation, CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just (ann, inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe (Annotation, ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just (ann, inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe (Annotation, ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just (ann, inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe (Annotation, InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just (ann, inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe (Annotation, Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just (ann, inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe (Annotation, ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just (ann, inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe (Annotation, ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just (ann, inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation =
  concreteSpan <=< fromAnnotation

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan sp = Just sp

collectTcDiagnostics :: (Data a) => a -> [TcDiagnostic]
collectTcDiagnostics =
  everything (maybe [] (maybeToList . fromAnnotation) . cast)

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
