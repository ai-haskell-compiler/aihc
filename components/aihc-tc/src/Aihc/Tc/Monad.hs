{-# LANGUAGE OverloadedStrings #-}

-- | The type checker monad and state.
module Aihc.Tc.Monad
  ( -- * Monad
    TcM,
    runTcM,
    abortTc,
    tcAbortMessage,

    -- * State
    TcState (..),
    initTcState,

    -- * Fresh names
    freshUnique,
    freshMetaTv,
    freshMetaTvOfKind,
    freshSkolemTv,
    freshEvVar,
    getUniqueBoundary,

    -- * Meta-variable solutions
    writeMetaTv,
    readMetaTv,
    writeKindMeta,
    readKindMeta,
    readMetaTvKind,
    writeRuntimeRepDependency,
    readRuntimeRepDependency,

    -- * Evidence
    bindEvidence,
    lookupEvidence,

    -- * Environment
    TcEnv (..),
    TcBinder (..),
    TcTermKey (..),
    TcTyConKey (..),
    Closedness (..),
    emptyTcEnv,
    lookupTerm,
    lookupResolvedTerm,
    resolvedTermKey,
    resolvedTermTarget,
    resolvedUnqualifiedTermKey,
    resolvedLocalTermKey,
    withTcModule,
    extendTermEnv,
    extendResolvedTermEnv,
    extendTermEnvPermanent,
    getTermEnv,
    lookupTyCon,
    lookupResolvedTyCon,
    extendTyConEnvPermanent,
    getTyConEnv,
    addDataType,
    getDataTypes,
    lookupDataType,
    localTcOptions,
    tcMonoLocalBinds,
    tcMonomorphismRestriction,
    getTcLevel,
    withTcLevel,
    addInstance,
    getInstances,
    addDataFamilyInstance,
    getDataFamilyInstances,
    addClass,
    getClasses,
    lookupClass,

    -- * GADT constructor registry
    markGadtCon,
    isGadtCon,

    -- * Diagnostics
    emitDiagnostic,
    emitError,
    emitWarning,
    getDiagnostics,
    withErrorTracking,
  )
where

import Aihc.Name (GlobalName (..), LocalName (..), ModuleId, Namespace (..), OccName (..), WiredInName (..), globalName)
import Aihc.Name qualified as CompilerName
import Aihc.Parser.Syntax (Annotation, Module, Name (..), SourceSpan (..), UnqualifiedName (..), fromAnnotation, nameText, unqualifiedNameText)
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Resolve qualified as Resolve
import Aihc.Tc.Env (ClassInfo (..), DataFamilyInstanceInfo, DataTypeInfo (..), InstanceInfo, TyConInfo (..))
import Aihc.Tc.Error
import Aihc.Tc.Evidence
import Aihc.Tc.Types
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', runStateT)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)

-- | The type checker monad.
--
-- Uses 'ReaderT' for the local environment and 'StateT' for mutable state
-- (fresh name supply, meta-variable solutions, evidence bindings, diagnostics).
type TcM a = ReaderT TcEnv (StateT TcState Identity) a

-- | Strict identity, used as the base monad.
-- (We avoid IO/ST for the MVP; the Map-based meta-variable store is
-- functionally equivalent to STRef and can be migrated later.)
type Identity = Either TcAbort

-- | Fatal abort (internal error, not a user-facing diagnostic).
newtype TcAbort = TcAbort String
  deriving (Show)

-- | Run the type checker computation.
runTcM :: TcEnv -> TcState -> TcM a -> Either TcAbort (a, TcState)
runTcM env st m = runStateT (runReaderT m env) st

abortTc :: String -> TcM a
abortTc msg = lift (lift (Left (TcAbort msg)))

tcAbortMessage :: TcAbort -> String
tcAbortMessage (TcAbort msg) = msg

-- | The local typing environment (read-only within a scope).
data TcEnv = TcEnv
  { -- | Local term bindings in scope.
    --
    -- The keys come from @aihc-resolve@'s 'ResolvedLocal' identifiers, not
    -- from source text. This lets TC preserve lexical identity without doing
    -- name resolution or conflating duplicate textual names.
    tcEnvTerms :: !(Map TcTermKey TcBinder),
    -- | Whether local binding groups follow GHC's MonoLocalBinds rule.
    tcEnvMonoLocalBinds :: !Bool,
    -- | Whether the monomorphism restriction is active.
    tcEnvMonomorphismRestriction :: !Bool,
    -- | Current implication nesting level.
    tcEnvTcLevel :: !TcLevel,
    -- | The module whose declarations are currently being checked.
    tcEnvModule :: !(Maybe ModuleId)
  }
  deriving (Show)

-- | Whether a polymorphic binding is known to have no free type variables.
data Closedness
  = Closed
  | NotClosed
  deriving (Eq, Show)

-- | A binding in the term environment.
data TcBinder
  = -- | Polymorphic binding (top-level or let with signature).
    TcIdBinder !TypeScheme !Closedness
  | -- | Monomorphic binding (lambda-bound, pattern-bound, local let).
    TcMonoIdBinder !TcType
  deriving (Show)

data TcTermKey
  = TcTermLocal !LocalName
  | TcTermGlobal !GlobalName
  | TcTermBuiltin !WiredInName
  | TcTermLegacy !Text
  deriving (Eq, Ord, Show, Read)

instance IsString TcTermKey where
  fromString = TcTermLegacy . fromString

data TcTyConKey
  = TcTypeGlobal !GlobalName
  | TcTypeBuiltin !WiredInName
  | TcTypeLegacy !Text
  deriving (Eq, Ord, Show, Read)

-- | An empty environment at the top level.
emptyTcEnv :: TcEnv
emptyTcEnv =
  TcEnv
    { tcEnvTerms = Map.empty,
      tcEnvMonoLocalBinds = True,
      tcEnvMonomorphismRestriction = True,
      tcEnvTcLevel = topTcLevel,
      tcEnvModule = Nothing
    }

-- | The mutable state of the type checker.
data TcState = TcState
  { -- | Next unique identifier to allocate.
    tcsNextUnique :: !Int,
    -- | Solutions for meta (unification) variables.
    tcsMetaSolutions :: !(Map Unique TcType),
    -- | Solutions for kind meta-variables.
    tcsKindSolutions :: !(Map Unique Kind),
    -- | Declared kinds of representation-polymorphic meta-variables.
    tcsMetaKinds :: !(Map Unique Kind),
    -- | A runtime-representation meta and the value type whose representation
    -- determines it.
    tcsRuntimeRepDependencies :: !(Map Unique TcType),
    -- | Evidence bindings accumulated during solving.
    tcsEvBinds :: !(Map Unique EvTerm),
    -- | Diagnostics (errors and warnings) collected.
    tcsDiagnostics :: ![TcDiagnostic],
    -- | Global term bindings accumulated from declarations and imported
    -- interfaces.
    --
    -- This map remains text-keyed because it is not used to decide lexical
    -- scope. Occurrences reach it only after @aihc-resolve@ has attached a
    -- 'ResolvedTopLevel' or 'ResolvedBuiltin' target; TC then uses the target's
    -- selected global name to retrieve the type.
    tcsGlobalTerms :: !(Map TcTermKey TcBinder),
    -- | Global type constructors accumulated by top-level declarations.
    tcsGlobalTyCons :: !(Map TcTyConKey TyConInfo),
    -- | Checked constructor layouts for data and newtype declarations.
    tcsDataTypes :: !(Map Text DataTypeInfo),
    -- | Type classes in scope, including their superclass layouts and defaults.
    tcsClasses :: !(Map Text ClassInfo),
    -- | Class instances in scope.
    tcsInstances :: ![InstanceInfo],
    -- | Standalone data-family instance equations in scope.
    tcsDataFamilyInstances :: ![DataFamilyInstanceInfo],
    -- | Names of GADT constructors (have non-trivial result types).
    tcsGadtCons :: !(Set Text)
  }
  deriving (Show)

-- | Initial state with no variables or bindings.
initTcState :: TcState
initTcState =
  TcState
    { tcsNextUnique = 0,
      tcsMetaSolutions = Map.empty,
      tcsKindSolutions = Map.empty,
      tcsMetaKinds = Map.empty,
      tcsRuntimeRepDependencies = Map.empty,
      tcsEvBinds = Map.empty,
      tcsDiagnostics = [],
      tcsGlobalTerms = builtinTerms,
      tcsGlobalTyCons = Map.empty,
      tcsDataTypes = Map.empty,
      tcsClasses = Map.empty,
      tcsInstances = [],
      tcsDataFamilyInstances = [],
      tcsGadtCons = Set.empty
    }

builtinTerms :: Map TcTermKey TcBinder
builtinTerms =
  Map.fromList
    [ (TcTermBuiltin (WiredInName TermNamespace (OccName ":")), TcIdBinder consScheme Closed),
      (TcTermBuiltin (WiredInName TermNamespace (OccName "[]")), TcIdBinder nilScheme Closed)
    ]
  where
    aVar = TyVarId "a" (Unique (-1000))
    aTy = TcTyVar aVar
    listA = TcTyCon (TyCon "[]" 1) [aTy]
    consScheme = ForAll [aVar] [] (TcFunTy aTy (TcFunTy listA listA))
    nilScheme = ForAll [aVar] [] listA

-- | Allocate a fresh 'Unique'.
freshUnique :: TcM Unique
freshUnique = lift $ do
  st <- get
  let u = tcsNextUnique st
  modify' (\s -> s {tcsNextUnique = u + 1})
  pure (Unique u)

-- | Allocate a fresh meta (unification) type variable.
freshMetaTv :: TcM TcType
freshMetaTv = TcMetaTv <$> freshUnique

freshMetaTvOfKind :: Kind -> TcM TcType
freshMetaTvOfKind kind = do
  unique <- freshUnique
  lift $ modify' $ \state ->
    state {tcsMetaKinds = Map.insert unique kind (tcsMetaKinds state)}
  pure (TcMetaTv unique)

-- | Allocate a fresh skolem (rigid) type variable.
freshSkolemTv :: Text -> TcM TyVarId
freshSkolemTv name = do
  u <- freshUnique
  pure (TyVarId {tvName = name, tvUnique = u})

-- | Allocate a fresh evidence variable.
freshEvVar :: TcM EvVar
freshEvVar = EvVar <$> freshUnique

-- | Snapshot the unique supply. Uniques below this boundary were allocated
-- before the current type-checking region.
getUniqueBoundary :: TcM Unique
getUniqueBoundary = Unique <$> lift (gets tcsNextUnique)

-- | Record the solution for a meta-variable.
writeMetaTv :: Unique -> TcType -> TcM ()
writeMetaTv u ty = lift $ modify' $ \s ->
  s {tcsMetaSolutions = Map.insert u ty (tcsMetaSolutions s)}

-- | Look up the current solution for a meta-variable.
readMetaTv :: Unique -> TcM (Maybe TcType)
readMetaTv u = lift $ gets $ \s ->
  Map.lookup u (tcsMetaSolutions s)

readMetaTvKind :: Unique -> TcM Kind
readMetaTvKind unique =
  lift $ gets $ Map.findWithDefault liftedTypeKind unique . tcsMetaKinds

writeRuntimeRepDependency :: Unique -> TcType -> TcM ()
writeRuntimeRepDependency unique representedType =
  lift $ modify' $ \state ->
    state
      { tcsRuntimeRepDependencies =
          Map.insert unique representedType (tcsRuntimeRepDependencies state)
      }

readRuntimeRepDependency :: Unique -> TcM (Maybe TcType)
readRuntimeRepDependency unique =
  lift $ gets $ Map.lookup unique . tcsRuntimeRepDependencies

-- | Record the solution for a kind meta-variable.
writeKindMeta :: Unique -> Kind -> TcM ()
writeKindMeta u kind = lift $ modify' $ \s ->
  s {tcsKindSolutions = Map.insert u kind (tcsKindSolutions s)}

-- | Look up the current solution for a kind meta-variable.
readKindMeta :: Unique -> TcM (Maybe Kind)
readKindMeta u = lift $ gets $ \s ->
  Map.lookup u (tcsKindSolutions s)

-- | Bind an evidence variable to an evidence term.
bindEvidence :: EvVar -> EvTerm -> TcM ()
bindEvidence (EvVar u) ev = lift $ modify' $ \s ->
  s {tcsEvBinds = Map.insert u ev (tcsEvBinds s)}

-- | Look up an evidence binding.
lookupEvidence :: EvVar -> TcM (Maybe EvTerm)
lookupEvidence (EvVar u) = lift $ gets $ \s ->
  Map.lookup u (tcsEvBinds s)

-- | Look up a global term by its selected global name.
lookupTerm :: Text -> TcM (Maybe TcBinder)
lookupTerm name = do
  current <- currentTermKey name >>= lookupTermKey
  case current of
    Just binder -> pure (Just binder)
    Nothing -> lift $ gets (lookupUnambiguousGlobalTerm name . tcsGlobalTerms)

-- Compiler-generated references have no surface occurrence for the resolver
-- to annotate.  They may use an occurrence name only when that occurrence is
-- unambiguous in the semantic environment; source references always go
-- through 'lookupResolvedTerm' and therefore never take this compatibility
-- path.
lookupUnambiguousGlobalTerm :: Text -> Map TcTermKey TcBinder -> Maybe TcBinder
lookupUnambiguousGlobalTerm name terms =
  Map.lookup (TcTermBuiltin (WiredInName TermNamespace (OccName name))) terms
    <|> Map.lookup (TcTermLegacy name) terms
    <|> case [ binder
             | (TcTermGlobal identity, binder) <- Map.toList terms,
               globalOccName identity == OccName name
             ] of
      [binder] -> Just binder
      _ -> Nothing

lookupResolvedTerm :: Text -> ResolvedName -> TcM (Maybe TcBinder)
lookupResolvedTerm displayName resolved = do
  key <- resolvedNameTermKey displayName resolved
  exact <- lookupTermKey key
  case exact of
    Just binder -> pure (Just binder)
    Nothing ->
      case key of
        -- A few parser-synthesized constructors and old cached interfaces do
        -- not yet carry a declaration key. They may satisfy a resolved use
        -- only when there is exactly one compatible global occurrence.
        TcTermGlobal identity -> lift $ gets (lookupUnambiguousGlobalTerm (unOccName (globalOccName identity)) . tcsGlobalTerms)
        TcTermBuiltin identity -> lift $ gets (lookupUnambiguousGlobalTerm (unOccName (wiredInOccName identity)) . tcsGlobalTerms)
        _ -> pure Nothing

lookupTermKey :: TcTermKey -> TcM (Maybe TcBinder)
lookupTermKey key =
  case key of
    TcTermLocal _ ->
      asks $ \env -> Map.lookup key (tcEnvTerms env)
    TcTermGlobal name ->
      lift $ gets $ \s -> Map.lookup (TcTermGlobal name) (tcsGlobalTerms s)
    TcTermBuiltin name ->
      lift $ gets $ \s -> Map.lookup (TcTermBuiltin name) (tcsGlobalTerms s)
    TcTermLegacy name ->
      lift $ gets $ \s -> Map.lookup (TcTermLegacy name) (tcsGlobalTerms s)

resolvedTermKey :: Name -> TcM TcTermKey
resolvedTermKey name =
  resolvedNameTermKey (nameText name) =<< resolvedTermTarget name

resolvedUnqualifiedTermKey :: UnqualifiedName -> TcM TcTermKey
resolvedUnqualifiedTermKey name =
  case termResolution (unqualifiedNameAnns name) of
    Just resolution ->
      resolvedNameTermKey (unqualifiedNameText name) (resolutionTarget resolution)
    Nothing ->
      abortTc ("missing resolver annotation for binder " <> show (unqualifiedNameText name))

resolvedNameTermKey :: Text -> ResolvedName -> TcM TcTermKey
resolvedNameTermKey displayName resolved =
  case resolved of
    ResolvedLocal name ->
      pure (TcTermLocal name)
    ResolvedTopLevel name ->
      pure (TcTermGlobal name)
    ResolvedBuiltin name ->
      pure (TcTermBuiltin name)
    ResolvedError msg ->
      abortTc ("resolver error reached type checker for term " <> show displayName <> ": " <> msg)

-- | Snapshot all visible term bindings keyed by resolver-selected identity.
getTermEnv :: TcM (Map TcTermKey TcBinder)
getTermEnv = do
  locals <- asks tcEnvTerms
  globals <- lift $ gets tcsGlobalTerms
  pure (locals <> globals)

-- | Extend the term environment with a new binding for the duration
-- of the given computation.
extendTermEnv :: TcTermKey -> TcBinder -> TcM a -> TcM a
extendTermEnv key binder =
  local $ \env ->
    env {tcEnvTerms = Map.insert key binder (tcEnvTerms env)}

extendResolvedTermEnv :: UnqualifiedName -> TcBinder -> TcM a -> TcM a
extendResolvedTermEnv name binder action = do
  key <- resolvedLocalTermKey name
  extendTermEnv key binder action

-- | Permanently extend the global term environment (for top-level
-- declarations like data constructors and top-level bindings).
extendTermEnvPermanent :: Text -> TcBinder -> TcM ()
extendTermEnvPermanent name binder = do
  key <- currentTermKey name
  lift $ modify' $ \s ->
    s {tcsGlobalTerms = Map.insert key binder (tcsGlobalTerms s)}

currentTermKey :: Text -> TcM TcTermKey
currentTermKey name = do
  owner <- asks tcEnvModule
  pure $ case owner of
    Just moduleId' -> TcTermGlobal (globalName moduleId' TermNamespace name)
    Nothing -> TcTermLegacy name

-- | Set the owner used for top-level declarations and compiler-generated
-- siblings while checking one resolved module.
withTcModule :: Module -> TcM a -> TcM a
withTcModule modu =
  local $ \env -> env {tcEnvModule = Resolve.resolvedModuleIdentity modu <|> tcEnvModule env}

resolvedTermTarget :: Name -> TcM ResolvedName
resolvedTermTarget name =
  case termResolution (nameAnns name) of
    Just resolution -> pure (resolutionTarget resolution)
    Nothing ->
      abortTc ("missing resolver annotation for term occurrence " <> show (nameText name))

resolvedLocalTermKey :: UnqualifiedName -> TcM TcTermKey
resolvedLocalTermKey name =
  case termResolution (unqualifiedNameAnns name) of
    Just resolution ->
      case resolutionTarget resolution of
        ResolvedLocal localName -> pure (TcTermLocal localName)
        target ->
          abortTc ("expected local resolver annotation for binder " <> show (unqualifiedNameText name) <> ", got " <> show target)
    Nothing ->
      abortTc ("missing resolver annotation for binder " <> show (unqualifiedNameText name))

termResolution :: [Annotation] -> Maybe ResolutionAnnotation
termResolution =
  find ((== ResolutionNamespaceTerm) . resolutionNamespace)
    . mapMaybe fromAnnotation

lookupTyCon :: Text -> TcM (Maybe TyConInfo)
lookupTyCon name = currentTyConKey name >>= \key -> lift $ gets (Map.lookup key . tcsGlobalTyCons)

lookupResolvedTyCon :: Name -> TcM (Maybe TyConInfo)
lookupResolvedTyCon name =
  case typeResolution (nameAnns name) of
    Just resolution ->
      case resolutionTarget resolution of
        ResolvedTopLevel target -> lift $ gets (Map.lookup (TcTypeGlobal target) . tcsGlobalTyCons)
        ResolvedBuiltin target -> lift $ gets (Map.lookup (TcTypeBuiltin target) . tcsGlobalTyCons)
        _ -> pure Nothing
    Nothing -> lookupTyCon (nameText name)

getTyConEnv :: TcM (Map TcTyConKey TyConInfo)
getTyConEnv = lift $ gets tcsGlobalTyCons

extendTyConEnvPermanent :: Text -> TyConInfo -> TcM ()
extendTyConEnvPermanent name info = do
  key <- currentTyConKey name
  let info' =
        case key of
          TcTypeGlobal target -> info {tciTyCon = setTyConId (CompilerName.ResolvedGlobal target) (tciTyCon info)}
          TcTypeBuiltin target -> info {tciTyCon = setTyConId (CompilerName.ResolvedWiredIn target) (tciTyCon info)}
          TcTypeLegacy {} -> info
  lift $ modify' $ \s ->
    s {tcsGlobalTyCons = Map.insert key info' (tcsGlobalTyCons s)}

currentTyConKey :: Text -> TcM TcTyConKey
currentTyConKey name = do
  owner <- asks tcEnvModule
  pure $ case owner of
    Just moduleId' -> TcTypeGlobal (globalName moduleId' TypeNamespace name)
    Nothing -> TcTypeLegacy name

typeResolution :: [Annotation] -> Maybe ResolutionAnnotation
typeResolution =
  find ((== ResolutionNamespaceType) . resolutionNamespace)
    . mapMaybe fromAnnotation

addDataType :: DataTypeInfo -> TcM ()
addDataType info = lift $ modify' $ \state ->
  state {tcsDataTypes = Map.insert (dtiName info) info (tcsDataTypes state)}

getDataTypes :: TcM [DataTypeInfo]
getDataTypes = lift $ gets (Map.elems . tcsDataTypes)

lookupDataType :: Text -> TcM (Maybe DataTypeInfo)
lookupDataType name = lift $ gets (Map.lookup name . tcsDataTypes)

addInstance :: InstanceInfo -> TcM ()
addInstance instanceInfo = lift $ modify' $ \s ->
  s {tcsInstances = instanceInfo : tcsInstances s}

getInstances :: TcM [InstanceInfo]
getInstances = lift $ gets tcsInstances

addDataFamilyInstance :: DataFamilyInstanceInfo -> TcM ()
addDataFamilyInstance instanceInfo = lift $ modify' $ \state ->
  state {tcsDataFamilyInstances = instanceInfo : tcsDataFamilyInstances state}

getDataFamilyInstances :: TcM [DataFamilyInstanceInfo]
getDataFamilyInstances = lift $ gets tcsDataFamilyInstances

addClass :: ClassInfo -> TcM ()
addClass classInfo = lift $ modify' $ \state ->
  state {tcsClasses = Map.insert (ciName classInfo) classInfo (tcsClasses state)}

getClasses :: TcM [ClassInfo]
getClasses = lift $ gets (Map.elems . tcsClasses)

lookupClass :: Text -> TcM (Maybe ClassInfo)
lookupClass className = lift $ gets (Map.lookup className . tcsClasses)

-- | Run a computation with adjusted local type-checker options.
localTcOptions :: (Bool -> Bool) -> (Bool -> Bool) -> TcM a -> TcM a
localTcOptions monoLocal monomorphism =
  local $ \env ->
    env
      { tcEnvMonoLocalBinds = monoLocal (tcEnvMonoLocalBinds env),
        tcEnvMonomorphismRestriction = monomorphism (tcEnvMonomorphismRestriction env)
      }

tcMonoLocalBinds :: TcM Bool
tcMonoLocalBinds = asks tcEnvMonoLocalBinds

tcMonomorphismRestriction :: TcM Bool
tcMonomorphismRestriction = asks tcEnvMonomorphismRestriction

-- | Get the current implication nesting level.
getTcLevel :: TcM TcLevel
getTcLevel = asks tcEnvTcLevel

-- | Run a computation at a deeper implication level.
withTcLevel :: TcM a -> TcM a
withTcLevel =
  local $ \env ->
    env {tcEnvTcLevel = pushLevel (tcEnvTcLevel env)}

-- | Emit a diagnostic (error or warning).
emitDiagnostic :: TcDiagnostic -> TcM ()
emitDiagnostic d = lift $ modify' $ \s ->
  s {tcsDiagnostics = d : tcsDiagnostics s}

-- | Emit an error diagnostic.
emitError :: SourceSpan -> TcErrorKind -> TcM ()
emitError loc kind =
  emitDiagnostic
    TcDiagnostic
      { diagLoc = diagnosticLoc loc,
        diagSeverity = TcError,
        diagKind = kind
      }

-- | Emit a warning diagnostic.
emitWarning :: SourceSpan -> TcErrorKind -> TcM ()
emitWarning loc kind =
  emitDiagnostic
    TcDiagnostic
      { diagLoc = diagnosticLoc loc,
        diagSeverity = TcWarning,
        diagKind = kind
      }

diagnosticLoc :: SourceSpan -> Maybe SourceSpan
diagnosticLoc NoSourceSpan = Nothing
diagnosticLoc sp = Just sp

-- | Get all diagnostics collected so far.
getDiagnostics :: TcM [TcDiagnostic]
getDiagnostics = lift $ gets (reverse . tcsDiagnostics)

-- | Run a recoverable phase and report whether it emitted any errors.
--
-- The type checker intentionally keeps going after many local errors so later
-- declarations can still be checked. Callers that produce successful
-- elaboration metadata use this to avoid treating a recovered binding as a
-- checked binding.
withErrorTracking :: TcM a -> TcM (a, Bool)
withErrorTracking action = do
  before <- currentErrorCount
  result <- action
  after <- currentErrorCount
  pure (result, after > before)

currentErrorCount :: TcM Int
currentErrorCount =
  lift $ gets $ length . filter isError . tcsDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Record that a constructor is a GADT constructor.
markGadtCon :: Text -> TcM ()
markGadtCon name = lift $ modify' $ \s ->
  s {tcsGadtCons = Set.insert name (tcsGadtCons s)}

-- | Check whether a constructor is a GADT constructor.
isGadtCon :: Text -> TcM Bool
isGadtCon name = lift $ gets $ \s ->
  Set.member name (tcsGadtCons s)
