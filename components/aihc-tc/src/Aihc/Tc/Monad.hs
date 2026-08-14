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
    TcConfig,
    tcConfig,
    TcEnv (..),
    TcBinder (..),
    TcTermKey (..),
    unqualifiedTermKey,
    Closedness (..),
    emptyTcEnv,
    mkKnownTyCon,
    lookupTerm,
    lookupKnownTerm,
    lookupResolvedTerm,
    resolvedTermKey,
    resolvedTermTarget,
    resolvedUnqualifiedTermKey,
    resolvedLocalTermKey,
    extendTermEnv,
    extendResolvedTermEnv,
    extendTermEnvPermanent,
    extendTyConTermEnvPermanent,
    extendResolvedTermEnvPermanent,
    getTermEnv,
    lookupTyCon,
    lookupResolvedTyCon,
    lookupDeclaredTyCon,
    lookupTyConByIdentity,
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

import Aihc.Parser.Syntax (Annotation, Name (..), SourceSpan (..), UnqualifiedName (..), fromAnnotation, nameText, unqualifiedNameText)
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Env (ClassInfo (..), DataFamilyInstanceInfo, DataTypeInfo (..), InstanceInfo, TyConInfo (..))
import Aihc.Tc.Error
import Aihc.Tc.Evidence
import Aihc.Tc.Types
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', runStateT)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
  { tcEnvConfig :: !TcConfig,
    -- | Local term bindings in scope.
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
    tcEnvTcLevel :: !TcLevel
  }
  deriving (Show)

newtype TcConfig = TcConfig PackageId
  deriving (Show)

tcConfig :: PackageId -> TcConfig
tcConfig = TcConfig

mkKnownTyCon :: Text -> Text -> Int -> Kind -> TcM TyCon
mkKnownTyCon moduleName name arity kind = do
  TcConfig packageIdentity <- asks tcEnvConfig
  pure (mkTyConWithOrigin packageIdentity moduleName name arity kind)

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
  = TcTermLocal !Int
  | TcTermGlobal !PackageId !Text !Text
  deriving (Eq, Ord, Show, Read)

unqualifiedTermKey :: Text -> TcTermKey
unqualifiedTermKey = TcTermGlobal (PackageId "") ""

-- | An empty environment at the top level.
emptyTcEnv :: TcEnv
emptyTcEnv =
  TcEnv
    { tcEnvConfig = tcConfig (PackageId "aihc-prim"),
      tcEnvTerms = Map.empty,
      tcEnvMonoLocalBinds = True,
      tcEnvMonomorphismRestriction = True,
      tcEnvTcLevel = topTcLevel
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
    -- Global keys store the package, module, and identifier selected by
    -- @aihc-resolve@.
    tcsGlobalTerms :: !(Map TcTermKey TcBinder),
    -- | Global type constructors accumulated by top-level declarations.
    tcsGlobalTyCons :: !(Map TyCon TyConInfo),
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
      tcsGlobalTerms = Map.empty,
      tcsGlobalTyCons = Map.empty,
      tcsDataTypes = Map.empty,
      tcsClasses = Map.empty,
      tcsInstances = [],
      tcsDataFamilyInstances = [],
      tcsGadtCons = Set.empty
    }

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
lookupTerm name =
  lift $ gets $ \s -> Map.lookup (unqualifiedTermKey name) (tcsGlobalTerms s)

lookupKnownTerm :: Text -> Text -> TcM (Maybe TcBinder)
lookupKnownTerm moduleName name = do
  TcConfig packageId <- asks tcEnvConfig
  lookupTermKey (TcTermGlobal packageId moduleName name)

lookupResolvedTerm :: Text -> ResolvedName -> TcM (Maybe TcBinder)
lookupResolvedTerm displayName resolved = do
  exact <- resolvedNameTermKey displayName resolved >>= lookupTermKey
  case (exact, resolved) of
    (Nothing, ResolvedTopLevel _ name) -> lookupTerm (nameText name)
    _ -> pure exact

lookupTermKey :: TcTermKey -> TcM (Maybe TcBinder)
lookupTermKey key =
  case key of
    TcTermLocal _ ->
      asks $ \env -> Map.lookup key (tcEnvTerms env)
    TcTermGlobal {} ->
      lift $ gets $ \s -> Map.lookup key (tcsGlobalTerms s)

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
    ResolvedLocal unique _ ->
      pure (TcTermLocal unique)
    ResolvedTopLevel packageId name ->
      pure (TcTermGlobal packageId (fromMaybe "" (nameQualifier name)) (nameText name))
    ResolvedBuiltin name ->
      pure (unqualifiedTermKey name)
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
extendTermEnvPermanent name binder = lift $ modify' $ \s ->
  s {tcsGlobalTerms = Map.insert (unqualifiedTermKey name) binder (tcsGlobalTerms s)}

extendTyConTermEnvPermanent :: TyCon -> Text -> TcBinder -> TcM ()
extendTyConTermEnvPermanent tyCon name binder = do
  extendTermEnvPermanent name binder
  lift $ modify' $ \state ->
    state
      { tcsGlobalTerms =
          Map.insert
            (TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon) name)
            binder
            (tcsGlobalTerms state)
      }

-- | Add a source binder under its resolver identity and its source name.
extendResolvedTermEnvPermanent :: UnqualifiedName -> TcBinder -> TcM ()
extendResolvedTermEnvPermanent name binder = do
  extendTermEnvPermanent (unqualifiedNameText name) binder
  case termResolution (unqualifiedNameAnns name) of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName} ->
      lift $ modify' $ \state ->
        state
          { tcsGlobalTerms =
              Map.insert
                (TcTermGlobal packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName))
                binder
                (tcsGlobalTerms state)
          }
    _ -> pure ()

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
        ResolvedLocal unique _ -> pure (TcTermLocal unique)
        target ->
          abortTc ("expected local resolver annotation for binder " <> show (unqualifiedNameText name) <> ", got " <> show target)
    Nothing ->
      abortTc ("missing resolver annotation for binder " <> show (unqualifiedNameText name))

termResolution :: [Annotation] -> Maybe ResolutionAnnotation
termResolution =
  find ((== ResolutionNamespaceTerm) . resolutionNamespace)
    . mapMaybe fromAnnotation

lookupTyCon :: Text -> TcM (Maybe TyConInfo)
lookupTyCon name =
  lift $ gets $ find matches . Map.elems . tcsGlobalTyCons
  where
    matches info = tciName info == name || tyConName (tciTyCon info) == name

lookupResolvedTyCon :: Name -> TcM (Maybe TyConInfo)
lookupResolvedTyCon name =
  case typeResolution (nameAnns name) of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName} -> do
      exact <- lookupTyConOrigin packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName)
      maybe (lookupTyCon (nameText name)) (pure . Just) exact
    _ -> lookupTyCon (nameText name)

lookupDeclaredTyCon :: UnqualifiedName -> TcM (Maybe TyConInfo)
lookupDeclaredTyCon name =
  case typeResolution (unqualifiedNameAnns name) of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName} -> do
      exact <- lookupTyConOrigin packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName)
      maybe (lookupTyCon (unqualifiedNameText name)) (pure . Just) exact
    _ -> lookupTyCon (unqualifiedNameText name)

lookupTyConByIdentity :: TyCon -> TcM (Maybe TyConInfo)
lookupTyConByIdentity tyCon = lift $ gets $ Map.lookup tyCon . tcsGlobalTyCons

lookupTyConOrigin :: PackageId -> Text -> Text -> TcM (Maybe TyConInfo)
lookupTyConOrigin packageId moduleName name =
  lift $ gets $ find matches . Map.elems . tcsGlobalTyCons
  where
    matches info =
      let tyCon = tciTyCon info
       in tyConPackageId tyCon == packageId
            && tyConModuleName tyCon == moduleName
            && tyConName tyCon == name

typeResolution :: [Annotation] -> Maybe ResolutionAnnotation
typeResolution =
  find ((== ResolutionNamespaceType) . resolutionNamespace)
    . mapMaybe fromAnnotation

getTyConEnv :: TcM (Map TyCon TyConInfo)
getTyConEnv = lift $ gets tcsGlobalTyCons

extendTyConEnvPermanent :: Text -> TyConInfo -> TcM ()
extendTyConEnvPermanent _ info = lift $ modify' $ \s ->
  s {tcsGlobalTyCons = Map.insert (tciTyCon info) info (tcsGlobalTyCons s)}

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
