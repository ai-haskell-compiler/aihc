{-# LANGUAGE OverloadedStrings #-}

-- | The type checker monad and state.
module Aihc.Tc.Monad
  ( -- * Monad
    TcM,
    runTcM,
    abortTc,

    -- * State
    TcState (..),
    initTcState,

    -- * Fresh names
    freshUnique,
    freshMetaTv,
    freshSkolemTv,
    freshEvVar,

    -- * Meta-variable solutions
    writeMetaTv,
    readMetaTv,
    writeKindMeta,
    readKindMeta,

    -- * Evidence
    bindEvidence,
    lookupEvidence,

    -- * Environment
    TcEnv (..),
    TcBinder (..),
    Closedness (..),
    emptyTcEnv,
    lookupTerm,
    extendTermEnv,
    extendTermEnvPermanent,
    getTermEnv,
    lookupTyCon,
    extendTyConEnvPermanent,
    getTyConEnv,
    localTcOptions,
    tcMonoLocalBinds,
    tcMonomorphismRestriction,
    getTcLevel,
    withTcLevel,
    addInstance,
    getInstances,

    -- * GADT constructor registry
    markGadtCon,
    isGadtCon,

    -- * Diagnostics
    emitDiagnostic,
    emitError,
    getDiagnostics,
    withErrorTracking,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Env (InstanceInfo, TyConInfo)
import Aihc.Tc.Error
import Aihc.Tc.Evidence
import Aihc.Tc.Types
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | The local typing environment (read-only within a scope).
data TcEnv = TcEnv
  { -- | Term bindings in scope (variables -> types).
    tcEnvTerms :: !(Map Text TcBinder),
    -- | Whether local binding groups follow GHC's MonoLocalBinds rule.
    tcEnvMonoLocalBinds :: !Bool,
    -- | Whether the monomorphism restriction is active.
    tcEnvMonomorphismRestriction :: !Bool,
    -- | Current implication nesting level.
    tcEnvTcLevel :: !TcLevel
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
    TcIdBinder !Text !TypeScheme !Closedness
  | -- | Monomorphic binding (lambda-bound, pattern-bound, local let).
    TcMonoIdBinder !Text !TcType
  deriving (Show)

-- | An empty environment at the top level.
emptyTcEnv :: TcEnv
emptyTcEnv =
  TcEnv
    { tcEnvTerms = Map.empty,
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
    -- | Evidence bindings accumulated during solving.
    tcsEvBinds :: !(Map Unique EvTerm),
    -- | Diagnostics (errors and warnings) collected.
    tcsDiagnostics :: ![TcDiagnostic],
    -- | Global term bindings (accumulated by top-level declarations).
    tcsGlobalTerms :: !(Map Text TcBinder),
    -- | Global type constructors accumulated by top-level declarations.
    tcsGlobalTyCons :: !(Map Text TyConInfo),
    -- | Class instances in scope.
    tcsInstances :: ![InstanceInfo],
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
      tcsEvBinds = Map.empty,
      tcsDiagnostics = [],
      tcsGlobalTerms = builtinTerms,
      tcsGlobalTyCons = Map.empty,
      tcsInstances = [],
      tcsGadtCons = Set.empty
    }

builtinTerms :: Map Text TcBinder
builtinTerms =
  Map.fromList
    [ (":", TcIdBinder ":" consScheme Closed),
      ("[]", TcIdBinder "[]" nilScheme Closed)
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

-- | Allocate a fresh skolem (rigid) type variable.
freshSkolemTv :: Text -> TcM TyVarId
freshSkolemTv name = do
  u <- freshUnique
  pure (TyVarId {tvName = name, tvUnique = u})

-- | Allocate a fresh evidence variable.
freshEvVar :: TcM EvVar
freshEvVar = EvVar <$> freshUnique

-- | Record the solution for a meta-variable.
writeMetaTv :: Unique -> TcType -> TcM ()
writeMetaTv u ty = lift $ modify' $ \s ->
  s {tcsMetaSolutions = Map.insert u ty (tcsMetaSolutions s)}

-- | Look up the current solution for a meta-variable.
readMetaTv :: Unique -> TcM (Maybe TcType)
readMetaTv u = lift $ gets $ \s ->
  Map.lookup u (tcsMetaSolutions s)

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

-- | Look up a term in the environment (local first, then global state).
lookupTerm :: Text -> TcM (Maybe TcBinder)
lookupTerm name = do
  localResult <- asks $ \env -> Map.lookup name (tcEnvTerms env)
  case localResult of
    Just _ -> pure localResult
    Nothing -> lift $ gets $ \s -> Map.lookup name (tcsGlobalTerms s)

-- | Snapshot all visible term bindings, with local bindings shadowing globals.
getTermEnv :: TcM (Map Text TcBinder)
getTermEnv = do
  locals <- asks tcEnvTerms
  globals <- lift $ gets tcsGlobalTerms
  pure (locals <> globals)

-- | Extend the term environment with a new binding for the duration
-- of the given computation.
extendTermEnv :: Text -> TcBinder -> TcM a -> TcM a
extendTermEnv name binder =
  local $ \env ->
    env {tcEnvTerms = Map.insert name binder (tcEnvTerms env)}

-- | Permanently extend the global term environment (for top-level
-- declarations like data constructors and top-level bindings).
extendTermEnvPermanent :: Text -> TcBinder -> TcM ()
extendTermEnvPermanent name binder = lift $ modify' $ \s ->
  s {tcsGlobalTerms = Map.insert name binder (tcsGlobalTerms s)}

lookupTyCon :: Text -> TcM (Maybe TyConInfo)
lookupTyCon name = lift $ gets $ \s -> Map.lookup name (tcsGlobalTyCons s)

getTyConEnv :: TcM (Map Text TyConInfo)
getTyConEnv = lift $ gets tcsGlobalTyCons

extendTyConEnvPermanent :: Text -> TyConInfo -> TcM ()
extendTyConEnvPermanent name info = lift $ modify' $ \s ->
  s {tcsGlobalTyCons = Map.insert name info (tcsGlobalTyCons s)}

addInstance :: InstanceInfo -> TcM ()
addInstance instanceInfo = lift $ modify' $ \s ->
  s {tcsInstances = instanceInfo : tcsInstances s}

getInstances :: TcM [InstanceInfo]
getInstances = lift $ gets tcsInstances

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
      { diagLoc = loc,
        diagSeverity = TcError,
        diagKind = kind
      }

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
