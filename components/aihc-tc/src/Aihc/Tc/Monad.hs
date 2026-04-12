-- | The type checker monad and state.
module Aihc.Tc.Monad
  ( -- * Monad
    TcM,
    runTcM,

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

    -- * Evidence
    bindEvidence,
    lookupEvidence,

    -- * Environment
    TcEnv (..),
    TcBinder (..),
    emptyTcEnv,
    lookupTerm,
    extendTermEnv,
    extendTermEnvPermanent,
    getTcLevel,
    withTcLevel,

    -- * Diagnostics
    emitDiagnostic,
    emitError,
    getDiagnostics,
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Error
import Aihc.Tc.Evidence
import Aihc.Tc.Types
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | The local typing environment (read-only within a scope).
data TcEnv = TcEnv
  { -- | Term bindings in scope (variables -> types).
    tcEnvTerms :: !(Map Text TcBinder),
    -- | Current implication nesting level.
    tcEnvTcLevel :: !TcLevel
  }
  deriving (Show)

-- | A binding in the term environment.
data TcBinder
  = -- | Polymorphic binding (top-level or let with signature).
    TcIdBinder !Text !TypeScheme
  | -- | Monomorphic binding (lambda-bound, pattern-bound, local let).
    TcMonoIdBinder !Text !TcType
  deriving (Show)

-- | An empty environment at the top level.
emptyTcEnv :: TcEnv
emptyTcEnv =
  TcEnv
    { tcEnvTerms = Map.empty,
      tcEnvTcLevel = topTcLevel
    }

-- | The mutable state of the type checker.
data TcState = TcState
  { -- | Next unique identifier to allocate.
    tcsNextUnique :: !Int,
    -- | Solutions for meta (unification) variables.
    tcsMetaSolutions :: !(Map Unique TcType),
    -- | Evidence bindings accumulated during solving.
    tcsEvBinds :: !(Map Unique EvTerm),
    -- | Diagnostics (errors and warnings) collected.
    tcsDiagnostics :: ![TcDiagnostic],
    -- | Global term bindings (accumulated by top-level declarations).
    tcsGlobalTerms :: !(Map Text TcBinder)
  }
  deriving (Show)

-- | Initial state with no variables or bindings.
initTcState :: TcState
initTcState =
  TcState
    { tcsNextUnique = 0,
      tcsMetaSolutions = Map.empty,
      tcsEvBinds = Map.empty,
      tcsDiagnostics = [],
      tcsGlobalTerms = Map.empty
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
