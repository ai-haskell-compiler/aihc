module Aihc.Resolve.Monad
  ( ResolveM,
    runResolveM,
    resolution,
    withResolution,
    ModuleInfo (..),
    currentModuleInfo,
    currentScope,
    currentSpan,
    withScope,
    extendScope,
    withAmbientSpan,
    withEffectiveSpan,
    withPushedSpan,
    freshLocal,
    withResetLocalSupply,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Extension,
    SourceSpan,
    UnqualifiedName,
    mkAnnotation,
  )
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Types
import Control.Applicative ((<|>))

data ResolveEnv = ResolveEnv
  { envScope :: !Scope,
    envModuleInfo :: !ModuleInfo,
    envSpan :: !(Maybe SourceSpan)
  }

data ModuleInfo = ModuleInfo
  { moduleInfoExtensions :: ![Extension],
    moduleInfoExplicitPreludeImport :: !Bool,
    moduleInfoBuiltinScope :: !Scope
  }

data ResolveState = ResolveState
  { stateNextLocal :: !Int,
    -- | The resolutions that failed, most recent first. 'runResolveM' puts
    -- them back in source order.
    stateErrors :: ![ResolveError]
  }

newtype ResolveM a = ResolveM
  { unResolveM :: ResolveEnv -> ResolveState -> (a, ResolveState)
  }

instance Functor ResolveM where
  fmap f action =
    ResolveM $ \env state ->
      let (result, state') = unResolveM action env state
       in (f result, state')

instance Applicative ResolveM where
  pure result = ResolveM $ \_ state -> (result, state)

  fun <*> arg =
    ResolveM $ \env state ->
      let (f, state') = unResolveM fun env state
          (result, state'') = unResolveM arg env state'
       in (f result, state'')

instance Monad ResolveM where
  action >>= next =
    ResolveM $ \env state ->
      let (result, state') = unResolveM action env state
       in unResolveM (next result) env state'

-- | Run a resolution, and give what it produced together with the next
-- unused local number and every resolution that failed, in source order.
runResolveM :: Scope -> ModuleInfo -> Int -> ResolveM a -> (Int, [ResolveError], a)
runResolveM scope moduleInfo nextLocal action =
  let initialEnv = ResolveEnv {envScope = scope, envModuleInfo = moduleInfo, envSpan = Nothing}
      initialState = ResolveState {stateNextLocal = nextLocal, stateErrors = []}
      (result, finalState) = unResolveM action initialEnv initialState
   in (stateNextLocal finalState, reverse (stateErrors finalState), result)

-- | Attach a resolution to a piece of syntax, and record the resolution
-- if it failed.
--
-- Every resolution annotation the resolver attaches to syntax is made here.
-- A failed resolution is thus collected as it is made, and nothing has to
-- walk the resolved syntax afterwards looking for one.
--
-- The caller passes what to attach the annotation to rather than taking
-- the annotation and attaching it itself. This runs on every name the
-- resolver looks at, and the state monad pays for a bind in allocation, so
-- the whole thing is one step.
withResolution ::
  Maybe SourceSpan ->
  Identifier ->
  ResolutionNamespace ->
  ResolvedName ->
  (Annotation -> a) ->
  ResolveM a
withResolution span' identifier namespace target attach =
  ResolveM $ \_env state ->
    let annotation = ResolutionAnnotation span' identifier namespace target
        -- Settle whether this one failed now rather than leaving a thunk
        -- over the state behind for every name in the module.
        state' = case target of
          ResolvedError message -> state {stateErrors = resolutionError annotation message : stateErrors state}
          ResolvedTopLevel {} -> state
          ResolvedLocal {} -> state
          ResolvedSyntax -> state
     in state' `seq` (attach (mkAnnotation annotation), state')
{-# INLINE withResolution #-}

-- | The annotation for one resolution, when the caller has nothing to
-- attach it to yet.
resolution :: Maybe SourceSpan -> Identifier -> ResolutionNamespace -> ResolvedName -> ResolveM Annotation
resolution span' identifier namespace target =
  withResolution span' identifier namespace target id

asks :: (ResolveEnv -> a) -> ResolveM a
asks f = ResolveM $ \env state -> (f env, state)

gets :: (ResolveState -> a) -> ResolveM a
gets f = ResolveM $ \_ state -> (f state, state)

modify' :: (ResolveState -> ResolveState) -> ResolveM ()
modify' f =
  ResolveM $ \_ state ->
    let state' = f state
     in state' `seq` ((), state')

local :: (ResolveEnv -> ResolveEnv) -> ResolveM a -> ResolveM a
local f action =
  ResolveM $ \env state -> unResolveM action (f env) state

currentScope :: ResolveM Scope
currentScope = asks envScope

currentModuleInfo :: ResolveM ModuleInfo
currentModuleInfo = asks envModuleInfo

currentSpan :: ResolveM (Maybe SourceSpan)
currentSpan = asks envSpan

withScope :: Scope -> ResolveM a -> ResolveM a
withScope scope = local (\env -> env {envScope = scope})

extendScope :: Scope -> ResolveM a -> ResolveM a
extendScope localScope = local (\env -> env {envScope = localScope `unionScope` envScope env})

withAmbientSpan :: Maybe SourceSpan -> ResolveM a -> ResolveM a
withAmbientSpan span' = local (\env -> env {envSpan = span'})

-- | Resolve under @localSpan@ when the node carries one, and otherwise keep
-- the span already in force.
withEffectiveSpan :: Maybe SourceSpan -> ResolveM a -> ResolveM a
withEffectiveSpan localSpan action = do
  ambient <- currentSpan
  withAmbientSpan (localSpan <|> ambient) action

withPushedSpan :: Annotation -> ResolveM a -> ResolveM a
withPushedSpan ann action = do
  ambient <- currentSpan
  withAmbientSpan (pushSpanFromAnn ambient ann) action

freshLocal :: UnqualifiedName -> ResolveM ResolvedName
freshLocal name = do
  currentId <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = currentId + 1})
  pure (ResolvedLocal currentId name)

withResetLocalSupply :: ResolveM a -> ResolveM a
withResetLocalSupply action = do
  savedNextLocal <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = 0})
  result <- action
  modify' (\state -> state {stateNextLocal = savedNextLocal})
  pure result
