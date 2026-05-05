module Aihc.Resolve.Monad
  ( ResolveM,
    runResolveM,
    currentScope,
    currentSpan,
    withScope,
    extendScope,
    withAmbientSpan,
    withEffectiveSpan,
    withPushedSpan,
    freshLocal,
    withLocalSupply,
    emitAnnotation,
    emitAnnotations,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    SourceSpan (..),
    UnqualifiedName,
  )
import Aihc.Resolve.Scope
import Aihc.Resolve.Span
import Aihc.Resolve.Types

data ResolveEnv = ResolveEnv
  { envScope :: !Scope,
    envSpan :: !SourceSpan
  }

data ResolveState = ResolveState
  { stateNextLocal :: !Int,
    stateGeneratedAnnotations :: [ResolutionAnnotation]
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

runResolveM :: Scope -> Int -> ResolveM a -> (Int, [ResolutionAnnotation], a)
runResolveM scope nextLocal action =
  let initialEnv = ResolveEnv {envScope = scope, envSpan = NoSourceSpan}
      initialState = ResolveState {stateNextLocal = nextLocal, stateGeneratedAnnotations = []}
      (result, finalState) = unResolveM action initialEnv initialState
   in (stateNextLocal finalState, reverse (stateGeneratedAnnotations finalState), result)

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

currentSpan :: ResolveM SourceSpan
currentSpan = asks envSpan

withScope :: Scope -> ResolveM a -> ResolveM a
withScope scope = local (\env -> env {envScope = scope})

extendScope :: Scope -> ResolveM a -> ResolveM a
extendScope localScope = local (\env -> env {envScope = localScope `unionScope` envScope env})

withAmbientSpan :: SourceSpan -> ResolveM a -> ResolveM a
withAmbientSpan span' = local (\env -> env {envSpan = span'})

withEffectiveSpan :: SourceSpan -> ResolveM a -> ResolveM a
withEffectiveSpan localSpan action = do
  ambient <- currentSpan
  withAmbientSpan (effectiveResolutionSpan ambient localSpan) action

withPushedSpan :: Annotation -> ResolveM a -> ResolveM a
withPushedSpan ann action = do
  ambient <- currentSpan
  withAmbientSpan (pushSpanFromAnn ambient ann) action

freshLocal :: UnqualifiedName -> ResolveM ResolvedName
freshLocal name = do
  currentId <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = currentId + 1})
  pure (ResolvedLocal currentId name)

withLocalSupply :: Int -> ResolveM a -> ResolveM a
withLocalSupply nextLocal action = do
  savedNextLocal <- gets stateNextLocal
  modify' (\state -> state {stateNextLocal = nextLocal})
  result <- action
  modify' (\state -> state {stateNextLocal = savedNextLocal})
  pure result

emitAnnotation :: ResolutionAnnotation -> ResolveM ()
emitAnnotation annotation =
  modify' (\state -> state {stateGeneratedAnnotations = annotation : stateGeneratedAnnotations state})

emitAnnotations :: [ResolutionAnnotation] -> ResolveM ()
emitAnnotations = mapM_ emitAnnotation
