{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for parallel computations.
-}
module Data.Effect.Concurrent.Parallel where

#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (forever)
import Data.Effect (Emb, UnliftIO)
import Data.Function (fix)
import Data.Tuple (swap)
import UnliftIO (
    MonadIO,
    MonadUnliftIO,
    atomically,
    liftIO,
    mask,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryReadTMVar,
    uninterruptibleMask_,
    withRunInIO,
 )
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)

-- | An `Applicative`-based effect for executing computations in parallel.
data Parallel :: Effect where
    -- | Executes two actions in parallel and blocks until both are complete.
    -- Finally, aggregates the execution results based on the specified function.
    LiftP2
        :: (a -> b -> c)
        -- ^ A function that aggregates the two execution results.
        -> f a
        -- ^ The first action to be executed in parallel.
        -> f b
        -- ^ The second action to be executed in parallel.
        -> Parallel f c

-- | An effect that blocks a computation indefinitely.
data Halt :: Effect where
    -- | Blocks a computation indefinitely.
    Halt :: Halt f a

{- |
An effect that adopts the result of the computation that finishes first among
two computations and cancels the other.
-}
data Race :: Effect where
    -- | Adopts the result of the computation that finishes first among two
    --   computations and cancels the other.
    Race :: f a -> f a -> Race f a

makeEffectF ''Halt
makeEffectsH [''Parallel, ''Race]

{- |
A wrapper that allows using the `Parallel` effect in the form of `Applicative` /
 `Alternative` instances.
-}
newtype Concurrently ff es a = Concurrently {runConcurrently :: Eff ff es a}

deriving instance (Functor (Eff ff es)) => Functor (Concurrently ff es)

instance
    (Parallel :> es, Applicative (Eff ff es), Free c ff)
    => Applicative (Concurrently ff es)
    where
    pure = Concurrently . pure
    {-# INLINE pure #-}

    liftA2 f (Concurrently a) (Concurrently b) = Concurrently $ liftP2 f a b
    {-# INLINE liftA2 #-}

instance
    (Race :> es, Halt :> es, Parallel :> es, Applicative (Eff ff es), Free c ff)
    => Alternative (Concurrently ff es)
    where
    empty = Concurrently halt
    {-# INLINE empty #-}

    (Concurrently a) <|> (Concurrently b) = Concurrently $ race a b
    {-# INLINE (<|>) #-}

{- |
Executes three actions in parallel and blocks until all are complete.
Finally, aggregates the execution results based on the specified function.
-}
liftP3
    :: forall a b c d es ff con
     . (Parallel :> es, Free con ff)
    => (a -> b -> c -> d)
    -- ^ A function that aggregates the three execution results.
    -> Eff ff es a
    -- ^ The first action to be executed in parallel.
    -> Eff ff es b
    -- ^ The second action to be executed in parallel.
    -> Eff ff es c
    -- ^ The third action to be executed in parallel.
    -> Eff ff es d
liftP3 f a b = liftP2 ($) (liftP2 f a b)
{-# INLINE liftP3 #-}

-- | An effect that realizes polling and cancellation of actions running in parallel.
data Poll :: Effect where
    -- | Performs polling on an action running in parallel in the form of a fold.
    --
    -- First, the parallel execution of two actions begins.
    --
    -- When the execution of the first action completes, polling on the second
    -- action is performed at that point, and the result is passed to the
    -- folding function. If the function returns `Left`, the folding terminates
    -- and it becomes the final result. If the second action is not yet
    -- complete, it is canceled. If the function returns `Right`, the folding
    -- continues, and the same process repeats.
    Poldl
        :: (a -> Maybe b -> f (Either r a))
        -- ^ A function for folding.
        -> f a
        -- ^ The first action to be executed in parallel.
        -> f b
        -- ^ The second action to be executed in parallel; the target of polling.
        -> Poll f r

makeEffectH ''Poll

-- | Executes two actions in parallel. If the first action completes before the second, the second action is canceled.
cancels
    :: forall a b es ff c
     . (Poll :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ The action that controls the cancellation.
    -> Eff ff es b
    -- ^ The action to be canceled.
    -> Eff ff es (a, Maybe b)
cancels = poldl $ curry $ pure . Left
{-# INLINE cancels #-}

-- | Executes two actions in parallel. If the second action completes before the first, the first action is canceled.
cancelBy
    :: forall a b es ff c
     . (Poll :> es, Applicative (Eff ff es), Free c ff)
    => Eff ff es a
    -- ^ The action to be canceled.
    -> Eff ff es b
    -- ^ The action that controls the cancellation.
    -> Eff ff es (Maybe a, b)
cancelBy = flip $ poldl $ curry $ pure . Left . swap
{-# INLINE cancelBy #-}

-- | An effect for parallel computations based on a `Traversable` container @t@.
data For (t :: Type -> Type) :: Effect where
    -- | Executes in parallel the actions stored within a `Traversable` container @t@.
    For :: t (f a) -> For t f (t a)

makeEffectH_ ''For
makeHFunctor' ''For \(t :< _) -> [t|Functor $t|]

-- | Converts the `Traversable` container-based parallel computation effect t`For` into the `Applicative`-based parallel computation effect `Parallel`.
forToParallel
    :: forall t a es ff c
     . (Parallel :> es, Traversable t, Applicative (Eff ff es), Free c ff)
    => For t (Eff ff es) a
    -> Eff ff es a
forToParallel (For iters) = runConcurrently $ traverse Concurrently iters
{-# INLINE forToParallel #-}

runConcurrentIO
    :: forall a es ff c
     . (UnliftIO :> es, Emb IO :> es, forall es'. Monad (Eff ff es'), Free c ff)
    => Eff ff (Parallel ': Race ': Poll ': Halt ': es) a
    -> Eff ff es a
runConcurrentIO = runHaltIO . runPollIO . runRaceIO . runParallelIO
{-# INLINE runConcurrentIO #-}

runParallelIO
    :: forall a es ff c
     . (UnliftIO :> es, Emb IO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Parallel ': es) a
    -> Eff ff es a
runParallelIO = interpret parallelToIO
{-# INLINE runParallelIO #-}

parallelToIO :: (MonadUnliftIO m) => Parallel ~~> m
parallelToIO (LiftP2 f a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            t <- forkIO do
                x <- restore $ run a
                atomically $ putTMVar var x

            y <- restore $ run b

            atomically do
                x <- readTMVar var
                pure $ f x y
                <* uninterruptibleMask_ (killThread t)
{-# INLINE parallelToIO #-}

runPollIO
    :: forall a es ff c
     . (Emb IO :> es, UnliftIO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Poll ': es) a
    -> Eff ff es a
runPollIO = interpret pollToIO
{-# INLINE runPollIO #-}

runRaceIO
    :: forall a es ff c
     . (Emb IO :> es, UnliftIO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Race ': es) a
    -> Eff ff es a
runRaceIO = interpret raceToIO
{-# INLINE runRaceIO #-}

runHaltIO
    :: forall a es ff c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Halt ': es) a
    -> Eff ff es a
runHaltIO = interpret haltToIO
{-# INLINE runHaltIO #-}

raceToIO :: (MonadUnliftIO m) => Race ~~> m
raceToIO (Race a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            let runThread m = forkIO do
                    x <- restore $ run m
                    atomically $ putTMVar var x

            t1 <- runThread a
            t2 <- runThread b

            atomically (readTMVar var)
                <* uninterruptibleMask_ (killThread t1 *> killThread t2)
{-# INLINE raceToIO #-}

pollToIO :: (MonadUnliftIO m) => Poll ~~> m
pollToIO (Poldl f a b) =
    withRunInIO \run -> do
        var <- newEmptyTMVarIO
        mask \restore -> do
            t <- forkIO do
                x <- restore $ run b
                atomically $ putTMVar var x

            restore (run a) >>= fix \next acc -> do
                poll <- atomically $ tryReadTMVar var
                restore (run $ f acc poll) >>= \case
                    Left r -> do
                        uninterruptibleMask_ $ killThread t
                        pure r
                    Right acc' -> next acc'
{-# INLINE pollToIO #-}

haltToIO :: (MonadIO m) => Halt ~~> m
haltToIO Halt = liftIO $ forever $ threadDelay maxBound
{-# INLINE haltToIO #-}

runParallelAsSequential
    :: forall a es ff c
     . (Applicative (Eff ff es), Free c ff)
    => Eff ff (Parallel ': es) a
    -> Eff ff es a
runParallelAsSequential = interpret parallelToSequential
{-# INLINE runParallelAsSequential #-}

parallelToSequential :: (Applicative (Eff ff es)) => Parallel ~~> Eff ff es
parallelToSequential (LiftP2 f a b) = liftA2 f a b
{-# INLINE parallelToSequential #-}

runForAsParallel
    :: forall t a es ff c
     . (Parallel :> es, Traversable t, Applicative (Eff ff es), Free c ff)
    => Eff ff (For t ': es) a
    -> Eff ff es a
runForAsParallel = interpret forToParallel
{-# INLINE runForAsParallel #-}
