{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for controlling time-related operations.
-}
module Data.Effect.Concurrent.Timer where

import Control.Concurrent.Thread.Delay qualified as Thread
import Control.Effect (perform)
import Control.Effect.Interpret (interpose)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Effect (Emb)
import Data.Effect.Coroutine (Yield, yield)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Time (DiffTime)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import GHC.Clock (getMonotonicTimeNSec)

-- | An effect for time-related operations.
data Timer :: Effect where
    -- | Retrieves the current relative time from an arbitrary fixed reference point.
    --   The reference point does not change within the context of that scope.
    Clock :: Timer f DiffTime
    -- | Temporarily suspends computation for the specified duration.
    Sleep :: DiffTime -> Timer f ()

makeEffectF ''Timer

{- |
Creates a scope where elapsed time can be obtained.
An action to retrieve the elapsed time, re-zeroed at the start of the scope, is passed to the scope.
-}
withElapsedTime
    :: forall a es ff c
     . (Timer :> es, Monad (Eff ff es), Free c ff)
    => (Eff ff es DiffTime -> Eff ff es a)
    -- ^ A scope where the elapsed time can be obtained.
    -- An action to retrieve the elapsed time is passed as an argument.
    -> Eff ff es a
withElapsedTime f = do
    start <- clock
    f $ clock <&> (`subtract` start)
{-# INLINE withElapsedTime #-}

-- | Returns the time taken for a computation along with the result as a pair.
measureTime
    :: forall a es ff c
     . (Timer :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es a
    -> Eff ff es (DiffTime, a)
measureTime m = withElapsedTime \elapsedTime -> do
    r <- m
    elapsedTime <&> (,r)
{-# INLINE measureTime #-}

{- |
Temporarily suspends computation until the relative time from the fixed reference point in the current scope's context, as given by the argument.
If the specified resume time has already passed, returns the elapsed time (positive value) in `Just`.
-}
sleepUntil :: forall es ff c. (Timer :> es, Monad (Eff ff es), Free c ff) => DiffTime -> Eff ff es (Maybe DiffTime)
sleepUntil t = do
    now <- clock
    when (t > now) do
        sleep $ t - now
    pure if t < now then Just (now - t) else Nothing
{-# INLINE sleepUntil #-}

{- |
Repeats a computation indefinitely. Controls so that each loop occurs at a specific time interval.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
runCyclic
    :: forall a es ff c
     . (Timer :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es DiffTime
    -- ^ An action called at the start of each loop to determine the time interval until the next loop.
    --   For example, @pure 1@ would control the loop to have a 1-second interval.
    -> Eff ff es ()
    -- ^ The computation to repeat.
    -> Eff ff es a
runCyclic deltaTime a = do
    t0 <- clock
    flip fix t0 \next t -> do
        t' <- (t +) <$> deltaTime
        a
        delay <- sleepUntil t'
        next $ maybe t' (t' +) delay
{-# INLINE runCyclic #-}

{- |
Controls to repeat a specified computation at fixed time intervals. A specialized version of `runCyclic`.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
runPeriodic
    :: forall a es ff c
     . (Timer :> es, Monad (Eff ff es), Free c ff)
    => DiffTime
    -- ^ Loop interval
    -> Eff ff es ()
    -- ^ The computation to repeat.
    -> Eff ff es a
runPeriodic interval = runCyclic (pure interval)
{-# INLINE runPeriodic #-}

{- |
Calls `yield` of a coroutine at fixed intervals.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
periodicTimer
    :: forall a es ff c
     . (Timer :> es, Yield () () :> es, Monad (Eff ff es), Free c ff)
    => DiffTime
    -> Eff ff es a
periodicTimer interval = runPeriodic interval $ yield ()
{-# INLINE periodicTimer #-}

{- |
Calls `yield` of a coroutine at specific intervals.
Controls so that the time returned by `yield` becomes the time interval until the next loop.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
cyclicTimer
    :: forall a es ff c
     . (Timer :> es, Yield () DiffTime :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es a
cyclicTimer = runCyclic (yield ()) (pure ())
{-# INLINE cyclicTimer #-}

-- | An effect that realizes control of wait times such that the specified time becomes the interval until the next @wait@ when @wait@ is executed repeatedly.
data CyclicTimer :: Effect where
    -- | Controls the wait time so that when @wait@ is executed repeatedly, the specified time becomes the interval until the next @wait@.
    Wait :: DiffTime -> CyclicTimer f ()

makeEffectF ''CyclicTimer

runTimerIO
    :: forall a ff es c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Timer ': es) a
    -> Eff ff es a
runTimerIO =
    interpret \case
        Clock -> do
            t <- getMonotonicTimeNSec & liftIO
            pure $ picosecondsToDiffTime $ fromIntegral t * 1000
        Sleep t ->
            Thread.delay (diffTimeToPicoseconds t `quot` 1000_000) & liftIO
{-# INLINE runTimerIO #-}

-- | Re-zeros the clock time in the local scope.
restartClock :: forall a ff es c. (Timer :> es, Monad (Eff ff es), Free c ff) => Eff ff es a -> Eff ff es a
restartClock a = do
    t0 <- clock
    a & interpose \case
        Clock -> do
            t <- clock
            pure $ t - t0
        other -> perform other
{-# INLINE restartClock #-}
