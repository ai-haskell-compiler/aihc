{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module System.Metrics.StatsD
  ( StatCounter,
    StatGauge,
    StatTiming,
    StatSet,
    Stats,
    StatConfig (..),
    newStatCounter,
    newStatGauge,
    newStatTiming,
    newStatSet,
    incrementCounter,
    setGauge,
    incrementGauge,
    decrementGauge,
    addTiming,
    newSetElement,
    withStats,
    defStatConfig,
  )
where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as C
import Data.HashSet qualified as HashSet
import System.Metrics.StatsD.Internal
  ( MetricData (..),
    StatConfig (..),
    StatCounter (..),
    StatGauge (..),
    StatSet (..),
    StatTiming (..),
    Stats,
    Value (..),
    connectStatsD,
    newMetric,
    newMetrics,
    newStats,
    processSample,
    statsLoop,
  )
import UnliftIO (MonadIO, MonadUnliftIO, throwIO)
import UnliftIO.Async (link, withAsync)

type Key = String

defStatConfig :: StatConfig
defStatConfig =
  StatConfig
    { reportStats = True,
      reportSamples = True,
      namespace = "",
      prefixStats = "stats",
      prefixCounter = "counters",
      prefixTimer = "timers",
      prefixGauge = "gauges",
      prefixSet = "sets",
      statsdServer = "127.0.0.1",
      statsdPort = 8125,
      flushInterval = 1000,
      timingPercentiles = [90, 95],
      appendNewline = False
    }

newStatCounter ::
  (MonadIO m) => Stats -> Key -> Int -> m StatCounter
newStatCounter stats key sampling = do
  when (sampling < 0) $
    throwIO $
      userError "Counter sampling rate should not be negative"
  newMetric stats (C.pack key) (CounterData 0)
  return $ StatCounter stats (C.pack key) sampling

newStatGauge ::
  (MonadIO m) => Stats -> Key -> Int -> m StatGauge
newStatGauge stats key ini = do
  newMetric stats (C.pack key) (GaugeData ini)
  return $ StatGauge stats (C.pack key)

newStatTiming :: (MonadIO m) => Stats -> Key -> Int -> m StatTiming
newStatTiming stats key sampling = do
  when (sampling < 0) $
    throwIO $
      userError "Timing sampling rate should not be negative"
  newMetric stats (C.pack key) (TimingData [])
  return $ StatTiming stats (C.pack key) sampling

newStatSet :: (MonadIO m) => Stats -> Key -> m StatSet
newStatSet stats key = do
  newMetric stats (C.pack key) (SetData HashSet.empty)
  return $ StatSet stats (C.pack key)

incrementCounter :: (MonadIO m) => StatCounter -> Int -> m ()
incrementCounter StatCounter {..} =
  processSample stats sampling key . Counter

setGauge :: (MonadIO m) => StatGauge -> Int -> m ()
setGauge StatGauge {..} i =
  processSample stats 1 key (Gauge i False)

incrementGauge :: (MonadIO m) => StatGauge -> Int -> m ()
incrementGauge StatGauge {..} i =
  processSample stats 1 key (Gauge i True)

decrementGauge :: (MonadIO m) => StatGauge -> Int -> m ()
decrementGauge x i = incrementGauge x (negate i)

addTiming :: (MonadIO m) => StatTiming -> Int -> m ()
addTiming StatTiming {..} =
  processSample stats sampling key . Timing

newSetElement :: (MonadIO m) => StatSet -> String -> m ()
newSetElement StatSet {..} =
  processSample stats 1 key . Set . C.pack

withStats :: (MonadUnliftIO m) => StatConfig -> (Stats -> m a) -> m a
withStats cfg go = do
  metrics <- newMetrics
  socket <- connectStatsD cfg.statsdServer cfg.statsdPort
  let stats = newStats cfg metrics socket
  if cfg.reportStats
    then withAsync (statsLoop stats) (\a -> link a >> go stats)
    else go stats