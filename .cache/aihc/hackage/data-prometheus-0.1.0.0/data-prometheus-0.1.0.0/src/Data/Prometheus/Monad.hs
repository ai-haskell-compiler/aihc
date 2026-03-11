{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Monad
  ( MetricState(..)
  , Metrics
  , MetricsT
  , ToMetrics(..)
  , execMetricsT
  , addMetric'
  , addMetric
  , subMetrics
  , labeledMetrics
  , metric
  , sub
  , desc
  , label
  , mkGauge
  , mkCounter
  , eitherExitCode
  , eitherToGauge
  , boolToGauge
  , enumToGauge
  , floatToGauge
  , logError
  ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map
import qualified Data.Text
import qualified GHC.Float

import Data.Prometheus.Types

data MetricState = MetricState
  { baseMetric :: MetricId
  , metrics :: Map MetricId Metric
  , errors  :: [Text]
  }

type MetricsT m = StateT MetricState m ()

type Metrics = MetricsT Identity

class ToMetrics a where
  toMetrics
    :: Monad m
    => a
    -> MetricsT m

instance ToMetrics a => ToMetrics [a] where
  toMetrics xs =
    mapM_
      (\(k, v) ->
        labeledMetrics
          "id"
          (Data.Text.pack $ show k)
          $ toMetrics v
      )
      (zip [(0 :: Int)..] xs)

-- | Evaluate metrics into `MetricState`
execMetricsT
  :: Monad m
  => MetricId
  -> MetricsT m
  -> m MetricState
execMetricsT rootMetric =
  flip
    execStateT
    (MetricState rootMetric mempty mempty)

-- | Add metric with value
addMetric'
  :: Monad m
  => (MetricId -> MetricId) -- ^ Function to change the current @MetricId@
  -> Metric -- ^ @Metric@ to add
  -> MetricsT m
addMetric' f mData = do
  mId <- f <$> gets baseMetric
  modify $ \ms ->
    ms { metrics = Data.Map.insert mId mData (metrics ms) }

-- | Add metric with value
addMetric
  :: Monad m
  => Text -- ^ Suffix (sub metric to add)
  -> Metric -- ^ @Metric@ to add
  -> MetricsT m
addMetric subName = addMetric' (sub subName)

-- | Combinator to create sub-metrics
subMetrics
  :: Monad m
  => Text
  -> MetricsT m
  -> MetricsT m
subMetrics subName act = do
  old <- gets baseMetric
  modify $ \ms ->
    ms { baseMetric = sub subName $ baseMetric ms }
  act
  modify $ \ms ->
    ms { baseMetric = old }

-- | Combinator to create labeled metrics
labeledMetrics
  :: Monad m
  => Text -- ^ Label name
  -> Text -- ^ Label value
  -> MetricsT m
  -> MetricsT m
labeledMetrics labelName labelValue act = do
  old <- gets baseMetric
  modify $ \ms ->
    ms { baseMetric = label labelName labelValue $ baseMetric ms }
  act
  modify $ \ms ->
    ms { baseMetric = old }

-- | Create metric with just `name`
metric
  :: Text
  -> MetricId
metric mName = MetricId mName mempty mempty

-- | Append `subName` to the name of a @MetricId@
--
-- > metric "a" & sub "b"
-- results in name "a_b"
sub
  :: Text
  -> MetricId
  -> MetricId
sub subName m =
  m { metricIdName = metricIdName m <> "_" <> subName }

-- | Set help text / description of a @MetricId@
desc
  :: Text
  -> MetricId
  -> MetricId
desc h m =
  m { metricIdHelp = h }

-- | Add label to MetricId
label
  :: Text
  -> Text
  -> MetricId
  -> MetricId
label k v m =
  m { metricIdLabels = Data.Map.insert k v (metricIdLabels m) }

-- | Create @Gauge@ metric
mkGauge
  :: Double
  -> Metric
mkGauge = Gauge

-- | Create @Counter@ metric
mkCounter
  :: Double
  -> Metric
mkCounter = Counter

-- | Right is exitcode 0, Left non-zero
eitherExitCode :: Either a b -> Integer
eitherExitCode (Right _) = 0
eitherExitCode (Left _) = 1

-- | Convert Either to Gauge, 0 meaning Right
eitherToGauge :: Either a b -> Metric
eitherToGauge = mkGauge . fromIntegral . eitherExitCode

-- | Convert Bool to Gauge, 0 meaning False
boolToGauge :: Bool -> Metric
boolToGauge False = mkGauge 0
boolToGauge True = mkGauge 1

-- | Convert Enum to Gauge, 0 (typically) meaning Ok status
enumToGauge :: Enum a => a -> Metric
enumToGauge = mkGauge . fromIntegral . fromEnum

-- | Convert @Float@ to Gauge
floatToGauge
  :: Float
  -> Metric
floatToGauge = mkGauge . GHC.Float.float2Double

-- | Log error message
--
-- These are appended after all metrics were printed
--
-- Not a standard token but textfile collector ignores it as a comment
-- and we can use it to provide some insight to our scripts.
logError
  :: Monad m
  => Text
  -> StateT MetricState m ()
logError err =
  modify $ \ms -> ms { errors = (errors ms) ++ [errComment] }
  where
    errComment =
      Data.Text.unwords
      [ "# ERROR"
      , err
      ]
