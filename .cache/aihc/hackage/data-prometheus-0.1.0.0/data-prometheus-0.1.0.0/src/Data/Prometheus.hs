module Data.Prometheus
  ( parseProm
  , runMetrics
  , runMetricsT
  , filterMetrics
  , findMetrics
  , hasLabel
  , byLabel
  , byLabel'
  , module Data.Prometheus.Monad
  , module Data.Prometheus.Parse
  , module Data.Prometheus.Pretty
  , module Data.Prometheus.Types
  ) where

import Control.Monad.Identity (runIdentity)
import Data.Text (Text)
import Data.Map (Map)
import Data.Attoparsec.Text

import qualified Data.Text
import qualified Data.Map
import Data.Prometheus.Monad
import Data.Prometheus.Parse
import Data.Prometheus.Pretty
import Data.Prometheus.Types

-- | Parse Prometheus metrics from Text
parseProm
  :: Text
  -> Either String (Map MetricId Metric)
parseProm = parseOnly parseMetrics

-- | Evaluate metrics and return pretty-printed output
-- as expected by textfile collector
runMetricsT
  :: Monad m
  => MetricId
  -> MetricsT m
  -> m Text
runMetricsT rootMetric x = do
  ms <- execMetricsT rootMetric x
  pure
    $ mconcat
        [ prettyMetrics (metrics ms)
        , Data.Text.unlines (errors ms)
        ]

-- | Evaluate metrics and return pretty-printed output
-- as expected by textfile collector
runMetrics
  :: MetricId
  -> Metrics
  -> Text
runMetrics rootMetric =
    runIdentity
  . runMetricsT rootMetric

-- | Filter metrics where name is prefixed by `pattern`
filterMetrics :: Text -> Map MetricId a -> Map MetricId a
filterMetrics pattern =
  Data.Map.filterWithKey
    (\k _ -> pattern `Data.Text.isPrefixOf` (metricIdName k))

-- | Find metrics where name is equal to `pattern`
findMetrics :: Text -> Map MetricId a -> Map MetricId a
findMetrics pattern =
  Data.Map.filterWithKey (\k _ -> pattern == metricIdName k)

-- | Find metrics by `label`
hasLabel :: Text -> Map MetricId a -> Map MetricId a
hasLabel label' =
  Data.Map.filterWithKey (\k _ -> Data.Map.member label' (metricIdLabels k))

-- | Find metrics with `label` which matches `contents`
byLabel
  :: Text
  -> Text
  -> Map MetricId a
  -> Map MetricId a
byLabel label' contents = byLabel' label' (==contents)

-- | Find metrics with `label` which content satisfies `op` predicate
byLabel'
  :: Text
  -> (Text -> Bool)
  -> Map MetricId a
  -> Map MetricId a
byLabel' label' op =
  Data.Map.filterWithKey
    $ \k _ ->
      case Data.Map.lookup label' (metricIdLabels k) of
        Nothing -> False
        Just lc -> op lc

