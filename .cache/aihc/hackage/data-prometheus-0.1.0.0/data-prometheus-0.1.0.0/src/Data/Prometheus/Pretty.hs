{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Prometheus.Pretty
  ( prettyMetrics
  , prettyMetric
  , prettyMetricShort
  , prettyId
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.Text
import qualified Data.Map

import Data.Prometheus.Types
import Data.Prometheus.Monad

prettyMetrics :: Map MetricId Metric -> Text
prettyMetrics =
    mconcat
  . Data.Map.elems
  . snd
  . Data.Map.mapAccumWithKey
      helpTypeOnce
      mempty
  where
    -- render help and metric type just once
    -- for consecutive metridId names
    -- (with only difference in labels)
    helpTypeOnce prevName mid@MetricId{..} x | prevName /= metricIdName =
      (metricIdName, prettyMetric mid x)
    helpTypeOnce _prevName mid@MetricId{..} x | otherwise =
      (metricIdName, prettyMetricShort mid x <> "\n")

prettyMetric :: MetricId -> Metric -> Text
prettyMetric mId mData =
  Data.Text.unlines
    [ prettyHelp mId
    , prettyType mId mData
    , prettyMetricShort mId mData
    ]

prettyMetricShort
  :: MetricId
  -> Metric
  -> Text
prettyMetricShort mId mData =
  case mData of
    Counter x -> simple mId x
    Gauge x -> simple mId x
    Summary{..} ->
      Data.Text.unlines
      $ [ simple
            (label
              "quantile"
              (Data.Text.pack $ show k)
              mId
            )
            v
        | (k, v) <- Data.Map.toList sumQuantiles
        ]
        ++
        [ simple (sub "sum" mId) sumSum
        , simple (sub "count" mId) sumCount
        ]
    Histogram{..} ->
      Data.Text.unlines
      $ [ simple
            (label
              "le"
              (Data.Text.pack $ show k)
              (sub "bucket" mId)
            )
            v
        | (k, v) <- Data.Map.toList histBuckets
        ]
        ++
        [ simple (sub "sum" mId) histSum
        , simple (sub "count" mId) histCount
        ]

  where
    simple i val =
      Data.Text.unwords
        [ prettyId i
        , Data.Text.pack $ show val
        ]

prettyHelp
  :: MetricId
  -> Text
prettyHelp MetricId{..} =
  Data.Text.unwords
    [ "# HELP"
    , metricIdName
    , metricIdHelp
    ]

prettyType
  :: MetricId
  -> Metric
  -> Text
prettyType mId x =
  Data.Text.unwords
  [ "# TYPE"
  , metricIdName mId
  , toTypeStr x
  ]

toTypeStr
  :: IsString p
  => Metric
  -> p
toTypeStr (Counter _)   = "counter"
toTypeStr (Gauge _)     = "gauge"
toTypeStr (Summary{})   = "summary"
toTypeStr (Histogram{}) = "histogram"

prettyId
  :: MetricId
  -> Text
prettyId MetricId{..} =
  mconcat
    [ metricIdName
    , prettyLabels metricIdLabels
    ]

prettyLabels
  :: Map Text Text
  -> Text
prettyLabels labels | Data.Map.null labels = mempty
prettyLabels labels | otherwise =
  mconcat
    [ "{"
    , Data.Text.intercalate ","
        $ Data.Map.elems
        $ Data.Map.mapWithKey
            (\k v -> mconcat [k, "=\"", v, "\""]) 
            labels
    , "}"
    ]
