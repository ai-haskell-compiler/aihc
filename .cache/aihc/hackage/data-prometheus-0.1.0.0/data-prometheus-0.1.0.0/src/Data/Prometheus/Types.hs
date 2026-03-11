module Data.Prometheus.Types
  ( MetricId(..)
  , Metric(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)

data MetricId = MetricId
  { metricIdName :: Text
  , metricIdHelp :: Text
  , metricIdLabels :: Map Text Text }
  deriving (Eq, Ord, Show)

data Metric
  = Counter Double
  | Gauge Double
  | Summary
    { sumQuantiles :: Map Double Double
    , sumSum       :: Double
    , sumCount     :: Double
    }
  | Histogram
    { histBuckets :: Map Double Double
    , histSum     :: Double
    , histCount   :: Double
    }
  deriving (Eq, Ord, Show)
