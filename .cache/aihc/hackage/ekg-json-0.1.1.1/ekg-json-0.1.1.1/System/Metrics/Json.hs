{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Encoding of ekg metrics as JSON. The encoding defined by the
-- functions in this module are standardized and used by the ekg web
-- UI. The purpose of this module is to let other web servers and
-- frameworks than the one used by the ekg package expose ekg metrics.
module System.Metrics.Json
    ( -- * Converting metrics to JSON values
      sampleToJson
    , valueToJson

      -- ** Newtype wrappers with instances
    , Sample(..)
    , Value(..)
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

type KeyMap = KM.KeyMap

insertMap :: T.Text -> v -> KeyMap v -> KeyMap v
insertMap = KM.insert . K.fromText

lookupMap :: T.Text -> KeyMap v -> Maybe v
lookupMap = KM.lookup . K.fromText
#else
type KeyMap = M.HashMap T.Text

insertMap :: T.Text -> v -> KeyMap v -> KeyMap v
insertMap = M.insert

lookupMap :: T.Text -> KeyMap v -> Maybe v
lookupMap = M.lookup
#endif

------------------------------------------------------------------------
-- * Converting metrics to JSON values


-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name introduces a new level of nesting. For example, the metrics
-- @[("foo.bar", 10), ("foo.baz", "label")]@ are encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": {
-- >       "type:", "c",
-- >       "val": 10
-- >     },
-- >     "baz": {
-- >       "type": "l",
-- >       "val": "label"
-- >     }
-- >   }
-- > }
--
sampleToJson :: Metrics.Sample -> A.Value
sampleToJson metrics =
    buildOne metrics $ A.emptyObject
  where
    buildOne :: M.HashMap T.Text Metrics.Value -> A.Value -> A.Value
    buildOne m o = M.foldlWithKey' build o m

    build :: A.Value -> T.Text -> Metrics.Value -> A.Value
    build m name val = go m (T.splitOn "." name) val

    go :: A.Value -> [T.Text] -> Metrics.Value -> A.Value
    go (A.Object m) [str] val      = A.Object $ insertMap str metric m
      where metric = valueToJson val
    go (A.Object m) (str:rest) val = case lookupMap str m of
        Nothing -> A.Object $ insertMap str (go A.emptyObject rest val) m
        Just m' -> A.Object $ insertMap str (go m' rest val) m
    go v _ _                        = typeMismatch "Object" v

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
    " instead"
  where
    name = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"

-- | Encodes a single metric as a JSON object. Example:
--
-- > {
-- >   "type": "c",
-- >   "val": 89460
-- > }
--
valueToJson :: Metrics.Value -> A.Value
valueToJson (Metrics.Counter n)      = scalarToJson n CounterType
valueToJson (Metrics.Gauge n)        = scalarToJson n GaugeType
valueToJson (Metrics.Label l)        = scalarToJson l LabelType
valueToJson (Metrics.Distribution l) = distrubtionToJson l

-- | Convert a scalar metric (i.e. counter, gauge, or label) to a JSON
-- value.
scalarToJson :: A.ToJSON a => a -> MetricType -> A.Value
scalarToJson val ty = A.object
    ["val" .= val, "type" .= metricType ty]
{-# SPECIALIZE scalarToJson :: Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE scalarToJson :: T.Text -> MetricType -> A.Value #-}

data MetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType      = "c"
metricType GaugeType        = "g"
metricType LabelType        = "l"
metricType DistributionType = "d"

-- | Convert a distribution to a JSON value.
distrubtionToJson :: Distribution.Stats -> A.Value
distrubtionToJson stats = A.object
    [ "mean" .= Distribution.mean stats
    , "variance" .= Distribution.variance stats
    , "count" .= Distribution.count stats
    , "sum" .= Distribution.sum stats
    , "min" .= Distribution.min stats
    , "max" .= Distribution.max stats
    , "type" .= metricType DistributionType
    ]

------------------------------------------------------------------------
-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
    deriving Show

-- | Uses 'sampleToJson'.
instance A.ToJSON Sample where
    toJSON (Sample s) = sampleToJson s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
    deriving Show

-- | Uses 'valueToJson'.
instance A.ToJSON Value where
    toJSON (Value v) = valueToJson v
