{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.Nagios.Plugin.PerfData
(
    UOM(..),
    PerfValue(..),
    PerfDatum(..),
    ToPerfData,
    toPerfData,
    barePerfDatum
) where

import           Data.Int
import           Data.Text (Text)

import           Numeric

-- | A Nagios "unit of measure". 'NoUOM' translates to an empty
-- string in the check output; it is idiomatic to use it liberally
-- whenever the standard units do not fit.
data UOM =
    Second
  | Millisecond
  | Microsecond
  | Percent
  | Byte
  | Kilobyte
  | Megabyte
  | Gigabyte
  | Terabyte
  | Counter
  | NullUnit
  | UnknownUOM
  deriving (Eq)

instance Show UOM where
    show Second      = "s"
    show Millisecond      = "ms"
    show Microsecond      = "us"
    show Percent     = "%"
    show Byte        = "B"
    show Kilobyte        = "KB"
    show Megabyte        = "MB"
    show Gigabyte        = "GB"
    show Terabyte        = "GB"
    show Counter     = "c"
    show NullUnit    = ""
    show UnknownUOM  = "?"

{-# DEPRECATED UnknownUOM "Will be removed in 0.4.0 in favour of failing on parse." #-}

-- | Value of a performance metric.
data PerfValue = RealValue Double | IntegralValue Int64
  deriving (Eq, Ord)

instance Show PerfValue where
    show (RealValue x) = showFFloat Nothing x ""
    show (IntegralValue x) = show x

-- | One performance metric. A plugin will output zero or more of these,
--   whereupon Nagios generally passes them off to an external system such
--   as <http://oss.oetiker.ch/rrdtool/ RRDTool> or
--   <https://github.com/anchor/vaultaire Vaultaire>.
--   The thresholds are purely informative (designed to be graphed), and
--   do not affect alerting; likewise with `_min` and `_max`.
data PerfDatum = PerfDatum
    { _label :: Text             -- ^ Name of quantity being measured.
    , _value :: PerfValue        -- ^ Measured value, integral or real.
    , _uom   :: UOM              -- ^ Unit of measure; 'NoUOM' is fine here.
    , _min   :: Maybe PerfValue  -- ^ Measured quantity cannot be lower than this.
    , _max   :: Maybe PerfValue  -- ^ Measured quantity cannot be higher than this.
    , _warn  :: Maybe PerfValue  -- ^ Warning threshold for graphing.
    , _crit  :: Maybe PerfValue  -- ^ Critical threshold for graphing.
    }
  deriving (Eq, Show)

class ToPerfData a where
    { toPerfData :: a -> [PerfDatum] }

-- | Create a PerfDatum from only the required values, using Nothing
--   for all the others.
barePerfDatum ::
       Text
    -> PerfValue
    -> UOM
    -> PerfDatum
barePerfDatum info val uom = PerfDatum info val uom Nothing Nothing Nothing Nothing
