-- |
--   Module    : Media.Timestamp
--   License   : MIT
--   Stability : experimental
--
-- A simple timestamp type.
module Media.Timestamp
  ( Time (..),
    Range (..),
  )
where

import Data.Kind (Type)
-- | Timestamp data type.
--
-- @since 0.2.0.0
type Time :: Type
data Time = Time
  { hour        :: Int,
    minute      :: Int,
    second      :: Int,
    millisecond :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | Interval of two timestamps.
--
-- @since 0.1.0.0
type Range :: Type
data Range = Range
  { from :: Time,
    to   :: Time
  }
  deriving stock (Eq, Ord, Show)
