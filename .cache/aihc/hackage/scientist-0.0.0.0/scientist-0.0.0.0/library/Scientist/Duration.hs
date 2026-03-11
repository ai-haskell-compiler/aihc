{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scientist.Duration
  ( Duration
  , measureDuration
  , durationToSeconds
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Data.Fixed (Fixed(..), Nano, showFixed)
import qualified System.Clock as Clock

-- | Time elapsed in seconds up to nanosecond precision
--
-- >> 1.005
-- 1.005s
newtype Duration = Duration
  { _unDuration :: Nano
  }
  deriving stock (Eq)
  deriving newtype (Enum, Ord, Num, Real, Fractional, RealFrac)

instance Show Duration where
  show (Duration x) = showFixed True x <> "s"

measureDuration :: MonadIO m => m a -> m (a, Duration)
measureDuration f = do
  begin <- liftIO getTime
  (,) <$> f <*> liftIO
    (fromNanoSecs . Clock.toNanoSecs . subtract begin <$> getTime)
  where getTime = Clock.getTime Clock.Monotonic

-- | Convert from duration to seconds
--
-- >> toSecs 0.000001
-- 0.000001
durationToSeconds :: Duration -> Double
durationToSeconds = realToFrac

-- | Convert to duration from nanoseconds
--
-- >> fromNanoSecs 1000
-- 0.000001s
fromNanoSecs :: Integer -> Duration
fromNanoSecs = Duration . MkFixed
