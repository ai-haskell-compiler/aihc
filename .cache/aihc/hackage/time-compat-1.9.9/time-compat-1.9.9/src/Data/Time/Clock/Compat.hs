{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Data.Time.Clock.Compat (
    -- * Universal Time
    -- | Time as measured by the Earth.
    UniversalTime(..),

    -- * Absolute intervals, DiffTime
    DiffTime,
    pattern Picoseconds,
    pattern Seconds,
    pattern Minutes,
    pattern Hours,
    secondsToDiffTime,
    picosecondsToDiffTime,
    diffTimeToPicoseconds,

    -- * UTCTime
    UTCTime (..),

    -- * NominalDiffTime
    NominalDiffTime,
    pattern Nominal,
    secondsToNominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay,

    -- * UTC differences
    addUTCTime,
    diffUTCTime,

    -- * Current time
    getCurrentTime,
    getTime_resolution,

    -- * Type aliases
    Year,
    MonthOfYear,
    DayOfMonth,
    ) where

import Data.Time.Orphans ()
import Data.Time.Calendar.Types

import Data.Time.Clock
import Data.Fixed (Pico, Fixed (MkFixed))
import Debug.Trace

#if !MIN_VERSION_time(1,15,0)
pattern Picoseconds :: Integer -> DiffTime
pattern Picoseconds a <- (diffTimeToPicoseconds -> a)
    where
        Picoseconds a = picosecondsToDiffTime a

{-# COMPLETE Picoseconds #-}

pattern Seconds :: Pico -> DiffTime
pattern Seconds a <- (MkFixed . diffTimeToPicoseconds -> a)
  where
        Seconds a = picosecondsToDiffTime (case a of MkFixed a' -> a')

{-# COMPLETE Seconds #-}

pattern Minutes :: Pico -> DiffTime
pattern Minutes a <- Seconds ((/ 60) -> a)
    where
        Minutes a = Seconds $ a * 60

{-# COMPLETE Minutes #-}

pattern Hours :: Pico -> DiffTime
pattern Hours a <- Minutes ((/ 60) -> a)
    where
        Hours a = Minutes $ a * 60

{-# COMPLETE Hours #-}

-- | convert from DiffTime
pattern Nominal :: DiffTime -> NominalDiffTime
pattern Nominal dt <- (Seconds . nominalDiffTimeToSeconds -> dt)
    where
        Nominal dt = secondsToNominalDiffTime (case dt of Seconds dt' -> dt')

{-# COMPLETE Nominal #-}

#endif

#if !MIN_VERSION_time(1,9,1)

-- | Create a 'NominalDiffTime' from a number of seconds.
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

-- | Get the seconds in a 'NominalDiffTime'.
nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac

#endif
