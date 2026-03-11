--------------------------------------------------------------------------------
-- |
-- Module : Data.DotNet.TimeSpan
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- .NET TimeSpan implemented in Haskell.
--------------------------------------------------------------------------------
module Data.DotNet.TimeSpan
    ( TimeSpan
    , timeSpanTicks
    , timeSpanHoursMinsSecs
    , timeSpanDaysHoursMinsSecs
    , timeSpanDaysHoursMinsSecsMillis
    , ticks
    , days
    , hours
    , minutes
    , seconds
    , millis
    , fromSeconds
    , fromMinutes
    , fromHours
    , fromDays
    , totalDays
    , totalHours
    , totalMinutes
    , totalSeconds
    , totalMillis
    ) where

--------------------------------------------------------------------------------
import Data.DotNet.TimeSpan.Internal
