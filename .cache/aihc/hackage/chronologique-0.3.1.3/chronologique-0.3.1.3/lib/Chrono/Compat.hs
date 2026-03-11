--
-- Time to manipulate time
--
-- Copyright © 2013-2016 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

--
-- | Compatibility with time types from other time handling libraries. Some of
-- these are just conveniences, but it's not always obvious how to convert
-- between time types even in the same package.
--
module Chrono.Compat
(
  -- * time package, from base
    convertToPosix
  , convertFromPosix
  , convertToUTC
  , convertFromUTC
  -- * hourglass package
  , convertToHourglass
  , convertFromHourglass
) where

import Data.Int (Int64)
import Data.Hourglass
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Chrono.TimeStamp

--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'NominalDiffTime', allowing you to then use the time manipulation
-- functions in "Data.Time.Clock" from __time__.
--
convertToPosix :: TimeStamp -> POSIXTime
convertToPosix = fromRational . (/ 1e9) . fromIntegral

--
--
-- | Utility function to convert nanoseconds since Unix epoch to a
-- 'ElapsedP', allowing you to then use the time manipulation
-- functions in the __hourglass__ package.
--
convertToHourglass :: TimeStamp -> ElapsedP
convertToHourglass = timeGetElapsedP

convertFromHourglass :: ElapsedP -> TimeStamp
convertFromHourglass = timeFromElapsedP


--
-- | Annoyingly, the various types in __time__ don't interoperate. Quite frequently
-- you need to get to, or from, 'UTCTime'.
--
convertToUTC :: TimeStamp -> UTCTime
convertToUTC = posixSecondsToUTCTime . convertToPosix

convertFromUTC :: UTCTime -> TimeStamp
convertFromUTC = convertFromPosix . utcTimeToPOSIXSeconds

convertFromPosix :: POSIXTime -> TimeStamp
convertFromPosix =
  let
    nano :: POSIXTime -> Int64
    nano = floor . (* 1000000000) . toRational
  in
    TimeStamp . fromIntegral . nano
