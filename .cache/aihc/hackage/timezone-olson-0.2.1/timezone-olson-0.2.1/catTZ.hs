-- | Parse an Olson timezone file, then render it.
-- If -i is specified, interpret it as a TimeZoneSeries before rendering.
module Main where

import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import System.Environment

main = do
  args <- getArgs
  if head args == "-i"
    then getTimeZoneSeriesFromOlsonFile  (args !! 1) >>=
         renderTimeZoneSeriesToOlsonFile (args !! 2)
    else getOlsonFromFile (args !! 0) >>= renderOlsonToFile (args !! 1)
