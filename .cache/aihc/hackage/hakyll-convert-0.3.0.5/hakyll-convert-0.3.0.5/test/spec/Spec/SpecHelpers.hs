module Spec.SpecHelpers (fromGregorian) where

import qualified Data.Time.Calendar as Calendar
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
fromGregorian year month day hours minutes seconds =
  let utcDay = Calendar.fromGregorian year month day
      utcSeconds = secondsToDiffTime $ fromIntegral $ seconds + 60 * (minutes + 60 * hours)
   in UTCTime utcDay utcSeconds
