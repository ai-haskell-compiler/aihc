{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.System.Time
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @old-time@ library.

/Since: 2/
-}
module TextShow.System.Time () where

import System.IO.Unsafe (unsafePerformIO)
import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day,
                    calendarTimeToString, toCalendarTime)

import TextShow (TextShow(..), fromString)
import TextShow.TH (deriveTextShow)

-- | /Since: 2/
instance TextShow ClockTime where
    showb = fromString . calendarTimeToString . unsafePerformIO . toCalendarTime

-- | /Since: 2/
$(deriveTextShow ''TimeDiff)
-- | /Since: 2/
$(deriveTextShow ''Month)
-- | /Since: 2/
$(deriveTextShow ''Day)
-- | /Since: 2/
$(deriveTextShow ''CalendarTime)
