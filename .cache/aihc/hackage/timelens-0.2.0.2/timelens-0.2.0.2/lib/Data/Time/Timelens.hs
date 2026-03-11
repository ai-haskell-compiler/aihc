-- | Lenses for the @time@ package.
--
-- When creating an optic (such as a 'L.Lens', an 'L.Iso', etc.), this
-- module uses the same name that the @lens@ package uses.  This makes
-- it very easy for you to predict what name to use when you are
-- looking for the lens.  However, it also means that you will
-- probably want to @import@ this module @qualified@.
module Data.Time.Timelens where

import qualified Control.Lens as L
import Data.Fixed (Pico)
import qualified Data.Time as T

-- * "Data.Time.Calendar"

-- ** 'T.Day'

toModifiedJulianDay :: L.Iso' T.Day Integer
toModifiedJulianDay = L.iso T.toModifiedJulianDay T.ModifiedJulianDay

-- * "Data.Time.Clock"

-- ** 'T.UniversalTime'

getModJulianDate :: L.Iso' T.UniversalTime Rational
getModJulianDate = L.iso T.getModJulianDate T.ModJulianDate

-- ** 'T.UTCTime'

utctDay :: L.Lens' T.UTCTime T.Day
utctDay = L.lens T.utctDay (\utc dy -> utc { T.utctDay = dy })

utctDayTime :: L.Lens' T.UTCTime T.DiffTime
utctDayTime = L.lens T.utctDayTime (\utc dt -> utc { T.utctDayTime = dt })

-- * "Data.Time.Format"

-- ** 'T.TimeLocale'

wDays :: L.Lens' T.TimeLocale [(String, String)]
wDays = L.lens T.wDays (\tl dys -> tl { T.wDays = dys })

months :: L.Lens' T.TimeLocale [(String, String)]
months = L.lens T.months (\tl mts -> tl { T.months = mts })

amPm :: L.Lens' T.TimeLocale (String, String)
amPm = L.lens T.amPm (\tl ap -> tl { T.amPm = ap })

dateTimeFmt :: L.Lens' T.TimeLocale String
dateTimeFmt = L.lens T.dateTimeFmt (\tl s -> tl { T.dateTimeFmt = s })

dateFmt :: L.Lens' T.TimeLocale String
dateFmt = L.lens T.dateFmt (\tl s -> tl { T.dateFmt = s })

timeFmt :: L.Lens' T.TimeLocale String
timeFmt = L.lens T.timeFmt (\tl s -> tl { T.timeFmt = s })

time12Fmt :: L.Lens' T.TimeLocale String
time12Fmt = L.lens T.time12Fmt (\tl s -> tl { T.time12Fmt = s })

knownTimeZones :: L.Lens' T.TimeLocale [T.TimeZone]
knownTimeZones = L.lens T.knownTimeZones (\tl s -> tl { T.knownTimeZones = s })

-- * "Data.Time.LocalTime"

-- ** 'T.TimeZone'

timeZoneMinutes :: L.Lens' T.TimeZone Int
timeZoneMinutes = L.lens T.timeZoneMinutes (\s b -> s { T.timeZoneMinutes = b })

timeZoneSummerOnly :: L.Lens' T.TimeZone Bool
timeZoneSummerOnly = L.lens T.timeZoneSummerOnly
  (\s b -> s { T.timeZoneSummerOnly = b })

timeZoneName :: L.Lens' T.TimeZone String
timeZoneName = L.lens T.timeZoneName (\s b -> s { T.timeZoneName = b })

-- ** 'T.TimeOfDay'

todHour :: L.Lens' T.TimeOfDay Int
todHour = L.lens T.todHour (\s b -> s { T.todHour = b })

todMin :: L.Lens' T.TimeOfDay Int
todMin = L.lens T.todMin (\s b -> s { T.todMin = b })

todSec :: L.Lens' T.TimeOfDay Pico
todSec = L.lens T.todSec (\s b -> s { T.todSec = b })

-- ** 'T.LocalTime'

localDay :: L.Lens' T.LocalTime T.Day
localDay = L.lens T.localDay (\s b -> s { T.localDay = b })

localTimeOfDay :: L.Lens' T.LocalTime T.TimeOfDay
localTimeOfDay = L.lens T.localTimeOfDay (\s b -> s { T.localTimeOfDay = b })

-- ** 'T.ZonedTime'

zonedTimeToLocalTime :: L.Lens' T.ZonedTime T.LocalTime
zonedTimeToLocalTime = L.lens T.zonedTimeToLocalTime
  (\s b -> s { T.zonedTimeToLocalTime = b })

zonedTimeZone :: L.Lens' T.ZonedTime T.TimeZone
zonedTimeZone = L.lens T.zonedTimeZone
  (\s b -> s { T.zonedTimeZone = b })

