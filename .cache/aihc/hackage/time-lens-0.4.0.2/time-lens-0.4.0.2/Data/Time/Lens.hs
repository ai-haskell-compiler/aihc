module Data.Time.Lens
    ( -- * Time
      -- $time
      HasTime(..)
    , hours
    , minutes
    , seconds
      -- * Date
      -- $date
    , HasDate(..)
    , year
    , month
    , day
    , gregorian
      -- * Time zone
      -- $zone
    , HasTimeZone(..)
      -- * Re-exports from "Data.Time"
    , T.Day
    , T.TimeOfDay
    , T.LocalTime
    , T.ZonedTime
    , T.getZonedTime
      -- * Re-exports from "Data.Lens.Light"
    , Lens
    , getL
    , modL
    , setL
    )
where

import Control.Category
import Prelude hiding ((.), id)
import Data.Lens.Light
import Data.Fixed
import qualified Data.Time as T
import Data.Time (TimeOfDay(..), LocalTime(..), fromGregorian)

-- $time
-- The semantics of 'getL' for time lenses ('time','hours','minutes','seconds')
-- is straightforward.
--
-- The semantics of 'setL' is to «normalize» the time before setting. Hence
-- @'modL' 'minutes' (+5)@ will correctly add 5 minutes to the time, e.g.
--
-- >>> modL minutes (+5) (TimeOfDay 16 57 13)
-- 17:02:13
--
-- If this means crossing a day boundary, the semantics varies for different
-- structures. For structures that have a date component (i.e. for instances of
-- 'HasDate') the date is adjusted appropriately.
--
-- >>> modL hours (+10) (LocalTime (fromGregorian 2012 05 23) (TimeOfDay 16 57 13))
-- 2012-05-24 02:57:13
-- >>> modL seconds (subtract 1) (LocalTime (fromGregorian 2012 05 23) (TimeOfDay 0 0 0))
-- 2012-05-22 23:59:59
--
-- If there's no date, the time is simply wrapped around.
--
-- >>> modL seconds (subtract 1) (TimeOfDay 0 0 0)
-- 23:59:59

class HasTime a where
    time :: Lens a T.TimeOfDay

hours :: HasTime a => Lens a Int
hours = (lens T.todHour $ \x t -> t { T.todHour = x }) . time

minutes :: HasTime a => Lens a Int
minutes = (lens T.todMin $ \x t -> t { T.todMin = x }) . time

seconds :: HasTime a => Lens a Pico
seconds = (lens T.todSec $ \x t -> t { T.todSec = x }) . time

instance HasTime T.TimeOfDay where
    time = ntime $ iso id id

localTimeOfZonedTime :: Lens T.ZonedTime T.LocalTime
localTimeOfZonedTime =
    lens T.zonedTimeToLocalTime $
        \x t -> t { T.zonedTimeToLocalTime = x }

instance HasTime T.LocalTime where
    time = ntimeAdjustDay $
        lens T.localTimeOfDay $ \x t -> t { T.localTimeOfDay = x }

instance HasTime T.ZonedTime where
    time = time . localTimeOfZonedTime

instance HasTime T.UTCTime where
    time = time . iso (T.utcToLocalTime T.utc) (T.localTimeToUTC T.utc)

-- $date
-- In contrast to 'time', the 'date' lens is a simple accessor (it doesn't make
-- sense to «normalize» a 'T.Day').
--
-- Instead, setters for 'year', 'month' and 'day' have special semantics
-- described below.
-- Getters are always straightforward.

class HasDate a where
    date :: Lens a T.Day

-- | The semantics of 'gregorian' corresponds to that of 'T.toGregorian' and
-- 'T.fromGregorian'
gregorian :: HasDate a => Lens a (Integer,Int,Int)
gregorian = iso T.toGregorian (uncurry3 T.fromGregorian) . date

-- | @'modL' 'year' (+n)@ adds @n@ years, matching month and day, with Feb 29th
-- rolled over to Mar 1st if necessary (like 'T.addGregorianYearsRollOver')
year :: HasDate a => Lens a Integer
year = lens getYear setYear . date
    where
    getYear date =
        case T.toGregorian date of
            (year,_,_) -> year
    setYear year date =
        case T.toGregorian date of
            (origYear,_,_) -> T.addGregorianYearsRollOver (fromIntegral $ year - origYear) date

-- | @'modL' 'month' (+n)@ adds @n@ months, with days past the last day of the
-- month rolling over to the next month (like 'T.addGregorianMonthsRollOver')
month :: HasDate a => Lens a Int
month = lens getMonth setMonth . date
    where
    getMonth date =
        case T.toGregorian date of
            (_,month,_) -> month
    setMonth month date =
        case T.toGregorian date of
            (_,origMonth,_) -> T.addGregorianMonthsRollOver (fromIntegral $ month - origMonth) date

-- | @'modL' 'day' (+n)@ computes the date @n@ days after the original date
-- (like 'T.addDays')
day :: HasDate a => Lens a Int
day = lens getDay setDay . date
    where
    getDay date =
        case T.toGregorian date of
            (_,_,day) -> day
    setDay day date =
        case T.toGregorian date of
            (_,_,origDay) -> T.addDays (fromIntegral $ day - origDay) date

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

instance HasDate T.Day where
    date = iso id id

instance HasDate T.LocalTime where
    date = lens T.localDay (\d s -> s { T.localDay = d })

instance HasDate T.ZonedTime where
    date = date . localTimeOfZonedTime

instance HasDate T.UTCTime where
    date = date . iso (T.utcToLocalTime T.utc) (T.localTimeToUTC T.utc)

-- $zone
-- Getting 'timeZone' is straightforward. Setting 'TimeZone' changes both
-- 'timeZone' and 'time' (and 'date', if present) in such a way that the new
-- zoned time corresponds to the same UTC time as the original zoned time.

class HasTime a => HasTimeZone a where
    timeZone :: Lens a T.TimeZone

instance HasTimeZone T.ZonedTime where
    timeZone = lens T.zonedTimeZone setTimeZone
        where
        setTimeZone newz (T.ZonedTime oldt oldz) = T.ZonedTime newt newz
            where
                newt = modL minutes (+ tdiff) oldt
                tdiff = T.timeZoneMinutes newz - T.timeZoneMinutes oldz

--
-- Auxiliary functions
--

normalizeTime :: T.TimeOfDay -> (T.TimeOfDay, Integer)
normalizeTime = timeToTimeOfDay . timeOfDayToTime

-- Can't rely on a HasTime instance here because this function will be used to
-- define one
ntime :: Lens a T.TimeOfDay -> Lens a T.TimeOfDay
ntime time = iso id (fst . normalizeTime) . time

ntimeAdjustDay :: (HasDate a) => Lens a T.TimeOfDay -> Lens a T.TimeOfDay
ntimeAdjustDay time = lens (getL time) $ \t ->
    case normalizeTime t of
        (t', days) -> setL time t' . modL date (T.addDays days)

-- We don't use T.timeToTimeOfDay and T.timeOfDayToTime here for the following
-- reasons:
-- * T.timeOfDayToTime could potentially perform bounds checking (although its
-- current implementation doesn't)
-- * T.timeToTimeOfDay converts excess time to leap seconds
timeOfDayToTime :: T.TimeOfDay -> T.DiffTime
timeOfDayToTime (T.TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

timeToTimeOfDay :: T.DiffTime -> (T.TimeOfDay, Integer)
timeToTimeOfDay dt = (T.TimeOfDay (fromInteger h) (fromInteger m) s, d) where
    s' = realToFrac dt
    s = mod' s' 60
    m' = div' s' 60
    m = mod' m' 60
    h' = div' m' 60
    h = mod' h' 24
    d = div' h' 24
