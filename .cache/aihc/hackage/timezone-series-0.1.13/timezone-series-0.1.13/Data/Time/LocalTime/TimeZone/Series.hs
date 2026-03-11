{-# LANGUAGE CPP #-}

-- | A @TimeZoneSeries@ describes a timezone by specifying the various
-- clock settings that occurred in the past and are scheduled to occur
-- in the future for the timezone.

module Data.Time.LocalTime.TimeZone.Series
(
  -- * Representing a timezone
  -- $abouttzs
  TimeZoneSeries(..),
  timeZoneFromSeries,
  isValidLocalTime,
  isRedundantLocalTime,
  latestNonSummer,

  -- ** Converting between UTC and local time
  -- $aboutfuncs
  utcToLocalTime',
  localTimeToUTC',

  -- * Representing a moment in a timezone
  ZoneSeriesTime(..),
  zonedTimeToZoneSeriesTime,
  zoneSeriesTimeToLocalTime,
  zoneSeriesTimeZone,
  localTimeToZoneSeriesTime
)
where

import Data.Time (UTCTime, LocalTime, TimeZone(timeZoneSummerOnly),
                  ZonedTime(ZonedTime),
                  utcToLocalTime, localTimeToUTC)
#if MIN_VERSION_time(1,9,1)
import Data.Time.Format.Internal
  ( FormatTime(formatCharacter)
  , ParseTime( buildTime
             , parseTimeSpecifier
             , substituteTimeSpecifier
             )
  )
#else
import Data.Time (FormatTime(formatCharacter), ParseTime(buildTime))
#endif
import Data.List (partition)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))

-- $abouttzs
-- A @TimeZoneSeries@ describes a timezone with a set of 'TimeZone'
-- objects. Each @TimeZone@ object describes the clock setting in the
-- timezone for a specific period of history during which the clocks
-- do not change.
--
-- Most operating systems provide information about timezone series
-- for the local timezone and for many other timezones of the world.
-- On MS Windows systems, this information can be read from the
-- registry. On other systems, this information is typically provided
-- in the form of Olson timezone files: \/etc\/localtime (or some
-- other file) for the local timezone, and files located in
-- \/usr\/share\/zoneinfo\/ or \/etc\/zoneinfo\/ (or some other
-- directory) for other timezones.

-- | A @TimeZoneSeries@ consists of a default @TimeZone@ object and a
-- sequence of pairs of a @UTCTime@ and a @TimeZone@ object. Each
-- @UTCTime@ indicates a moment at which the clocks changed, and the
-- corresponding @TimeZone@ object describes the new state of the
-- clocks after the change. The default @TimeZone@ object is used for
-- times preceding the earliest @UTCTime@, or if the sequence of pairs
-- is empty. The times in the sequence are in order from latest to
-- earlist (note that this is the opposite of the way that they are
-- stored in an Olson timezone file).
data TimeZoneSeries =
       TimeZoneSeries {
         tzsTimeZone ::    TimeZone,
                             -- ^ The default timezone state
         tzsTransitions :: [(UTCTime, TimeZone)]
                             -- ^ A list of pairs of the time of a
                             -- change of clocks and the new timezone
                             -- state after the change
    }
  deriving (Eq, Ord, Typeable)

instance Show TimeZoneSeries where
  show = show . latestNonSummer

instance Read TimeZoneSeries where
    readsPrec n = map (first $ flip TimeZoneSeries []) . readsPrec n

instance NFData TimeZoneSeries where
  rnf tzs = rnf (tzsTimeZone tzs, tzsTransitions tzs)

instance ParseTime TimeZoneSeries where
  buildTime locale = mapBuiltTime (flip TimeZoneSeries []) . buildTime locale
#if MIN_VERSION_time(1,9,1)
  parseTimeSpecifier _ = parseTimeSpecifier (Proxy :: Proxy TimeZone)
  substituteTimeSpecifier _ =
    substituteTimeSpecifier (Proxy :: Proxy TimeZone)
#endif

#if MIN_VERSION_time(1,6,0)
mapBuiltTime :: Functor f => (a -> b) -> f a -> f b
mapBuiltTime = fmap
#else
mapBuiltTime :: a -> a
mapBuiltTime = id
#endif

-- | The latest non-summer @TimeZone@ in a @TimeZoneSeries@ is in some
-- sense representative of the timezone.
latestNonSummer :: TimeZoneSeries -> TimeZone
latestNonSummer (TimeZoneSeries d cs) = fromMaybe d . listToMaybe $
    filter (not . timeZoneSummerOnly) tzs ++ tzs
  where
    tzs = map snd cs

instance FormatTime TimeZoneSeries where
  formatCharacter =
    fmap (mapFormatCharacter latestNonSummer) .
    formatCharacter

#if MIN_VERSION_time(1,9,1)
mapFormatCharacter ::
  (a -> b) -> Maybe (x -> b -> String) -> Maybe (x -> a -> String)
mapFormatCharacter f = fmap (\g x -> g x . f)
#elif MIN_VERSION_time(1,8,0)
mapFormatCharacter ::
  (a -> b) -> (c -> d -> e -> b -> z) -> c -> d -> e -> a -> z
mapFormatCharacter f g locale mpado mwidth = g locale mpado mwidth . f
#else
mapFormatCharacter :: (a -> b) -> (c -> d -> b -> z) -> c -> d -> a -> z
mapFormatCharacter f g locale mpado = g locale mpado . f
#endif

-- | Given a timezone represented by a @TimeZoneSeries@, and a @UTCTime@,
-- provide the state of the timezone's clocks at that time.
timeZoneFromSeries :: TimeZoneSeries -> UTCTime -> TimeZone
timeZoneFromSeries (TimeZoneSeries dfault changes) t = fromMaybe dfault .
  fmap snd . listToMaybe . dropWhile ((> t) . fst) $ changes

-- The following functions attempt to deal correctly with corner cases
-- where multiple clock changes overlap with each other, even though
-- as far as I know such weird cases have never actually occurred in
-- any timezone. So far.
--
-- The theorem is that if G is the set of individual time changes
-- for which t is invalid ("in the Gap"), and O is the set of
-- individual time changes for which t is redundant ("in the Overlap"),
-- then t is invalid for the full set of changes iff #G > #O, and
-- t is redundant for the full set of changes iff #G < #O.
-- A proof for this theorem, supplied by Aristid Breitkreuz,
-- is available at:
-- http://projects.haskell.org/time-ng/gaps_and_overlaps.html

-- | When a clock change moves the clock forward, local times that
-- are between the wall clock time before the change and the wall
-- clock time after the change cannot occur.
isValidLocalTime :: TimeZoneSeries -> LocalTime -> Bool
isValidLocalTime tzs lt = gapsVsOverlaps tzs lt /= GT

-- | When a clock change moves the clock backward, local times that
-- are between the wall clock time before the change and the wall
-- clock time after the change occur twice.
isRedundantLocalTime :: TimeZoneSeries -> LocalTime -> Bool
isRedundantLocalTime tzs lt = gapsVsOverlaps tzs lt == LT

-- Compare the number of gaps to the number of overlaps in the
-- timezone series for the given local time, as described above.
gapsVsOverlaps :: TimeZoneSeries -> LocalTime -> Ordering
gapsVsOverlaps tzs@(TimeZoneSeries d cs) lt = compareLengths gaps overlaps
  where
    (gaps, overlaps) = partition (uncurry (<)) relevantIntervals
    relevantIntervals = takeWhile notTooEarly . dropWhile tooLate $
                        gapsAndOverlaps tzs
    tooLate (a, b) = a > lt && b > lt
    notTooEarly (a, b) = a > lt || b > lt

-- Each pair (ltBefore, ltAfter) represents a time change
-- where the clock is moved from ltBefore to ltAfter. If
-- ltBefore < ltAfter the clock moves forward for that clock
-- change, and otherwise backward.
gapsAndOverlaps :: TimeZoneSeries -> [(LocalTime, LocalTime)]
gapsAndOverlaps (TimeZoneSeries d cs) = zip
      (zipWith utcToLocalTime (map snd (drop 1 cs) ++ [d]) (map fst cs))
      (map (uncurry $ flip utcToLocalTime) cs)

-- Fast lazy comparison of list lengths
compareLengths :: [a] -> [a] -> Ordering
compareLengths (_:xs) (_:ys) = compareLengths xs ys
compareLengths (_:_ ) _      = GT
compareLengths _      (_:_ ) = LT
compareLengths _      _      = EQ

-- | A @ZoneSeriesTime@ represents a moment of time in the context of
-- a particular timezone.
data ZoneSeriesTime = ZoneSeriesTime {
       zoneSeriesTimeToUTC :: UTCTime,
       zoneSeriesTimeSeries :: TimeZoneSeries
    }
  deriving (Eq, Ord, Typeable)

instance Show ZoneSeriesTime where
  show tzs = show $ ZonedTime (zoneSeriesTimeToLocalTime tzs)
                              (zoneSeriesTimeZone tzs)

instance Read ZoneSeriesTime where
    readsPrec n = map (first zonedTimeToZoneSeriesTime) . readsPrec n

instance ParseTime ZoneSeriesTime where
  buildTime locale = mapBuiltTime zonedTimeToZoneSeriesTime . buildTime locale
#if MIN_VERSION_time(1,9,1)
  parseTimeSpecifier _ = parseTimeSpecifier (Proxy :: Proxy ZonedTime)
  substituteTimeSpecifier _ =
    substituteTimeSpecifier (Proxy :: Proxy ZonedTime)
#endif

instance FormatTime ZoneSeriesTime where
  formatCharacter =
    fmap (mapFormatCharacter zoneSeriesTimeToZonedTime) .
    formatCharacter

-- | The @ZonedTime@ of a @ZoneSeriesTime@.
zoneSeriesTimeToZonedTime :: ZoneSeriesTime -> ZonedTime
zoneSeriesTimeToZonedTime zst =
  ZonedTime (zoneSeriesTimeToLocalTime zst) (zoneSeriesTimeZone zst)

-- | Use a trivial @TimeZoneSeries@ containing only the @TimeZone@
-- of the @ZonedTime@, and use it to define a @ZoneSeriesTime@.
zonedTimeToZoneSeriesTime :: ZonedTime -> ZoneSeriesTime
zonedTimeToZoneSeriesTime (ZonedTime t tz) =
  ZoneSeriesTime (localTimeToUTC tz t) (TimeZoneSeries tz [])

-- | The local time represented by a @ZoneSeriesTime@
zoneSeriesTimeToLocalTime :: ZoneSeriesTime -> LocalTime
zoneSeriesTimeToLocalTime (ZoneSeriesTime t tzs) = utcToLocalTime' tzs t

-- | The @TimeZone@ that is in effect at the moment represented by
-- a @ZoneSeriesTime@.
zoneSeriesTimeZone :: ZoneSeriesTime -> TimeZone
zoneSeriesTimeZone (ZoneSeriesTime t tzs) = timeZoneFromSeries tzs t

-- | The @ZoneSeriesTime@ that represents the given local time in the
-- given timezone. Local times that are invalid or redundant are treated
-- as described below.
localTimeToZoneSeriesTime :: TimeZoneSeries -> LocalTime -> ZoneSeriesTime
localTimeToZoneSeriesTime tzs lt = ZoneSeriesTime (localTimeToUTC' tzs lt) tzs

-- $aboutfuncs
-- The following functions are variants on functions in
-- "Data.Time.LocalTime" that convert between UTC and local time. The
-- originals can give a wrong result if the 'TimeZone' used for the
-- conversion is not actually in effect at the specified time. These
-- variants use a 'TimeZoneSeries' instead of a 'TimeZone'.
--
-- When converting from an invalid local time, the local time is
-- interpreted as if the time change that made it invalid never
-- happened.  When converting from a redundant local time, the latest
-- possible interpretation is used. Use the functions
-- 'isValidLocalTime' and 'isRedundantLocalTime' to detect these
-- conditions.

-- | Convert a UTC time to local time using the "TimeZone" that is in
-- effect at that time in the timezone represented by TimeZoneSeries.
utcToLocalTime' :: TimeZoneSeries -> UTCTime -> LocalTime
utcToLocalTime' tzs utc = utcToLocalTime (timeZoneFromSeries tzs utc) utc

-- | Convert a local time to UTC using the "TimeZone" that is in
-- effect at that time in the timezone represented by TimeZoneSeries.
-- Local times that are invalid or redundant are treated as described above.
localTimeToUTC' :: TimeZoneSeries -> LocalTime -> UTCTime
localTimeToUTC' (TimeZoneSeries dfault changes) lt =
  fromMaybe (localTimeToUTC dfault lt) . fmap snd . listToMaybe .
  dropWhile (uncurry (>)) $
  zip (map fst changes) (map (flip localTimeToUTC lt . snd) changes)
