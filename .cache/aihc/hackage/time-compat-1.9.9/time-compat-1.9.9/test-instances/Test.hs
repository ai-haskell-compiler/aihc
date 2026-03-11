{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import Control.DeepSeq (NFData (rnf), force)
import Data.Hashable (Hashable)
import Data.Data (Data)
import Data.Kind (Type, Constraint)
import GHC.Generics (Generic)
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Semi
import qualified Language.Haskell.TH.Syntax as TH

import Data.Time.Calendar.Compat
import Data.Time.Calendar.Month.Compat
import Data.Time.Calendar.Quarter.Compat
import Data.Time.Calendar.WeekDate.Compat
import Data.Time.Clock.System.Compat
import Data.Time.Clock.TAI.Compat
import Data.Time.Compat
import Data.Time.Format.Compat
import Data.Time.Format.ISO8601.Compat
import Test.HUnit.Base               ((@?=))

main :: IO ()
main = do
    utc <- getCurrentTime

    -- UTCTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force utc)

    -- ZonedTime
    zt <- getZonedTime
    putStrLn $ formatTime defaultTimeLocale rfc822DateFormat (force zt)

    -- SystemTime
    st <- getSystemTime
    print $ force st

    -- FormatTime DayOfWeek
    formatTime defaultTimeLocale "%u %w %a %A" Monday @?= "1 1 Mon Monday"

    -- TAI taiNominalDayStart
    show (taiNominalDayStart (ModifiedJulianDay 123)) @?= "1859-03-20 00:00:00 TAI"

-------------------------------------------------------------------------------
-- per type instances
-------------------------------------------------------------------------------

_instances :: [()]
_instances =
    [ inst (P @TimeZone) (P @Data)
    , inst (P @TimeZone) (P @Generic)
    , inst (P @TimeZone) (P @Read)
    , inst (P @TimeZone) (P @Show)
    , inst (P @TimeZone) (P @NFData)
    , inst (P @TimeZone) (P @Eq)
    , inst (P @TimeZone) (P @Ord)
    , inst (P @TimeZone) (P @FormatTime)
    , inst (P @TimeZone) (P @ISO8601)
    , inst (P @TimeZone) (P @ParseTime)
    , inst (P @TimeZone) (P @TH.Lift)

    , inst (P @TimeOfDay) (P @Data)
    , inst (P @TimeOfDay) (P @Generic)
    , inst (P @TimeOfDay) (P @Read)
    , inst (P @TimeOfDay) (P @Show)
    , inst (P @TimeOfDay) (P @NFData)
    , inst (P @TimeOfDay) (P @Eq)
    , inst (P @TimeOfDay) (P @Ord)
    , inst (P @TimeOfDay) (P @FormatTime)
    , inst (P @TimeOfDay) (P @ISO8601)
    , inst (P @TimeOfDay) (P @ParseTime)
    , inst (P @TimeOfDay) (P @TH.Lift)

    , inst (P @CalendarDiffTime) (P @Data)
    , inst (P @CalendarDiffTime) (P @Mon.Monoid)
    , inst (P @CalendarDiffTime) (P @Semi.Semigroup)
    , inst (P @CalendarDiffTime) (P @Generic)
    -- , inst (P @CalendarDiffTime) (P @Read)
    , inst (P @CalendarDiffTime) (P @Show)
    , inst (P @CalendarDiffTime) (P @NFData)
    , inst (P @CalendarDiffTime) (P @Eq)
    -- , inst (P @CalendarDiffTime) (P @FormatTime)
    , inst (P @CalendarDiffTime) (P @ISO8601)
    -- , inst (P @CalendarDiffTime) (P @ParseTime)
    , inst (P @CalendarDiffTime) (P @TH.Lift)

    , inst (P @LocalTime) (P @Data)
    , inst (P @LocalTime) (P @Generic)
    , inst (P @LocalTime) (P @Read)
    , inst (P @LocalTime) (P @Show)
    , inst (P @LocalTime) (P @NFData)
    , inst (P @LocalTime) (P @Eq)
    , inst (P @LocalTime) (P @Ord)
    , inst (P @LocalTime) (P @FormatTime)
    , inst (P @LocalTime) (P @ISO8601)
    , inst (P @LocalTime) (P @ParseTime)
    , inst (P @LocalTime) (P @TH.Lift)

    , inst (P @ZonedTime) (P @Data)
    , inst (P @ZonedTime) (P @Generic)
    , inst (P @ZonedTime) (P @Read)
    , inst (P @ZonedTime) (P @Show)
    , inst (P @ZonedTime) (P @NFData)
    , inst (P @ZonedTime) (P @FormatTime)
    , inst (P @ZonedTime) (P @ISO8601)
    , inst (P @ZonedTime) (P @ParseTime)
    , inst (P @ZonedTime) (P @TH.Lift)

    , inst (P @UniversalTime) (P @Data)
    , inst (P @UniversalTime) (P @Generic)
    , inst (P @UniversalTime) (P @Read)
    , inst (P @UniversalTime) (P @Show)
    , inst (P @UniversalTime) (P @NFData)
    , inst (P @UniversalTime) (P @Eq)
    , inst (P @UniversalTime) (P @Ord)
    , inst (P @UniversalTime) (P @FormatTime)
    , inst (P @UniversalTime) (P @ParseTime)
    , inst (P @UniversalTime) (P @TH.Lift)

    , inst (P @DiffTime) (P @Data)
    , inst (P @DiffTime) (P @Read)
    , inst (P @DiffTime) (P @Show)
    , inst (P @DiffTime) (P @NFData)
    , inst (P @DiffTime) (P @Eq)
    , inst (P @DiffTime) (P @Ord)
    -- , inst (P @DiffTime) (P @FormatTime)
    -- , inst (P @DiffTime) (P @ParseTime)
    , inst (P @DiffTime) (P @TH.Lift)

    , inst (P @UTCTime) (P @Data)
    , inst (P @UTCTime) (P @Generic)
    , inst (P @UTCTime) (P @Read)
    , inst (P @UTCTime) (P @Show)
    , inst (P @UTCTime) (P @NFData)
    , inst (P @UTCTime) (P @Eq)
    , inst (P @UTCTime) (P @Ord)
    , inst (P @UTCTime) (P @FormatTime)
    , inst (P @UTCTime) (P @ISO8601)
    , inst (P @UTCTime) (P @ParseTime)
    , inst (P @UTCTime) (P @TH.Lift)

    , inst (P @NominalDiffTime) (P @Data)
    -- , inst (P @NominalDiffTime) (P @Read)
    , inst (P @NominalDiffTime) (P @Show)
    , inst (P @NominalDiffTime) (P @NFData)
    , inst (P @NominalDiffTime) (P @Eq)
    , inst (P @NominalDiffTime) (P @Ord)
    -- , inst (P @NominalDiffTime) (P @FormatTime)
    -- , inst (P @NominalDiffTime) (P @ISO8601)
    -- , inst (P @NominalDiffTime) (P @ParseTime)
    , inst (P @NominalDiffTime) (P @TH.Lift)

    , inst (P @Day) (P @Data)
    , inst (P @Day) (P @Generic)
    , inst (P @Day) (P @Read)
    , inst (P @Day) (P @Show)
    , inst (P @Day) (P @NFData)
    , inst (P @Day) (P @Eq)
    , inst (P @Day) (P @Ord)
    , inst (P @Day) (P @FormatTime)
    , inst (P @Day) (P @ISO8601)
    , inst (P @Day) (P @ParseTime)
    , inst (P @Day) (P @TH.Lift)

    , inst (P @CalendarDiffDays) (P @Data)
    , inst (P @CalendarDiffDays) (P @Mon.Monoid)
    , inst (P @CalendarDiffDays) (P @Semi.Semigroup)
    , inst (P @CalendarDiffDays) (P @Generic)
    -- , inst (P @CalendarDiffDays) (P @Read)
    , inst (P @CalendarDiffDays) (P @Show)
    , inst (P @CalendarDiffDays) (P @NFData)
    , inst (P @CalendarDiffDays) (P @Eq)
    -- , inst (P @CalendarDiffDays) (P @FormatTime)
    , inst (P @CalendarDiffDays) (P @ISO8601)
    -- , inst (P @CalendarDiffDays) (P @ParseTime)
    , inst (P @CalendarDiffDays) (P @TH.Lift)

    , inst (P @DayOfWeek) (P @Data)
    , inst (P @DayOfWeek) (P @Read)
    , inst (P @DayOfWeek) (P @Show)
    , inst (P @DayOfWeek) (P @NFData)
    , inst (P @DayOfWeek) (P @Eq)
    , inst (P @DayOfWeek) (P @FormatTime)
    , inst (P @DayOfWeek) (P @TH.Lift)

    , inst (P @FirstWeekType) (P @Eq)
    , inst (P @FirstWeekType) (P @TH.Lift)

    , inst (P @Month) (P @Data)
    , inst (P @Month) (P @Read)
    , inst (P @Month) (P @Show)
    , inst (P @Month) (P @NFData)
    , inst (P @Month) (P @Eq)
    , inst (P @Month) (P @Ord)
    , inst (P @Month) (P @FormatTime)
    -- , inst (P @Month) (P @ParseTime)
    , inst (P @Month) (P @TH.Lift)

    , inst (P @Quarter) (P @Data)
    , inst (P @Quarter) (P @Read)
    , inst (P @Quarter) (P @Show)
    , inst (P @Quarter) (P @NFData)
    , inst (P @Quarter) (P @Eq)
    , inst (P @Quarter) (P @Ord)
    , inst (P @Quarter) (P @Generic)
    , inst (P @Quarter) (P @TH.Lift)

    , inst (P @QuarterOfYear) (P @Data)
    , inst (P @QuarterOfYear) (P @Read)
    , inst (P @QuarterOfYear) (P @Show)
    , inst (P @QuarterOfYear) (P @NFData)
    , inst (P @QuarterOfYear) (P @Eq)
    , inst (P @QuarterOfYear) (P @Ord)
    , inst (P @QuarterOfYear) (P @Eq)
    , inst (P @QuarterOfYear) (P @TH.Lift)

    , inst (P @Day)     (P @DayPeriod)
    , inst (P @Month)   (P @DayPeriod)
    , inst (P @Quarter) (P @DayPeriod)
    , inst (P @Year)    (P @DayPeriod)
    ]

data P a = P

inst :: c a => P a -> P c -> ()
inst _ _ = ()

-------------------------------------------------------------------------------
-- Old tests
-------------------------------------------------------------------------------

_ParseTimeInstances :: [()]
_ParseTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    ]
  where
    test :: ParseTime t => t -> ()
    test _ = ()

_FormatTimeInstances :: [()]
_FormatTimeInstances =
    [ () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , () -- test (undefined :: DiffTime)
    , () -- test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , () -- test (undefined :: CalendarDiffTime)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Month)
    ]
  where
    test :: FormatTime t => t -> ()
    test _ = ()

_NFDataInstances :: [()]
_NFDataInstances =
    [ test (undefined :: CalendarDiffTime)
    , test (undefined :: Day)
    , test (undefined :: DiffTime)
    , test (undefined :: NominalDiffTime)
    , test (undefined :: UTCTime)
    , test (undefined :: UniversalTime)
    , test (undefined :: CalendarDiffTime)
    , test (undefined :: CalendarDiffDays)
    , test (undefined :: TimeZone)
    , test (undefined :: TimeOfDay)
    , test (undefined :: LocalTime)
    , test (undefined :: ZonedTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Month)
    , test (undefined :: Quarter)
    , test (undefined :: QuarterOfYear)
    ]
  where
    test :: NFData t => t -> ()
    test = rnf

_EnumInstances :: [()]
_EnumInstances =
    [ test (undefined :: Day)
    , test (undefined :: Month)
    , test (undefined :: Quarter)
    , test (undefined :: QuarterOfYear)
    ]
  where
    test :: Enum t => t -> ()
    test _ = ()

_HashableInstances :: [()]
_HashableInstances =
    [ test (undefined :: TimeLocale)
    , test (undefined :: LocalTime)
    , test (undefined :: TimeOfDay)
    , test (undefined :: TimeZone)
    , test (undefined :: UniversalTime)
    , test (undefined :: UTCTime)
    , test (undefined :: NominalDiffTime)
    , test (undefined :: DiffTime)
    , test (undefined :: DayOfWeek)
    , test (undefined :: Day)
    , test (undefined :: QuarterOfYear)
    , test (undefined :: Quarter)
    , test (undefined :: Month)
    ]
  where
    test :: Hashable t => t -> ()
    test _ = ()
