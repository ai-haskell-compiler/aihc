{-# LANGUAGE CPP                #-}
module Data.Time.Calendar.Month.Compat (
    Month(..), addMonths, diffMonths,
    pattern YearMonth,
    fromYearMonthValid,
    pattern MonthDay,
    fromMonthDayValid,
    -- * time-compat extras
    fromYearMonth,
    toYearMonth,
    fromMonthDay,
    toMonthDay,
) where

#if MIN_VERSION_time(1,15,0)

import Data.Time.Calendar
import Data.Time.Calendar.Month

-- | Part of @YearMonth@ pattern
fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth = YearMonth

-- | Part of @YearMonth@ pattern
toYearMonth :: Month -> (Year, MonthOfYear)
toYearMonth (YearMonth y m) = (y, m)

-- | Part of 'MonthDay' pattern
fromMonthDay :: Month -> DayOfMonth -> Day
fromMonthDay = MonthDay

-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay (MonthDay m d) = (m, d)

#elif MIN_VERSION_time(1,11,0)

import Data.Time.Calendar (Year,MonthOfYear,Day,DayOfMonth)
import Data.Time.Calendar.DayPeriod
import Data.Time.Calendar.Month hiding (MonthDay)

-- | Part of @YearMonth@ pattern
fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth = YearMonth

-- | Part of @YearMonth@ pattern
toYearMonth :: Month -> (Year, MonthOfYear)
toYearMonth (YearMonth y m) = (y, m)

-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay = periodFromDay

-- | Part of 'MonthDay' pattern
fromMonthDay :: Month -> DayOfMonth -> Day
fromMonthDay = periodToDayClip

-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <- (toMonthDay -> (m,dm)) where
    MonthDay = fromMonthDay

{-# COMPLETE MonthDay #-}

#else

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.Internal
#else
import Data.Time.Format
#endif

import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Calendar.Types
import Data.Time.Calendar.Private
import Data.Time.Calendar.DayPeriod
import Data.Data
import Data.Fixed
import Text.Read
import Text.ParserCombinators.ReadP
import Control.DeepSeq (NFData (..))
import Data.Ix (Ix (..))
import Data.Hashable (Hashable (..))
import qualified Language.Haskell.TH.Syntax as TH

-- | An absolute count of common calendar months.
-- Number is equal to @(year * 12) + (monthOfYear - 1)@.
newtype Month = MkMonth Integer deriving (Eq, Ord, Data, Typeable, TH.Lift)

instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Hashable Month where
    hashWithSalt salt (MkMonth x) = hashWithSalt salt x

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

-- | Show as @yyyy-mm@.
instance Show Month where
    show ym = case toYearMonth ym of
        (y, m) -> show4 y ++ "-" ++ show2 m

-- | Read as @yyyy-mm@.
instance Read Month where
    readPrec = do
        y <- readPrec
        _ <- lift $ char '-'
        m <- readPrec
        return $ fromYearMonth y m

-------------------------------------------------------------------------------
-- ForematTime Month
-------------------------------------------------------------------------------

toSomeDay :: Month -> Day
toSomeDay (MkMonth m) =
    let (y,my) = divMod' m 12
    in fromGregorian y (succ (fromInteger my)) 1

#if MIN_VERSION_time(1,9,0)
#define FORMAT_OPTS fo
#else
#define FORMAT_OPTS tl mpo i
#endif

#if MIN_VERSION_time(1,9,0)
#define FORMAT_ARG _arg
#else
#define FORMAT_ARG
#endif

instance FormatTime Month where
    -- Year Count
    formatCharacter FORMAT_ARG 'Y' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'Y')
    formatCharacter FORMAT_ARG 'y' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'y')
    formatCharacter FORMAT_ARG 'c' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'c')
    -- Month of Year
    formatCharacter FORMAT_ARG 'B' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'B')
    formatCharacter FORMAT_ARG 'b' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'b')
    formatCharacter FORMAT_ARG 'h' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'h')
    formatCharacter FORMAT_ARG 'm' = fmap (\f FORMAT_OPTS d -> f FORMAT_OPTS (toSomeDay d)) (formatCharacter FORMAT_ARG 'm')

    formatCharacter FORMAT_ARG _  = Nothing

addMonths :: Integer -> Month -> Month
addMonths n (MkMonth a) = MkMonth $ a + n

diffMonths :: Month -> Month -> Integer
diffMonths (MkMonth a) (MkMonth b) = a - b

fromYearMonthValid :: Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y my = do
    my' <- clipValid 1 12 my
    return $ YearMonth y my'

-- | Part of @YearMonth@ pattern
fromYearMonth :: Year -> MonthOfYear -> Month
fromYearMonth y my = MkMonth $ (y * 12) + toInteger (pred $ clip 1 12 my)

-- | Part of @YearMonth@ pattern
toYearMonth :: Month -> (Year, MonthOfYear)
toYearMonth (MkMonth m) = case divMod' m 12 of
    (y, my) -> (y, succ (fromInteger my))

-- | Bidirectional abstract constructor.
-- Invalid months of year will be clipped to the correct range.
pattern YearMonth :: Year -> MonthOfYear -> Month
pattern YearMonth y my <- (toYearMonth -> (y, my))
  where YearMonth y my = fromYearMonth y my

{-# COMPLETE YearMonth #-}

-- | Part of 'MonthDay' pattern
toMonthDay :: Day -> (Month,DayOfMonth)
toMonthDay = periodFromDay

-- | Part of 'MonthDay' pattern
fromMonthDay :: Month -> DayOfMonth -> Day
fromMonthDay = periodToDayClip

fromMonthDayValid :: Month -> DayOfMonth -> Maybe Day
fromMonthDayValid = periodToDayValid

-- | Bidirectional abstract constructor.
-- Invalid days of month will be clipped to the correct range.
pattern MonthDay :: Month -> DayOfMonth -> Day
pattern MonthDay m dm <- (toMonthDay -> (m,dm)) where
    MonthDay = fromMonthDay

{-# COMPLETE MonthDay #-}

instance DayPeriod Month where
    periodFirstDay (YearMonth y m) = fromGregorian y m 1
    periodLastDay (YearMonth y m) = fromGregorian y m 31 -- clips to correct day
    dayPeriod (toGregorian -> (y, my, _)) = YearMonth y my

#endif
