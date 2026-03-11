{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Time.Calendar.Types (
    Year,
    MonthOfYear,
    DayOfMonth,
    DayOfYear,
    WeekOfYear,
    pattern CommonEra,
    pattern BeforeCommonEra,
) where

#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar          (DayOfMonth, MonthOfYear, Year)
import Data.Time.Calendar.MonthDay (DayOfYear)
import Data.Time.Calendar.WeekDate (WeekOfYear)
#endif

#if MIN_VERSION_time(1,12,1)
import Data.Time.Calendar (pattern CommonEra, pattern BeforeCommonEra)
#endif

#if !MIN_VERSION_time(1,11,0)

-- | Year of Common Era.
type Year = Integer

-- | Month of year, in range 1 (January) to 12 (December).
type MonthOfYear = Int

-- | Day of month, in range 1 to 31.
type DayOfMonth = Int

-- | Day of year, in range 1 (January 1st) to 366.
-- December 31st is 365 in a common year, 366 in a leap year.
type DayOfYear = Int

-- | Week of year, by various reckonings, generally in range 0-53 depending on reckoning
type WeekOfYear = Int

#endif

#if !MIN_VERSION_time(1,12,1)
-- | Also known as Anno Domini.
pattern CommonEra :: Integer -> Year
pattern CommonEra n <-
    ((\y -> if y > 0 then Just y else Nothing) -> Just n)
    where
        CommonEra n = n

-- | Also known as Before Christ.
-- Note that Year 1 = 1 CE, and the previous Year 0 = 1 BCE.
-- 'CommonEra' and 'BeforeCommonEra' form a @COMPLETE@ set.
pattern BeforeCommonEra :: Integer -> Year
pattern BeforeCommonEra n <-
    ((\y -> if y <= 0 then Just (1 - y) else Nothing) -> Just n)
    where
        BeforeCommonEra n = 1 - n

{-# COMPLETE CommonEra, BeforeCommonEra #-}
#endif
