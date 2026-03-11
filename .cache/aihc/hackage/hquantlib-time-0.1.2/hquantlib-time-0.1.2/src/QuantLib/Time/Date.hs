module QuantLib.Time.Date
        ( BusinessDayConvention(..)
        , Day(..)
        , Holiday(..)
        , getDaysBetween
        , isWeekEnd
        , getNextBusinessDay
        ) where

import Data.Time.Calendar (Day(..), addDays, toGregorian)
import Data.Time.Calendar.WeekDate (dayOfWeek, DayOfWeek(..))

{- | Business Day conventions
 - These conventions specify the algorithm used to adjust a date in case it is not a valid business day.
 -}
data BusinessDayConvention = Following 
        | ModifiedFollowing 
        | Preceding
        | ModifiedPreceding
        | Unadjusted
        deriving (Show, Eq, Enum)

-- | Defines a holidays for given calendar. Corresponds to calendar class in QuantLib
class Holiday m where
        isHoliday :: m->(Integer, Int, Int)->Bool
        
        isBusinessDay :: m->Day->Bool
        isBusinessDay m d = not (isHoliday m $ toGregorian d)

        hBusinessDayBetween :: m->(Day, Day)->Int
        hBusinessDayBetween m (fd, td) = foldl countDays 0 listOfDates
                where   countDays counter x     = counter + fromEnum (isBusinessDay m x)
                        listOfDates             = getDaysBetween (fd, td)

-- | Generate a list of all dates inbetween
getDaysBetween ::  (Day, Day) -> [Day]
getDaysBetween (fd, td) = reverse $ generator fd []
        where   generator date x
                        | date < td     = generator nextDate (nextDate : x)
                        | otherwise     = x
                        where   nextDate        = addDays 1 date

-- | Checks if the day is a weekend, i.e. Saturday or Sunday
isWeekEnd :: Day->Bool
isWeekEnd d     = (weekday == Saturday) || (weekday == Sunday)
        where   weekday = dayOfWeek d

-- | Gets the next working day
getNextBusinessDay :: Holiday a => a->Day->Day
getNextBusinessDay m d
        | isBusinessDay m nextDay       = nextDay
        | otherwise                     = getNextBusinessDay m nextDay
        where   nextDay = addDays 1 d
