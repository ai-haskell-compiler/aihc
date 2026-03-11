{-|

Calculation of bank holidays in England and Wales, using the rules that have
been in place since 1978, and including all exceptions to the rules in the
years 1995 to 2020. I do not know of any exceptions from 1978 until 1995, so
the calculations may be correct for those years too. Calculations for future
dates are predictions which may be rendered false if exceptions to the rules
are announced.

There are normally 8 bank holidays in England and Wales:

  * New Year's Day
  * Good Friday
  * Easter Monday
  * May Day
  * Spring Bank Holiday
  * Summer Bank Holiday
  * Christmas Day
  * Boxing Day

The rules for determining the precise date of each of these in any given year
are a little involved, since holidays may be moved to avoid falling on a
weekend:

  * The New Year's Day holiday is the 1st of January, or the following Monday if
    the 1st is a weekend.
  * Good Friday and Easter Monday are the Friday and Monday either side of
    Easter Sunday (as calculated by the Gregorian method).
  * May Day is the first Monday in May.
  * The Spring Bank Holiday is the last Monday in May.
  * The Summer Bank Holiday is the last Monday in August.
  * Christmas Day is the 25th of December unless that's a weekend,
    in which case it's the 27th.
  * Boxing Day is the 26th of December unless that's a weekend,
    in which case it's the 28th.

Exceptions may be made to these rules on a year-by-year basis.

This package is a reasonably efficient (constant-time) implementation of these
rules.

-}

module Data.Time.Calendar.BankHoliday.EnglandAndWales 
  ( bankHolidays
  , isBankHoliday
  , countBankHolidays
  ) where

import Data.List  ((\\))
import Data.Time
  ( Day
  , addDays
  , fromGregorian
  , toGregorian
  , toModifiedJulianDay
  )
import Data.Time.Calendar.Easter (gregorianEaster)
import qualified Data.Set as S
  ( Set
  , (\\)
  , fromList
  , member
  , split
  , toList
  , union
  )

{-| List the bank holidays for the given year, in ascending order. Bank
holidays never fall on a weekend. -}
bankHolidays :: Integer -> [Day]
bankHolidays yy = S.toList $ standardHolidays S.\\ filterByYear yy skipped `S.union` filterByYear yy extras
  where

  standardHolidays = S.fromList
    $  [ newYearsDay
       , firstMondayIn may
       , weekBefore $ firstMondayIn jun
       , weekBefore $ firstMondayIn sep ]
    ++ easter
    ++ christmas

  newYearsDay = case wd jan 1 of
    3 {- Sat -} -> jan 3
    4 {- Sun -} -> jan 2
    _           -> jan 1

  easter = let easterSunday = gregorianEaster yy in [addDays (-2) easterSunday, addDays 1 easterSunday]

  christmas = case wd dec 25 of
    2 {- Fri -} -> [dec 25, dec 28]
    3 {- Sat -} -> [dec 27, dec 28]
    4 {- Sun -} -> [dec 26, dec 27]
    _           -> [dec 25, dec 26]

  (jan, may, jun, sep, dec) =
    ( fromGregorian yy 1
    , fromGregorian yy 5
    , fromGregorian yy 6
    , fromGregorian yy 9
    , fromGregorian yy 12
    )

  firstMondayIn mm = addDays (negate $ wd mm 02) (mm 07)

  wd mm dd = toModifiedJulianDay (mm dd) `mod` 7
  weekBefore = addDays (-7)

filterByYear :: Integer -> S.Set Day -> S.Set Day
filterByYear y s0 = s2
  where
  (s1, _) = S.split                (fromGregorian (y+1) 1 1) s0
  (_ ,s2) = S.split (addDays (-1) $ fromGregorian  y    1 1) s1

skipped :: S.Set Day
skipped = S.fromList  [ fromGregorian 1995 05 1
                      , fromGregorian 2002 05 27
                      , fromGregorian 2012 05 28
                      , fromGregorian 2020 05 04
                      , fromGregorian 2022 05 30
                      ]

extras :: S.Set Day
extras  = S.fromList  [ fromGregorian 1995 05 08
                      , fromGregorian 1999 12 31
                      , fromGregorian 2002 06 03
                      , fromGregorian 2002 06 04
                      , fromGregorian 2011 04 29
                      , fromGregorian 2012 06 04
                      , fromGregorian 2012 06 05
                      , fromGregorian 2020 05 08
                      , fromGregorian 2022 06 02
                      , fromGregorian 2022 06 03
                      , fromGregorian 2022 09 19
                      , fromGregorian 2023 05 08
                      ]

extraYears :: [Integer]
extraYears = yearsOf extras \\ yearsOf skipped
  where
  yearsOf s = [y | (y,_,_) <- map toGregorian $ S.toList s]

{-| Returns whether a day is a bank holiday. -}
isBankHoliday :: Day -> Bool
isBankHoliday d = (not $ S.member d skipped) && (S.member d extras || isStandardHoliday)
  where
  (yy,mm,dd)   = toGregorian d
  dayOfWeek    = mod (toModifiedJulianDay d) 7
  isMonday     = dayOfWeek == 5
  isWeekend    = dayOfWeek `elem` [3,4]
  easterSunday = gregorianEaster yy

  isStandardHoliday
    | isWeekend = False
    | isMonday  =  (mm == 1  &&  dd <= 3)
                || (mm == 5  && (dd <= 7 || 31-7 < dd))
                || (mm == 8  &&             31-7 < dd)
                || (mm == 12 && 25 <= dd && dd < 29)
                || d == addDays 1 easterSunday
    | otherwise =  (mm,dd) == (1,1)
                || (mm == 12 && 25 <= dd && (dd < 27 || (dayOfWeek == 6 && dd < 29)))
                || d == addDays (-2) easterSunday

{-| Count the number of bank holidays between two 'Day's.

If @d0 <= d1@ then @countBankHolidays d0 d1@ is the number of 'Day's @d@ for
which @isBankHoliday d && d0 <= d && d < d1@. Note the count includes @d0@ but
excludes @d1@.

Additionally, @countBankHolidays d0 d1 == negate (countBankHolidays d1 d0)@ and
@countBankHolidays d0 d2 == countBankHolidays d0 d1 + countBankHolidays d1 d2@.

 -}
countBankHolidays :: Day -> Day -> Integer
countBankHolidays d0 d1
  = if d0 <= d1 then
      if y0 == y1
        then fromIntegral $ length $ takeWhile (<d1) $ dropWhile (<d0) $ bankHolidays y0
        else fromIntegral (length (takeWhile (<d1) $ bankHolidays y1)
                         - length (takeWhile (<d0) $ bankHolidays y0)
                         + length (dropWhile (<y0) $ takeWhile (<y1) extraYears))
           + 8 * (y1 - y0)
    else negate (countBankHolidays d1 d0)

    where
    (y0,_,_) = toGregorian d0
    (y1,_,_) = toGregorian d1
