{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Time.Calendar.Hebrew.Internal where

import Data.Typeable (Typeable)
import Data.Data (Data)
import Control.Arrow
import Data.Time.Calendar (Day (..))

------ data definitions
data Month = Tishrei | Cheshvan | Kislev | Tevet | Shevat
           | Adar | Adar1 | Adar2
           | Nissan | Iyar | Sivan | Tammuz | Av | Elul
    deriving (Eq, Ord, Show, Enum, Read, Data, Typeable)
data YearType = Chaser | Ksidran | Shlema
    deriving (Eq, Ord, Show, Enum)
data YearLeap = Leap | NonLeap
    deriving (Eq, Ord, Show, Enum)

monthHebrew :: Month -> String
monthHebrew Tishrei = "תשרי"
monthHebrew Cheshvan = "חשון"
monthHebrew Kislev = "כסלו"
monthHebrew Tevet = "טבת"
monthHebrew Shevat = "שבט"
monthHebrew Adar = "אדר"
monthHebrew Adar1 = "אדר א"
monthHebrew Adar2 = "אדר ב"
monthHebrew Nissan = "ניסן"
monthHebrew Iyar = "אייר"
monthHebrew Sivan = "סיון"
monthHebrew Tammuz = "תמוז"
monthHebrew Av = "אב"
monthHebrew Elul = "אלול"

------ newtypes
newtype Chalakim = Chalakim Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
type TotalChalakim = Chalakim

newtype Shaot = Shaot Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Days = Days Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
type Weekday = Days
type Julian = Days
type TotalDays = Days
type Date = Days

newtype Weeks = Weeks Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Months = Months Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Years = Years Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

------ simple conversions
daysFromWeeks :: Weeks -> Days
daysFromWeeks (Weeks w) = Days (w * 7)

weeksFromDays :: Days -> (Weeks, Days)
weeksFromDays (Days d) = (Weeks *** Days) (d `divMod` 7)

shaotFromDays :: Days -> Shaot
shaotFromDays (Days d) = Shaot (d * 24)

daysFromShaot :: Shaot -> (Days, Shaot)
daysFromShaot (Shaot s) = (Days *** Shaot) (s `divMod` 24)

chalakimFromShaot :: Shaot -> Chalakim
chalakimFromShaot (Shaot s) = Chalakim (s * 1080)

shaotFromChalakim :: Chalakim -> (Shaot, Chalakim)
shaotFromChalakim (Chalakim c) = (Shaot *** Chalakim) (c `divMod` 1080)

chalakimFromMonths :: Months -> Chalakim
chalakimFromMonths (Months m) = Chalakim m * lunarMonth

------ constants
lunarMonth :: TotalChalakim
lunarMonth = joinChalakim 0 29 12 793

------ building functions
splitChalakim :: TotalChalakim -> (Weeks, Weekday, Shaot, Chalakim)
splitChalakim tc =
    let (s', c) = shaotFromChalakim tc
        (d', s) = daysFromShaot s'
        (w, d) = weeksFromDays d'
     in (w, d, s, c)

joinChalakim :: Weeks -> Days -> Shaot -> Chalakim -> TotalChalakim
joinChalakim w d s c =
    chalakimFromShaot (shaotFromDays (daysFromWeeks w + d) + s) + c

------ year dependent constants
isLeapYear :: Years -> YearLeap
isLeapYear y =
    let res =
             case y `mod` 19 of
                3 -> Leap
                6 -> Leap
                8 -> Leap
                11 -> Leap
                14 -> Leap
                17 -> Leap
                0 -> Leap -- 19
                _ -> NonLeap
    in res

monthsTilTishrei :: Years -> Months
monthsTilTishrei (Years i) = Months $ (235 * i - 234) `div` 19

firstTishrei :: TotalChalakim
firstTishrei = joinChalakim 0 1 5 204

moladTishrei :: Years -> TotalChalakim
moladTishrei y = chalakimFromMonths (monthsTilTishrei y) + firstTishrei

monthLength :: YearLeap -> YearType -> Month -> Days
monthLength _ _ Tishrei = 30
monthLength _ _ Tevet = 29
monthLength _ _ Shevat = 30
monthLength _ _ Nissan = 30
monthLength _ _ Iyar = 29
monthLength _ _ Sivan = 30
monthLength _ _ Tammuz = 29
monthLength _ _ Av = 30
monthLength _ _ Elul = 29
monthLength Leap _ Adar = 0
monthLength Leap _ Adar1 = 30
monthLength Leap _ Adar2 = 29
monthLength NonLeap _ Adar = 29
monthLength NonLeap _ Adar1 = 0
monthLength NonLeap _ Adar2 = 0
monthLength _ Shlema Cheshvan = 30
monthLength _ _ Cheshvan = 29
monthLength _ Chaser Kislev = 29
monthLength _ _ Kislev = 30

------ conversion functions
dateFromJulian :: YearLeap -> YearType -> Julian -> (Month, Date)
dateFromJulian yl yt j' =
    let ml = monthLength yl yt
        helper :: Month -> Julian -> (Month, Date)
        helper m j
            | ml m >= j = (m, j)
            | m == Elul =
                error $ "Invalid dateFromJulain args: " ++ show (yl, yt, j', j)
            | otherwise = helper (succ m) (j - ml m)
     in helper Tishrei j'

------ determining year stuff
roshHashana :: Years -> TotalDays
roshHashana y = daysFromWeeks w + d + dechiyot
    where
        (w, d, s, c) = splitChalakim $ moladTishrei y
        dechiyot
            | s > 18 || s == 18 && c > 0 =
                case d of
                    0 -> 1
                    1 -> 1
                    2 -> 2 -- otherwise it would be Wednesday
                    3 -> 1
                    4 -> 2 -- otherwise it would be Friday
                    5 -> 1
                    6 -> 2 -- otherwise it would be Sunday
                    _ -> error $ "roshHashana: d ==" ++ show d
            | d `elem` [0, 3, 5] = 1 -- ADU rosh
            | d == 2 &&
              isLeapYear y == NonLeap &&
              (s > 9 ||
               s == 9 && c > 204) = 2
            | isLeapYear (y - 1) == Leap &&
              d == 1 &&
              (s > 15 ||
               s == 15 && c > 589) = 1
            | otherwise = 0

yearLength :: Years -> TotalDays
yearLength y = roshHashana (y + 1) - roshHashana y

julianFromDays :: TotalDays -> (Years, Julian)
julianFromDays td = uncurry helper $ approx td where
    helper :: Years -> TotalDays -> (Years, Julian)
    helper y d -- FIXME do not use yearLength here...
        | yearLength y < d = helper (y + 1) (d - yearLength y)
        | otherwise = (y, fromIntegral d)
    approx :: TotalDays -> (Years, TotalDays)
    approx (Days td') =
        let minYears = Years $ td' `div` 366
            Days rh = roshHashana minYears
            rem' = Days $ td' - rh + 1
         in (minYears, rem')

yearDef :: TotalDays -> TotalDays -> (YearLeap, YearType)
yearDef a b = case b - a of
                353 -> (NonLeap, Chaser)
                354 -> (NonLeap, Ksidran)
                355 -> (NonLeap, Shlema)
                383 -> (Leap, Chaser)
                384 -> (Leap, Ksidran)
                385 -> (Leap, Shlema)
                x -> error $ "Invalid year length: " ++ show x

------ convert dates
data HebrewDate = HebrewDate
    { year :: Int
    , month :: Month
    , date :: Int
    }
    deriving (Eq, Data, Typeable)
instance Show HebrewDate where
    show (HebrewDate y m d) = show d ++ " " ++ show m ++ ", " ++ show y

epochOffset :: Integral i => i
epochOffset = 2052004

fromHebrew :: HebrewDate -> Day
fromHebrew h =
    let Days td = totalDaysFromHebrew h
     in ModifiedJulianDay $ td - epochOffset

toHebrew :: Day -> HebrewDate
toHebrew d' =
    let jd = toModifiedJulianDay d' + epochOffset
        td = fromIntegral jd
        (y, j) = julianFromDays td
        (yl, yt) = yearDef (roshHashana y) (roshHashana $ y + 1)
        (m, d) = dateFromJulian yl yt j
     in HebrewDate (fromIntegral y) m (fromIntegral d)

totalDaysFromHebrew :: HebrewDate -> TotalDays
totalDaysFromHebrew (HebrewDate y m d) =
    let rh = roshHashana $ Years $ fromIntegral y
        rh2 = roshHashana $ Years $ fromIntegral $ y + 1
        (yl, yt) = yearDef rh rh2
        ml = monthLength yl yt
        ds = fromIntegral $ sum $ map ml [Tishrei ..m]
     in rh + ds + fromIntegral d - fromIntegral (ml m) - 1

clip :: HebrewDate -> HebrewDate
clip (HebrewDate y m d) =
    let y' = Years $ fromIntegral y
        (yl, yt) = yearDef (roshHashana y') (roshHashana $ y' + 1)
        m' = adjustMonth yl m
        ml = fromIntegral $ monthLength yl yt m'
        d' = if d > ml then ml else d
     in HebrewDate y m' d'

adjustMonth :: YearLeap -> Month -> Month
adjustMonth Leap Adar = Adar2
adjustMonth Leap x = x
adjustMonth NonLeap Adar1 = Adar
adjustMonth NonLeap Adar2 = Adar
adjustMonth NonLeap x = x

anniversaryInYear :: Int -- ^ year
                  -> HebrewDate
                  -> HebrewDate
anniversaryInYear y (HebrewDate _ m d) = clip $ HebrewDate y m d

nextAnniversary :: HebrewDate -- ^ so to say current date
                -> HebrewDate -- ^ date of event
                -> HebrewDate -- ^ first anniversary of event after current
nextAnniversary curr hd
    | geHD thisYear curr = thisYear
    | otherwise = nextYear
  where
    thisYear = anniversaryInYear (year curr) hd
    nextYear = anniversaryInYear (year curr + 1) hd

geHD :: HebrewDate -> HebrewDate -> Bool
geHD (HebrewDate y1 m1 d1) (HebrewDate y2 m2 d2) =
  case compare y1 y2 of
    LT -> False
    GT -> True
    EQ ->
      case compare m1 m2 of
        LT -> False
        GT -> True
        EQ -> d1 >= d2
