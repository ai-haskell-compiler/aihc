{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

-- | Parse strings that aren't so precise
module Data.Dates.Parsing
  (
    Config (..)
  , DateTime (..)
  , DateInterval (..)
  , Time (..)
  , defaultConfig
  , defaultConfigIO
  , parseDate
  , parseDateTime
  , pAbsDateTime
  , pAbsDate
  , pDate
  , pDateTime
  , time
  , pDateInterval
  , weekdayToInterval
  , dateWeekDay
  , getStartOfThisWeek
  , getStartOfNextWeek
  , lastDate
  , nextDate
  , addInterval
  , negateInterval
  , minusInterval
  , dateInFormat
  , extractDates, extractDatesY, extractDatesConfig
  , extractDateTimes, extractDateTimesY, extractDateTimesConfig
  , extract
  ) where

import Control.Lens
import Control.Monad

import Data.Char                            (toLower)
import Data.Data                            (Data, Typeable)
import Data.Hourglass
import Data.List                            (intercalate, find)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Read (readMaybe)

import Time.System (dateCurrent)

import Data.Dates.Parsing.Internal

data DateInterval = Days Int
                  | Weeks Int
                  | Months Int
                  | Years Int
  deriving (Eq,Show,Data,Typeable)

data Config = Config
    { _now            :: DateTime -- ^ "Current" date/time, to use as base for relative dates
    , _startOfWeekDay :: WeekDay} -- ^ Which day of the week to consider the start day

makeLenses ''Config

defaultConfig :: DateTime -> Config
defaultConfig now' = Config
  {
    _now = now'
  , _startOfWeekDay = Monday
  }

defaultConfigIO :: IO Config
defaultConfigIO = defaultConfig <$> dateCurrent

-- | Weekday as interval from the configure start of the week
weekdayToInterval :: Config -> WeekDay -> DateInterval
weekdayToInterval c wd =
  Days (fromIntegral $ fromEnum wd - fromEnum (c^.startOfWeekDay))

getStartOfThisWeek :: Config -> DateTime
getStartOfThisWeek c = (c^.now) `minusInterval` weekdayToInterval c (dateWeekDay (c^.now))

getStartOfNextWeek :: Config -> DateTime
getStartOfNextWeek c = getStartOfThisWeek c `addInterval` Weeks 1

-- | Get weekday of given date.
dateWeekDay :: DateTime -> WeekDay
dateWeekDay = getWeekDay . timeGetDate

lookupMonth :: String -> Either [Month] Month
lookupMonth = uniqFuzzyMatch

time :: Stream s m Char => ParsecT s st m TimeOfDay
time = do
    h <- fromIntegral <$> number 2 23
    minSep <- optionMaybe $ char ':' <|> char '.'
    (m, mOffset) <-
        case minSep of
            Nothing -> (0,) <$> (optional spaces >> optionMaybe ampm)
            Just _ -> do
                m <- number 2 59
                (m,) <$> (optional spaces >> optionMaybe ampm)

    sep <- optionMaybe $ char ':' <|> char '.'
    (s, offset) <-
        case sep of
            Nothing -> (0,) <$> (optional spaces >> optionMaybe ampm)
            Just _ -> do
                s <- number 2 59
                (s,) <$> (optional spaces >> optionMaybe ampm)

    if h > 12 then -- It shouldn't be a 24 hour time, so just ignore offset, if any
        pure $ TimeOfDay (Hours h) (Minutes m) (Seconds s) 0
    else
        case (mOffset, offset) of
            (Just mo, _) -> pure $ TimeOfDay (Hours (h + fromIntegral mo)) (Minutes m) (Seconds s) 0
            (Nothing, Just o) -> pure $ TimeOfDay (Hours (h + fromIntegral o)) (Minutes m) (Seconds s) 0
            (Nothing, Nothing)-> pure $ TimeOfDay (Hours h) (Minutes m) (Seconds s) 0

ampm :: Stream s m Char => ParsecT s st m Int
ampm = do
  s <- many1 letter
  case uppercase s of
    "AM" -> return 0
    "PM" -> return 12
    _ -> fail "AM/PM expected"

newtype DateFormat = DateFormat [(DatePart, String)]
data DatePart = D | M | Y
data DatePartVal = DV Int | MV Month | YV Int

datePart :: Stream s m Char => DatePart -> ParsecT s st m DatePartVal
datePart M = MV <$> pMonth
datePart D = DV <$> pDay
datePart Y = YV <$> pYear

isYV (YV _) = True
isYV _ = False

isMV (MV _) = True
isMV _ = False

isDV (DV _) = True
isDV _ = False

monthPart :: [DatePartVal] -> Month
monthPart = maybe January (\(MV m) -> m) . find isMV

dayPart :: [DatePartVal] -> Int
dayPart = maybe 1 (\(DV d) -> d) . find isDV

yearPart :: Int -> [DatePartVal] -> Int
yearPart year = maybe year (\(YV y) -> y) . find isYV

makeFormat :: String -> [DatePart] -> DateFormat
makeFormat sep parts = DateFormat $ zip parts $ repeat sep

dateInFormat year (DateFormat parts) = do
    partVals <- zipWithM go [1..] parts

    pure $ Date (yearPart year partVals) (monthPart partVals) (dayPart partVals)
    where
        go i (p, sep)
            -- The last one doesn't need to have a separator.
            | i == length parts = datePart p
            | otherwise = do
                v <- datePart p
                string sep
                pure v

euroNumDate = makeFormat "." [D, M, Y]
writtenDate = DateFormat [(M, " "), (D, ","), (Y, "")]
americanDate = makeFormat "/" [M, D, Y]
dashDate = makeFormat "-" [Y, M, D]
strDate = makeFormat " " [D, M, Y]
spaceDate = makeFormat " " [D, M]
spaceDateMD = makeFormat " " [M, D]

dotDateMonth = makeFormat "." [D, M]
dashDateMonth = makeFormat "-" [M, D]
slashDateMonth = makeFormat "/" [M, D]

pAbsDateTime :: Stream s m Char => Int -> ParsecT s st m DateTime
pAbsDateTime year = do
    date <- pAbsDate year

    optional spaces
    optional $ string "at"
    optional spaces

    maybeT <- optionMaybe time

    case maybeT of
        Nothing -> pure $ DateTime date (TimeOfDay 0 0 0 0)
        Just t -> pure $ DateTime date t

pAbsDate :: Stream s m Char => Int -> ParsecT s st m Date
pAbsDate year = choice $ map (try . dateInFormat year)
    [euroNumDate, americanDate, strDate, writtenDate, dashDate,
     dotDateMonth, dashDateMonth, slashDateMonth, spaceDate, spaceDateMD]

intervalToPeriod :: DateInterval -> Period
intervalToPeriod (Days ds)   = mempty { periodDays   = ds}
intervalToPeriod (Weeks ws)  = mempty { periodDays   = ws*7 }
intervalToPeriod (Months ms) = mempty { periodMonths = ms }
intervalToPeriod (Years ys)  = mempty { periodYears  = ys }

-- | Add date interval to DateTime
addInterval :: DateTime -> DateInterval -> DateTime
addInterval dt@DateTime {dtDate = date} interval =
  dt { dtDate = date `dateAddPeriod` intervalToPeriod interval }

-- | Negate DateInterval value: Days 3 -> Days (-3).
negateInterval :: DateInterval -> DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval :: DateTime -> DateInterval -> DateTime
minusInterval date int = date `addInterval` negateInterval int

maybePlural :: Stream s m Char => String -> ParsecT s st m String
maybePlural str = do
  r <- string str
  optional $ char 's'
  return r

pDateIntervalType :: Stream s m Char => ParsecT s st m (Int -> DateInterval)
pDateIntervalType = do
  s <- choice $ map maybePlural ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' -> return Days
    'w' -> return Weeks
    'm' -> return Months
    'y' -> return Years
    _ -> fail $ "Unknown date interval type: " ++ s

pDateInterval :: Stream s m Char => ParsecT s st m DateInterval
pDateInterval = do
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      spaces
      tp <- pDateIntervalType
      pure $ tp n

pRelDate :: Stream s m Char => Config -> ParsecT s st m DateTime
pRelDate c = do
  offs <- try futureDate
     <|> try pastDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ (c^.now) `addInterval` offs

lastDate :: Stream s m Char => Config -> ParsecT s st m DateTime
lastDate c = do
    string "last"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let lastWeekStart = getStartOfThisWeek c `minusInterval` Weeks 1
      return $ lastWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let lastMonth = now' `minusInterval` Months 1
      return $ lastMonth { dtDate = (dtDate lastMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let lastYear = now' `minusInterval` Years 1
      return $ lastYear { dtDate = (dtDate lastYear) { dateMonth = January, dateDay = 1 } }

nextDate :: Stream s m Char => Config -> ParsecT s st m DateTime
nextDate c = do
    string "next"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    startOfWeekDay' = c^.startOfWeekDay
    now' = c^.now
    byweek = do
      wd <- try (string "week" >> return startOfWeekDay') <|> pWeekDay
      let nextWeekStart = getStartOfNextWeek c
      return $ nextWeekStart `addInterval` weekdayToInterval c wd

    bymonth = do
      string "month"
      let nextMonth = now' `addInterval` Months 1
      return nextMonth { dtDate = (dtDate nextMonth) { dateDay = 1 } }

    byyear = do
      string "year"
      let nextYear = now' `addInterval` Years 1
      return nextYear { dtDate = (dtDate nextYear) { dateMonth = January, dateDay = 1 } }

pWeekDay :: Stream s m Char => ParsecT s st m WeekDay
pWeekDay = do
  w <- many1 (oneOf "mondaytueswnhrfi")
  case uniqFuzzyMatch w :: Either [WeekDay] WeekDay of
    Left ds -> fail $ if null ds
                         then "unknown weekday: " ++ w
                         else "ambiguous weekday '" ++ w ++ "' could mean: " ++ intercalate " or " (map show ds)
    Right d -> return d

futureDate :: Stream s m Char => ParsecT s st m DateInterval
futureDate = do
  string "in "
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Noperino."
    Just n -> do
      char ' '
      tp <- pDateIntervalType
      pure $ tp n

pastDate :: Stream s m Char => ParsecT s st m DateInterval
pastDate = do
  maybeN <- readMaybe <$> many1 digit

  case maybeN of
    Nothing -> fail "Could not parse digit."
    Just n -> do
      char ' '
      tp <- pDateIntervalType
      string " ago"
      pure $ tp $ negate n

today :: Stream s m Char => ParsecT s st m DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

tomorrow :: Stream s m Char => ParsecT s st m DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday :: Stream s m Char => ParsecT s st m DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pByWeek :: Stream s m Char => Config -> ParsecT s st m DateTime
pByWeek c =
  try (lastDate c) <|> nextDate c

-- | Parsec parser for DateTime.
pDateTime :: Stream s m Char
          => Config
          -> ParsecT s st m DateTime
pDateTime c =
      try (pRelDate c)
  <|> try (pByWeek c)
  <|> try (pAbsDateTime (dateYear (timeGetDate (c^.now))))

-- | Parsec parser for Date only.
pDate :: Stream s m Char
      => Config
      -> ParsecT s st m Date
pDate c =
      try (timeGetDate <$> pRelDate c)
  <|> try (timeGetDate <$> pByWeek c)
  <|> try (pAbsDate $ dateYear (timeGetDate (c^.now)))

-- | Parse date/time
parseDate :: Config
          -> String -- ^ String to parse
          -> Either ParseError Date
parseDate c = runParser (pDate c) () ""

-- | Parse date/time
parseDateTime :: Config
              -> String -- ^ String to parse
              -> Either ParseError DateTime
parseDateTime c = runParser (pDateTime c) () ""

-- | Same as extractDatesY, but will get the current year from the system, so you don't have to provide it.
extractDates :: String -> IO [Date]
extractDates str = extractDatesConfig <$> defaultConfigIO <*> pure str

extractDatesConfig :: Config -> String -> [Date]
extractDatesConfig config str =
    case parse (extract (pDate config)) "" str of
        Left err -> []
        Right dates -> dates

-- | Extract dates from a string, with the first argument being the current year (used for things like "Jan 18").
--
-- >>> extractDatesY 2018 "The party will be on 6/9"
-- [Date 2018 June 9]
extractDatesY :: Int -> String -> [Date]
extractDatesY y str =
    case parse (extract (pAbsDate y)) "" str of
        Left err -> error $ show err
        Right dates -> dates

extractDateTimes :: String -> IO [DateTime]
extractDateTimes str = extractDateTimesConfig <$> defaultConfigIO <*> pure str

-- | Extract dates with optional times from a string, with the first argument being the current year (used for things like "Jan 18").
-- If no time is specified, will return time at midnight.
--
-- Note: This function **WILL NOT** parse relative dates like "2 weeks ago." For that, you must use `extractDateTimesConfig` or `extractDateTimes`, because the parser needs to know the exact date.
--
-- >>> extractDateTimesY 2018 "The talk starts at 12.09.12 8:00 AM"
-- [DateTime {dtDate = Date {dateYear = 2012, dateMonth = September, dateDay = 12}, dtTime = TimeOfDay {todHour = 8h, todMin = 0m, todSec = 0s, todNSec = 0ns}}]
--
-- >>> extractDateTimesY 2018 "The party will be on 6/9"
-- [DateTime {dtDate = Date {dateYear = 2018, dateMonth = June, dateDay = 9}, dtTime = TimeOfDay {todHour = 0h, todMin = 0m, todSec = 0s, todNSec = 0ns}}]
extractDateTimesY :: Int -> String -> [DateTime]
extractDateTimesY y str =
    case parse (extract (pAbsDateTime y)) "" str of
        Left err -> []
        Right dates -> dates

extractDateTimesConfig :: Config -> String -> [DateTime]
extractDateTimesConfig config str =
    case parse (extract (pDateTime config)) "" str of
        Left err -> []
        Right dates -> dates

extract :: Stream s m Char => ParsecT s st m a -> ParsecT s st m [a]
extract parser = catMaybes <$> Text.Parsec.manyTill (try (Just <$> loop) <|> (anyChar >> pure Nothing)) eof
    where
        loop = try parser <|> do
            anyChar
            notFollowedBy eof
            loop

