{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.FuzzyTime.Resolve
  ( -- * Local time
    resolveLocalTimeForwards,
    resolveLocalTimeBackwards,

    -- * Time Of Day
    resolveTimeOfDayForwards,
    resolveTimeOfDayBackwards,
    normaliseTimeOfDay,
    morning,
    evening,

    -- * Day
    resolveDayForwards,
    resolveDayBackwards,

    -- ** Resolution helpers
    nextDayOfMonth,
    previousDayOfMonth,
    nextDayOfMonthOfYear,
    previousDayOfMonthOfYear,
    nextDayOfWeek,
    previousDayOfWeek,
  )
where

import Data.Fixed (Pico, mod')
import Data.FuzzyTime.Types (AmbiguousLocalTime (BothTimeAndDay, OnlyDaySpecified), FuzzyDay (..), FuzzyLocalTime (..), FuzzyTimeOfDay (AtExact, AtHour, AtMinute, Evening, HoursDiff, Midnight, MinutesDiff, Morning, Noon, SameTime, SecondsDiff))
import Data.Time (Day, DayOfWeek, LocalTime (LocalTime), TimeOfDay (TimeOfDay), addDays, midday, midnight, toGregorian)
import Data.Time.Calendar.Month (Month, fromMonthDayValid, fromYearMonthValid, pattern MonthDay)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Data.Word (Word8)

resolveLocalTimeForwards :: LocalTime -> FuzzyLocalTime -> Maybe AmbiguousLocalTime
resolveLocalTimeForwards (LocalTime ld ltod) = \case
  FuzzyLocalTimeDay fd -> OnlyDaySpecified <$> resolveDayForwards ld fd
  FuzzyLocalTimeTimeOfDay ftod -> do
    (d, tod) <- resolveTimeOfDayForwardsWithDiff ltod ftod
    pure $ BothTimeAndDay $ LocalTime (addDays d ld) tod
  FuzzyLocalTimeBoth fd ftod -> do
    let withDiff = resolveTimeOfDayForwardsWithDiff ltod ftod
        withoutDiff = (,) 0 <$> resolveTimeOfDayForwards ltod ftod
    (d, tod) <-
      case fd of
        Now -> withDiff
        Today -> withDiff
        _ -> withoutDiff
    day <- addDays d <$> resolveDayForwards ld fd
    pure $ BothTimeAndDay $ LocalTime day tod

resolveLocalTimeBackwards :: LocalTime -> FuzzyLocalTime -> Maybe AmbiguousLocalTime
resolveLocalTimeBackwards (LocalTime ld ltod) = \case
  FuzzyLocalTimeDay fd -> OnlyDaySpecified <$> resolveDayBackwards ld fd
  FuzzyLocalTimeTimeOfDay ftod -> do
    (d, tod) <- resolveTimeOfDayBackwardsWithDiff ltod ftod
    pure $ BothTimeAndDay $ LocalTime (addDays d ld) tod
  FuzzyLocalTimeBoth fd ftod -> do
    let withDiff = resolveTimeOfDayBackwardsWithDiff ltod ftod
        withoutDiff = (,) 0 <$> resolveTimeOfDayBackwards ltod ftod
    (d, tod) <-
      case fd of
        Now -> withDiff
        Today -> withDiff
        _ -> withoutDiff
    day <- addDays d <$> resolveDayBackwards ld fd
    pure $ BothTimeAndDay $ LocalTime day tod

resolveTimeOfDayForwards :: TimeOfDay -> FuzzyTimeOfDay -> Maybe TimeOfDay
resolveTimeOfDayForwards tod ftod = snd <$> resolveTimeOfDayForwardsWithDiff tod ftod

resolveTimeOfDayBackwards :: TimeOfDay -> FuzzyTimeOfDay -> Maybe TimeOfDay
resolveTimeOfDayBackwards tod ftod = snd <$> resolveTimeOfDayBackwardsWithDiff tod ftod

resolveTimeOfDayForwardsWithDiff :: TimeOfDay -> FuzzyTimeOfDay -> Maybe (Integer, TimeOfDay)
resolveTimeOfDayForwardsWithDiff tod@(TimeOfDay h m s) ftod =
  case ftod of
    SameTime -> Just (0, tod)
    Noon -> Just $ next midday
    Midnight -> Just $ next midnight
    Morning -> Just $ next morning
    Evening -> Just $ next evening
    AtHour h_ -> Just $ next $ TimeOfDay h_ 0 0
    AtMinute h_ m_ -> Just $ next $ TimeOfDay h_ m_ 0
    AtExact tod_ -> Just $ next tod_
    HoursDiff hd -> Just $ normaliseTimeOfDay (h + hd) m s
    MinutesDiff md -> Just $ normaliseTimeOfDay h (m + md) s
    SecondsDiff sd -> Just $ normaliseTimeOfDay h m (s + sd)
  where
    next tod_ = (skipIf (>= tod_), tod_)
    skipIf p =
      if p tod
        then 1
        else 0

resolveTimeOfDayBackwardsWithDiff :: TimeOfDay -> FuzzyTimeOfDay -> Maybe (Integer, TimeOfDay)
resolveTimeOfDayBackwardsWithDiff tod@(TimeOfDay h m s) ftod =
  case ftod of
    SameTime -> Just (0, tod)
    Noon -> Just $ previous midday
    Midnight -> Just $ previous midnight
    Morning -> Just $ previous morning
    Evening -> Just $ previous evening
    AtHour h_ -> Just $ previous $ TimeOfDay h_ 0 0
    AtMinute h_ m_ -> Just $ previous $ TimeOfDay h_ m_ 0
    AtExact tod_ -> Just $ previous tod_
    HoursDiff hd -> Just $ normaliseTimeOfDay (h + hd) m s
    MinutesDiff md -> Just $ normaliseTimeOfDay h (m + md) s
    SecondsDiff sd -> Just $ normaliseTimeOfDay h m (s + sd)
  where
    previous tod_ = (skipIf (<= tod_), tod_)
    skipIf p =
      if p tod
        then (-1)
        else 0

normaliseTimeOfDay :: Int -> Int -> Pico -> (Integer, TimeOfDay)
normaliseTimeOfDay h m s =
  let s' = s `mod'` 60
      totalM = m + round (s - s') `div` 60
      m' = totalM `mod` 60
      totalH = h + (totalM - m') `div` 60
      h' = totalH `mod` 24
      totalD = (totalH - h') `div` 24
   in (fromIntegral totalD, TimeOfDay h' m' s')

morning :: TimeOfDay
morning = TimeOfDay 6 0 0

evening :: TimeOfDay
evening = TimeOfDay 18 0 0

resolveDayForwards :: Day -> FuzzyDay -> Maybe Day
resolveDayForwards d fd =
  case fd of
    Yesterday -> Just $ addDays (-1) d
    Now -> Just d
    Today -> Just d
    Tomorrow -> Just $ addDays 1 d
    OnlyDay di -> nextDayOfMonth d di
    DayInMonth mi di -> nextDayOfMonthOfYear d mi di
    DiffDays ds -> Just $ addDays (fromIntegral ds) d
    DiffWeeks ws -> Just $ addDays (7 * fromIntegral ws) d
    DiffMonths ms -> Just $ addDays (30 * fromIntegral ms) d
    DayOfTheWeek dow diff -> Just $ addDays (7 * fromIntegral diff) (nextDayOfWeek d dow)
    ExactDay d_ -> Just d_

resolveDayBackwards :: Day -> FuzzyDay -> Maybe Day
resolveDayBackwards d fd =
  case fd of
    Yesterday -> Just $ addDays (-1) d
    Now -> Just d
    Today -> Just d
    Tomorrow -> Just $ addDays 1 d
    OnlyDay di -> previousDayOfMonth d di
    DayInMonth mi di -> previousDayOfMonthOfYear d mi di
    DiffDays ds -> Just $ addDays (fromIntegral ds) d
    DiffWeeks ws -> Just $ addDays (7 * fromIntegral ws) d
    DiffMonths ms -> Just $ addDays (30 * fromIntegral ms) d
    DayOfTheWeek dow diff -> Just $ addDays (7 * fromIntegral diff) (previousDayOfWeek d dow)
    ExactDay d_ -> Just d_

nextDayOfMonth :: Day -> Word8 -> Maybe Day
nextDayOfMonth = dayOfMonthHelper nextAfterDay succ

previousDayOfMonth :: Day -> Word8 -> Maybe Day
previousDayOfMonth = dayOfMonthHelper previousBeforeDay pred

dayOfMonthHelper ::
  (Day -> Maybe Day -> Maybe Day -> Maybe Day) ->
  (Month -> Month) ->
  Day ->
  Word8 ->
  Maybe Day
dayOfMonthHelper chooser changer d wi =
  let di :: Int
      di = fromIntegral wi
      MonthDay thisMonth _ = d
      guessThisMonth = fromMonthDayValid thisMonth (fromIntegral wi)
      guessOtherMonth = fromMonthDayValid (changer thisMonth) di
   in chooser d guessThisMonth guessOtherMonth

nextDayOfMonthOfYear :: Day -> Word8 -> Word8 -> Maybe Day
nextDayOfMonthOfYear = dayOfMonthOfYearHelper nextAfterDay succ

previousDayOfMonthOfYear :: Day -> Word8 -> Word8 -> Maybe Day
previousDayOfMonthOfYear = dayOfMonthOfYearHelper previousBeforeDay pred

dayOfMonthOfYearHelper ::
  (Day -> Maybe Day -> Maybe Day -> Maybe Day) ->
  (Integer -> Integer) ->
  Day ->
  Word8 ->
  Word8 ->
  Maybe Day
dayOfMonthOfYearHelper chooser changer d mw dw =
  let mi = fromIntegral mw
      di = fromIntegral dw
      (y, _, _) = toGregorian d
      current =
        fromYearMonthValid y mi >>= \m ->
          fromMonthDayValid m di
      other =
        fromYearMonthValid (changer y) mi >>= \m ->
          fromMonthDayValid m di
   in chooser d current other

nextDayOfWeek :: Day -> DayOfWeek -> Day
nextDayOfWeek = dayOfWeekHelper (\d current after -> if current > d then current else after) (addDays 7)

previousDayOfWeek :: Day -> DayOfWeek -> Day
previousDayOfWeek = dayOfWeekHelper (\d current before -> if current < d then current else before) (addDays (-7))

dayOfWeekHelper ::
  (Day -> Day -> Day -> Day) ->
  (Day -> Day) ->
  Day ->
  DayOfWeek ->
  Day
dayOfWeekHelper chooser changer day dow =
  let (y, woy, _) = toWeekDate day
      currentGuess = fromWeekDate y woy (fromEnum dow)
      otherGuess = changer currentGuess
   in chooser day currentGuess otherGuess

nextAfterDay :: Day -> Maybe Day -> Maybe Day -> Maybe Day
nextAfterDay today beforeGuess afterGuess =
  case beforeGuess of
    Just d ->
      if d > today
        then beforeGuess
        else afterGuess
    Nothing -> afterGuess

previousBeforeDay :: Day -> Maybe Day -> Maybe Day -> Maybe Day
previousBeforeDay today afterGuess beforeGuess =
  case afterGuess of
    Just d ->
      if d < today
        then afterGuess
        else beforeGuess
    Nothing -> beforeGuess
