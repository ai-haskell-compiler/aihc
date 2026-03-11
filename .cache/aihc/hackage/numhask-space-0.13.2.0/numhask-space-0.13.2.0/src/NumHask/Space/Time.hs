{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | data algorithms related to time (as a Space)
module NumHask.Space.Time
  ( TimeGrain (..),
    floorGrain,
    ceilingGrain,
    addGrain,
    sensibleTimeGrid,
    PosDiscontinuous (..),
    placedTimeLabelDiscontinuous,
    placedTimeLabelContinuous,
    fromNominalDiffTime,
    toNominalDiffTime,
    fromDiffTime,
    toDiffTime,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Data
import Data.Fixed (Fixed (MkFixed))
import Data.Sequence qualified as Seq
import Data.Text (Text, pack, unpack)
import Data.Time hiding (Hours, Minutes, Seconds)
import NumHask.Prelude
import NumHask.Space.Range
import NumHask.Space.Types

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space
-- >>> import NumHask.Space.Time (TimeGrain(..))
-- >>> import Data.Text (Text, pack)
-- >>> import Data.Time hiding (Hours, Minutes, Seconds)

-- | a step in time
data TimeGrain
  = Years Int
  | Months Int
  | Days Int
  | Hours Int
  | Minutes Int
  | Seconds Double
  deriving (Eq, Show, Generic, Data)

grainSecs :: TimeGrain -> Double
grainSecs (Years n) = fromIntegral n * 365.0 * fromNominalDiffTime nominalDay
grainSecs (Months n) = fromIntegral n * 365.0 / 12 * fromNominalDiffTime nominalDay
grainSecs (Days n) = fromIntegral n * fromNominalDiffTime nominalDay
grainSecs (Hours n) = fromIntegral n * 60 * 60
grainSecs (Minutes n) = fromIntegral n * 60
grainSecs (Seconds n) = n

-- | convenience conversion to Double
fromNominalDiffTime :: NominalDiffTime -> Double
fromNominalDiffTime t = fromInteger i * 1e-12
  where
    (MkFixed i) = nominalDiffTimeToSeconds t

-- | convenience conversion from Double
toNominalDiffTime :: Double -> NominalDiffTime
toNominalDiffTime x =
  let d0 = ModifiedJulianDay 0
      days = floor (x / fromNominalDiffTime nominalDay)
      secs = x - fromIntegral days * fromNominalDiffTime nominalDay
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime (addDays (fromIntegral days) d0) (picosecondsToDiffTime . fromIntegral $ floor (secs / 1.0e-12))
   in diffUTCTime t1 t0

-- | Convert from 'DiffTime' to seconds (as a Double)
--
-- >>> fromDiffTime $ toDiffTime 1
-- 1.0
fromDiffTime :: DiffTime -> Double
fromDiffTime = (* 1e-12) . fromInteger . diffTimeToPicoseconds

-- | Convert from seconds (as a Double) to 'DiffTime'
-- >>> toDiffTime 1
-- 1s
toDiffTime :: Double -> DiffTime
toDiffTime = picosecondsToDiffTime . fromIntegral . floor . (* 1e12)

-- | add a TimeGrain to a UTCTime
--
-- >>> addGrain (Years 1) 5 (UTCTime (fromGregorian 2015 2 28) (toDiffTime 0))
-- 2020-02-29 00:00:00 UTC
--
-- >>> addGrain (Months 1) 1 (UTCTime (fromGregorian 2015 2 28) (toDiffTime 0))
-- 2015-03-31 00:00:00 UTC
--
-- >>> addGrain (Hours 6) 5 (UTCTime (fromGregorian 2015 2 28) (toDiffTime 0))
-- 2015-03-01 06:00:00 UTC
--
-- >>> addGrain (Seconds 0.001) (60*1000+1) (UTCTime (fromGregorian 2015 2 28) (toDiffTime 0))
-- 2015-02-28 00:01:00.001 UTC
addGrain :: TimeGrain -> Int -> UTCTime -> UTCTime
addGrain (Years n) x (UTCTime d t) =
  UTCTime (addDays (-1) $ addGregorianYearsClip (fromIntegral n * fromIntegral x) (addDays 1 d)) t
addGrain (Months n) x (UTCTime d t) =
  UTCTime (addDays (-1) $ addGregorianMonthsClip (fromIntegral (n * x)) (addDays 1 d)) t
addGrain (Days n) x (UTCTime d t) = UTCTime (addDays (fromIntegral x * fromIntegral n) d) t
addGrain g@(Hours _) x d = addUTCTime (toNominalDiffTime (fromIntegral x * grainSecs g)) d
addGrain g@(Minutes _) x d = addUTCTime (toNominalDiffTime (fromIntegral x * grainSecs g)) d
addGrain g@(Seconds _) x d = addUTCTime (toNominalDiffTime (fromIntegral x * grainSecs g)) d

addHalfGrain :: TimeGrain -> UTCTime -> UTCTime
addHalfGrain (Years n) (UTCTime d t) =
  UTCTime
    ( addDays (-1) . (if m' == 1 then addGregorianMonthsClip 6 else id) $
        addGregorianYearsClip (fromIntegral d') (addDays 1 d)
    )
    t
  where
    (d', m') = divMod 2 n
addHalfGrain (Months n) (UTCTime d t) =
  UTCTime
    ( addDays (if m' == 1 then 15 else 0 {- sue me -})
        . addDays (-1)
        $ addGregorianMonthsClip (fromIntegral d') (addDays 1 d)
    )
    t
  where
    (d', m') = divMod 2 n
addHalfGrain (Days n) (UTCTime d t) =
  (if m' == 1 then addUTCTime (toNominalDiffTime (0.5 * grainSecs (Days 1))) else id) $
    UTCTime (addDays (fromIntegral d') d) t
  where
    (d', m') = divMod 2 n
addHalfGrain g@(Hours _) d = addUTCTime (toNominalDiffTime (0.5 * grainSecs g)) d
addHalfGrain g@(Minutes _) d = addUTCTime (toNominalDiffTime (0.5 * grainSecs g)) d
addHalfGrain g@(Seconds _) d = addUTCTime (toNominalDiffTime (0.5 * grainSecs g)) d

-- | compute the floor UTCTime based on the timegrain
--
-- >>> floorGrain (Years 5) (UTCTime (fromGregorian 1999 1 1) (toDiffTime 0))
-- 1995-12-31 00:00:00 UTC
--
-- >>> floorGrain (Months 3) (UTCTime (fromGregorian 2016 12 30) (toDiffTime 0))
-- 2016-09-30 00:00:00 UTC
--
-- >>> floorGrain (Days 5) (UTCTime (fromGregorian 2016 12 30) (toDiffTime 1))
-- 2016-12-30 00:00:00 UTC
--
-- >>> floorGrain (Minutes 15) (UTCTime (fromGregorian 2016 12 30) (toDiffTime $ 15*60+1))
-- 2016-12-30 00:15:00 UTC
--
-- >>> floorGrain (Seconds 0.1) (UTCTime (fromGregorian 2016 12 30) ((toDiffTime 0.12)))
-- 2016-12-30 00:00:00.1 UTC
floorGrain :: TimeGrain -> UTCTime -> UTCTime
floorGrain (Years n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' 1 1) (secondsToDiffTime 0)
  where
    (y, _, _) = toGregorian (addDays 1 d)
    y' = fromIntegral $ 1 + n * fromIntegral (floor (fromIntegral (y - 1) / fromIntegral n :: Double))
floorGrain (Months n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y m' 1) (secondsToDiffTime 0)
  where
    (y, m, _) = toGregorian (addDays 1 d)
    m' = fromIntegral (1 + fromIntegral n * floor (fromIntegral (m - 1) / fromIntegral n :: Double))
floorGrain (Days _) (UTCTime d _) = UTCTime d (secondsToDiffTime 0)
floorGrain (Hours h) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ fromIntegral (h * 3600 * fromIntegral (floor (s / (fromIntegral h * 3600)))) - s
floorGrain (Minutes m) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ fromIntegral (m * 60 * fromIntegral (floor (s / (fromIntegral m * 60)))) - s
floorGrain (Seconds secs) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ (secs * fromIntegral (floor (s / secs))) - s

-- | compute the ceiling UTCTime based on the timegrain
--
-- >>> ceilingGrain (Years 5) (UTCTime (fromGregorian 1999 1 1) (toDiffTime 0))
-- 2000-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Months 3) (UTCTime (fromGregorian 2016 12 30) (toDiffTime 0))
-- 2016-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Days 5) (UTCTime (fromGregorian 2016 12 30) (toDiffTime 1))
-- 2016-12-31 00:00:00 UTC
--
-- >>> ceilingGrain (Minutes 15) (UTCTime (fromGregorian 2016 12 30) (toDiffTime $ 15*60+1))
-- 2016-12-30 00:30:00 UTC
--
-- >>> ceilingGrain (Seconds 0.1) (UTCTime (fromGregorian 2016 12 30) (toDiffTime 0.12))
-- 2016-12-30 00:00:00.2 UTC
ceilingGrain :: TimeGrain -> UTCTime -> UTCTime
ceilingGrain (Years n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' 1 1) (secondsToDiffTime 0)
  where
    (y, _, _) = toGregorian (addDays 1 d)
    y' = fromIntegral $ 1 + n * fromIntegral (ceiling (fromIntegral (y - 1) / fromIntegral n :: Double))
ceilingGrain (Months n) (UTCTime d _) = UTCTime (addDays (-1) $ fromGregorian y' m'' 1) (secondsToDiffTime 0)
  where
    (y, m, _) = toGregorian (addDays 1 d)
    m' = (m + n - 1) `div` n * n
    (y', m'') = fromIntegral <$> if m' == 12 then (y + 1, 1) else (y, m' + 1)
ceilingGrain (Days _) (UTCTime d t) = if t == secondsToDiffTime 0 then UTCTime d (secondsToDiffTime 0) else UTCTime (addDays 1 d) (secondsToDiffTime 0)
ceilingGrain (Hours h) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ fromIntegral (h * 3600 * fromIntegral (ceiling (s / (fromIntegral h * 3600)))) - s
ceilingGrain (Minutes m) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ fromIntegral (m * 60 * fromIntegral (ceiling (s / (fromIntegral m * 60)))) - s
ceilingGrain (Seconds secs) u@(UTCTime _ t) = addUTCTime x u
  where
    s = fromDiffTime t
    x = toNominalDiffTime $ (secs * fromIntegral (ceiling (s / secs))) - s

-- | whether to include lower and upper times
data PosDiscontinuous = PosInnerOnly | PosIncludeBoundaries

-- | Dates used for time series analysis or attached to charts are often discontinuous, but we want to smooth that reality over and show a continuous range on the axis.
--
-- The assumption with getSensibleTimeGrid is that there is a list of discountinuous UTCTimes rather than a continuous range.  Output is a list of index points for the original [UTCTime] and label tuples, and a list of unused list elements.
--
-- >>> placedTimeLabelDiscontinuous PosIncludeBoundaries (Just (pack "%d %b")) 2 [UTCTime (fromGregorian 2017 12 6) (toDiffTime 0), UTCTime (fromGregorian 2017 12 29) (toDiffTime 0), UTCTime (fromGregorian 2018 1 31) (toDiffTime 0), UTCTime (fromGregorian 2018 3 3) (toDiffTime 0)]
-- ([(0,"06 Dec"),(1,"31 Dec"),(2,"28 Feb"),(3,"03 Mar")],[])
placedTimeLabelDiscontinuous :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> ([(Int, Text)], [UTCTime])
placedTimeLabelDiscontinuous _ _ _ [] = ([], [])
placedTimeLabelDiscontinuous posd format n ts = (zip (fst <$> inds') labels, rem')
  where
    r@(Range l u) = unsafeSpace1 ts
    (grain, tps) = sensibleTimeGrid InnerPos n r
    tps' = case posd of
      PosInnerOnly -> tps
      PosIncludeBoundaries -> [l] <> tps <> [u]
    begin = (tps', Seq.empty, zero :: Int)
    done (p, x, _) = (p, toList x)
    step ([], xs, n) _ = ([], xs, n)
    step (p : ps, xs, n) a
      | p == a = step (ps, xs Seq.:|> (n, p), n) a
      | p > a = (p : ps, xs, n + 1)
      | otherwise = step (ps, xs Seq.:|> (n - 1, p), n) a
    (rem', inds) = done $ foldl' step begin ts
    inds' = laterTimes inds
    fmt = case format of
      Just f -> unpack f
      Nothing -> autoFormat grain
    labels = pack . formatTime defaultTimeLocale fmt . snd <$> inds'

autoFormat :: TimeGrain -> String
autoFormat (Years x)
  | x == 1 = "%b %Y"
  | otherwise = "%Y"
autoFormat (Months _) = "%d %b %Y"
autoFormat (Days _) = "%d %b %y"
autoFormat (Hours x)
  | x > 3 = "%d/%m/%y %R"
  | otherwise = "%R"
autoFormat (Minutes _) = "%R"
autoFormat (Seconds _) = "%T%Q"

laterTimes :: [(Int, a)] -> [(Int, a)]
laterTimes [] = []
laterTimes [x] = [x]
laterTimes (x : xs) =
  (\(x, xs) -> toList $ xs Seq.:|> x) $
    foldl' step (x, Seq.empty) xs
  where
    step ((n, a), rs) (na, aa) =
      bool ((na, aa), rs Seq.:|> (n, a)) ((na, aa), rs) (na == n)

-- | A sensible time grid between two dates, projected onto (0,1) with no attempt to get finnicky.
--
-- >>> placedTimeLabelContinuous PosIncludeBoundaries (Just (pack "%d %b")) 2 (Range (UTCTime (fromGregorian 2017 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2017 12 29) (toDiffTime 0)))
-- [(0.0,"06 Dec"),(0.4347826086956521,"16 Dec"),(0.8695652173913042,"26 Dec"),(0.9999999999999999,"29 Dec")]
placedTimeLabelContinuous :: PosDiscontinuous -> Maybe Text -> Int -> Range UTCTime -> [(Double, Text)]
placedTimeLabelContinuous posd format n r@(Range l u) = zip tpsd labels
  where
    (grain, tps) = sensibleTimeGrid InnerPos n r
    tps' = case posd of
      PosInnerOnly -> tps
      PosIncludeBoundaries -> nubOrd $ [l] <> tps <> [u]
    fmt = case format of
      Just f -> unpack f
      Nothing -> autoFormat grain
    labels = pack . formatTime defaultTimeLocale fmt <$> tps'
    r' = fromNominalDiffTime $ diffUTCTime u l
    tpsd = (/ r') . fromNominalDiffTime . flip diffUTCTime l <$> tps'

-- | compute a sensible TimeGrain and list of UTCTimes
--
-- >>> sensibleTimeGrid InnerPos 2 (Range (UTCTime (fromGregorian 2016 12 31) (toDiffTime 0)) (UTCTime (fromGregorian 2017 12 31) (toDiffTime 0)))
-- (Months 6,[2016-12-31 00:00:00 UTC,2017-06-30 00:00:00 UTC,2017-12-31 00:00:00 UTC])
--
-- >>> sensibleTimeGrid InnerPos 2 (Range (UTCTime (fromGregorian 2017 1 1) (toDiffTime 0)) (UTCTime (fromGregorian 2017 12 30) (toDiffTime 0)))
-- (Months 6,[2017-06-30 00:00:00 UTC])
--
-- >>> sensibleTimeGrid UpperPos 2 (Range (UTCTime (fromGregorian 2017 1 1) (toDiffTime 0)) (UTCTime (fromGregorian 2017 12 30) (toDiffTime 0)))
-- (Months 6,[2017-06-30 00:00:00 UTC,2017-12-31 00:00:00 UTC])
--
-- >>> sensibleTimeGrid LowerPos 2 (Range (UTCTime (fromGregorian 2017 1 1) (toDiffTime 0)) (UTCTime (fromGregorian 2017 12 30) (toDiffTime 0)))
-- (Months 6,[2016-12-31 00:00:00 UTC,2017-06-30 00:00:00 UTC])
sensibleTimeGrid :: Pos -> Int -> Range UTCTime -> (TimeGrain, [UTCTime])
sensibleTimeGrid p n (Range l u) = (grain, ts)
  where
    span' = u `diffUTCTime` l
    grain = stepSensibleTime p span' n
    first' = floorGrain grain l
    last' = ceilingGrain grain u
    n' =
      round $
        fromNominalDiffTime (diffUTCTime last' first')
          / grainSecs grain
    posns = case p of
      OuterPos -> take (fromIntegral $ n' + 1)
      InnerPos ->
        drop (bool one zero (first' == l))
          . take (fromIntegral $ n' + bool zero one (last' == u))
      UpperPos -> drop 1 . take (fromIntegral $ n' + 1)
      LowerPos -> take (fromIntegral n')
      MidPos -> take (fromIntegral n')
    ts = case p of
      MidPos ->
        take (fromIntegral n') $
          addHalfGrain grain
            . (\x -> addGrain grain x first')
            <$> [0 ..]
      _notMid -> posns $ (\x -> addGrain grain x first') <$> [0 ..]

-- come up with a sensible step for a grid over a Field, where sensible means the 18th century
-- practice of using multiples of 3 to round
stepSensible3 ::
  Pos ->
  Double ->
  Int ->
  Double
stepSensible3 tp span' n =
  step
    + if tp == MidPos
      then step / 2
      else 0
  where
    step' = 10 ^^ floor (logBase 10 (span' / fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.05 = 12 * step'
      | err <= 0.3 = 6 * step'
      | err <= 0.5 = 3 * step'
      | otherwise = step'

-- | come up with a sensible TimeGrain over a NominalDiffTime
stepSensibleTime :: Pos -> NominalDiffTime -> Int -> TimeGrain
stepSensibleTime tp span' n
  | yearsstep >= 1 = Years (floor yearsstep)
  | monthsstep >= 1 = Months (fromIntegral (floor monthsstep))
  | daysstep >= 1 = Days (fromIntegral (floor daysstep))
  | hoursstep >= 1 = Hours (fromIntegral (floor hoursstep))
  | minutesstep >= 1 = Minutes (fromIntegral (floor minutesstep))
  | secondsstep >= 1 = Seconds secondsstep3
  | otherwise = Seconds secondsstep
  where
    sp = fromNominalDiffTime span'
    minutes = sp / 60
    hours = sp / (60 * 60)
    days = sp / fromNominalDiffTime nominalDay
    years = sp / 365 / fromNominalDiffTime nominalDay
    months' = years * 12
    yearsstep = stepSensible tp years n
    monthsstep = stepSensible3 tp months' n
    daysstep = stepSensible tp days n
    hoursstep = stepSensible3 tp hours n
    minutesstep = stepSensible3 tp minutes n
    secondsstep3 = stepSensible3 tp sp n
    secondsstep = stepSensible tp sp n
