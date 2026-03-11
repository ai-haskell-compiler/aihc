{- |
Copyright   :  (c) Henning Thielemann 2008-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Test.Data.EventList.Relative.TimeEnd (tests) where

import Test.Utility
import Test.QuickCheck (quickCheck)

import qualified Data.EventList.Relative.TimeBody as TimeBodyList
import qualified Data.EventList.Relative.TimeTime as TimeTimeList
import qualified Data.EventList.Relative.TimeMixed as TimeMixedList
import qualified Data.EventList.Relative.MixedTime as MixedTimeList
import qualified Data.EventList.Relative.BodyTime as BodyTimeList
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Data.EventList.Relative.TimeTimePrivate as TimeTimePriv
import qualified Data.EventList.Absolute.TimeTime as AbsTimeTimeList

import Data.EventList.Relative.MixedTime ((/.), (./), empty, )

import Data.EventList.Relative.TimeTimePrivate (($~~), lift, )

import qualified Numeric.NonNegative.Chunky as NonNegChunky
import qualified Numeric.NonNegative.Class  as NonNeg
import Numeric.NonNegative.Class ((-|), zero, add, )
import Data.EventList.Relative.TimeTime (isNormalized, )

import System.Random (Random, randomR, mkStdGen, )
import Control.Monad.Trans.State (state, evalState, gets, modify, )
import Control.Monad (liftM2, )
import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, mapPair, )
import qualified Data.List as List



viewLConsTime :: (Eq body, Eq time) =>
   TimeTimeList.T time body -> Bool
viewLConsTime xs =
   xs == uncurry MixedTimeList.consTime (MixedTimeList.viewTimeL xs)

viewLConsBody :: (Eq body, Eq time) =>
   BodyTimeList.T time body -> Bool
viewLConsBody xs =
   xs == maybe BodyTimeList.empty (uncurry MixedTimeList.consBody) (MixedTimeList.viewBodyL xs)



viewRSnocTime :: (Eq body, Eq time) =>
   TimeTimeList.T time body -> Bool
viewRSnocTime xs =
   xs == uncurry TimeMixedList.snocTime (TimeMixedList.viewTimeR xs)

viewRSnocBody :: (Eq body, Eq time) =>
   TimeBodyList.T time body -> Bool
viewRSnocBody xs =
   xs == maybe TimeBodyList.empty (uncurry TimeMixedList.snocBody) (TimeMixedList.viewBodyR xs)



viewLInfinite :: (NonNeg.C time, Eq body) => NonEmptyList time body -> Bool
viewLInfinite =
   checkInfinite .
   maybe (error "viewBodyL: empty list") snd . MixedTimeList.viewBodyL .
   snd . MixedTimeList.viewTimeL .
   makeInfiniteEventList

viewRInfinite :: (NonNeg.C time, Eq body) => NonEmptyList time body -> Bool
viewRInfinite =
   checkInfinite .
   maybe (error "viewBodyR: empty list") fst . TimeMixedList.viewBodyR .
   fst . TimeMixedList.viewTimeR .
   makeInfiniteEventList


switchLConsTime :: (Eq body, Eq time) =>
   TimeTimeList.T time body -> Bool
switchLConsTime xs =
   xs == MixedTimeList.switchTimeL MixedTimeList.consTime xs

switchLConsBody :: (Eq body, Eq time) =>
   BodyTimeList.T time body -> Bool
switchLConsBody xs =
   xs == MixedTimeList.switchBodyL BodyTimeList.empty MixedTimeList.consBody xs



switchRSnocTime :: (Eq body, Eq time) =>
   TimeTimeList.T time body -> Bool
switchRSnocTime xs =
   xs == TimeMixedList.switchTimeR TimeMixedList.snocTime xs

switchRSnocBody :: (Eq body, Eq time) =>
   TimeBodyList.T time body -> Bool
switchRSnocBody xs =
   xs == TimeMixedList.switchBodyR TimeBodyList.empty TimeMixedList.snocBody xs



switchLInfinite :: (NonNeg.C time, Eq body) => NonEmptyList time body -> Bool
switchLInfinite =
   checkInfinite .
   MixedTimeList.switchBodyL (error "switchBodyL: empty list") (flip const) .
   MixedTimeList.switchTimeL (flip const) .
   makeInfiniteEventList

switchRInfinite :: (NonNeg.C time, Eq body) => NonEmptyList time body -> Bool
switchRInfinite =
   checkInfinite .
   TimeMixedList.switchBodyR (error "switchBodyR: empty list") const .
   TimeMixedList.switchTimeR const .
   makeInfiniteEventList


consInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
consInfinite time body =
   checkInfinite .
   TimeTimeList.cons time body .
   makeInfiniteEventList

consTimeBodyInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
consTimeBodyInfinite time body =
   checkInfinite .
   MixedTimeList.consTime time .
   MixedTimeList.consBody body .
   makeInfiniteEventList


snocInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
snocInfinite time body =
   checkInfinite .
   flip (flip TimeTimeList.snoc body) time .
   makeInfiniteEventList

snocTimeBodyInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
snocTimeBodyInfinite time body =
   checkInfinite .
   flip TimeMixedList.snocTime time .
   flip TimeMixedList.snocBody body .
   makeInfiniteEventList


consInfix :: (NonNeg.C time, Eq body) =>
   time -> body -> time -> time -> body -> time -> Bool
consInfix t0a b0 t0b t1a b1 t1b =
   TimeTimeList.append (t0a /. b0 ./ t0b /. empty) (t1a /. b1 ./ t1b /. empty)
      == (t0a /. b0 ./ (add t0b t1a) /. b1 ./ t1b /. empty)


iterate' :: (a -> a) -> a -> [a]
iterate' f =
   let recourse x = ((:) $! x) $ recourse (f x)
   in  recourse

chunkyShow ::
   Int -> Bool
chunkyShow =
   (\t -> t==t) . take 1000000 . show .
   const (NonNegChunky.fromChunks $ iterate' (2-) (1::TimeDiff))

_chunkyCheck ::
   Int -> Bool
_chunkyCheck =
   (\t -> t==t) .
   take 1000000 .
--   (!!1000000) .
{-
   NonNegChunky.toChunks .
   NonNegChunky.fromChunks .
-}
   iterate' (1+)

{-
With an early implementation of mapTimeTail this resulted in heap exhaustion.
-}
mapTimeTailChunkyInfinite ::
   (NonNeg.C time, Num time, Eq body, Show body) =>
   (body -> body) ->
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
mapTimeTailChunkyInfinite _f =
   (\t -> t==t) . take 1000000 .
   NonNegChunky.toChunks .
   MixedTimeList.switchTimeL const .
{-
   MixedTimeList.switchTimeL
      MixedTimeList.consTime .
-}
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate' (2-) 1)

{-
mapTimeTailChunkyInfinite ::
   (NonNeg.C time, Num time, Eq body, Show body) =>
   (body -> body) ->
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
mapTimeTailChunkyInfinite f =
   (\t -> t==t) . take 1000000 . show .
   MixedTimeList.mapTimeTail (fmap f) .
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)
-}

{-
mapTimeTailChunkyInfinite :: (NonNeg.C time, Eq body) =>
   (body -> body) ->
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
mapTimeTailChunkyInfinite f =
--   not . NonNegChunky.isNull .
--   not . null . NonNegChunky.toChunks .
   (\t -> t==t) . take 1000000 . NonNegChunky.toChunks .
   MixedTimeList.switchTimeL const .
--   TimeTimeList.dropTime 1000000 .
   MixedTimeList.mapTimeTail (fmap f) .
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)


mapTimeTailChunkyInfinite :: (NonNeg.C time, Eq body) =>
   (body -> body) ->
   NonNegChunky.T time ->
   TimeTimeList.T (NonNegChunky.T time) body -> Bool
mapTimeTailChunkyInfinite f time =
   MixedTimeList.mapTimeTail (fmap f) .
   TimeTimeList.delay
      (let infTime = mappend time infTime in infTime)
-}


mapBodyComposition :: (Eq body2, Eq time) =>
   (body0 -> body1) -> (body1 -> body2) -> TimeTimeList.T time body0 -> Bool
mapBodyComposition f g evs =
   TimeTimeList.mapBody (g . f) evs  ==
   TimeTimeList.mapBody g (TimeTimeList.mapBody f evs)

mapTimeComposition :: (Eq body, Eq time2) =>
   (time0 -> time1) -> (time1 -> time2) -> TimeTimeList.T time0 body -> Bool
mapTimeComposition f g evs =
   TimeTimeList.mapTime (g . f) evs  ==
   TimeTimeList.mapTime g (TimeTimeList.mapTime f evs)


mapTimeBodyCommutative :: (Eq body1, Eq time1) =>
   (time0 -> time1) -> (body0 -> body1) -> TimeTimeList.T time0 body0 -> Bool
mapTimeBodyCommutative f g evs =
   TimeTimeList.mapBody g (TimeTimeList.mapTime f evs)  ==
   TimeTimeList.mapTime f (TimeTimeList.mapBody g evs)



mapBodyInfinite :: (NonNeg.C time, Eq body1) =>
   (body0 -> body1) -> NonEmptyList time body0 -> Bool
mapBodyInfinite f =
   checkInfinite . TimeTimeList.mapBody f . makeInfiniteEventList

mapTimeInfinite :: (NonNeg.C time0, Eq time1, Eq body) =>
   (time0 -> time1) -> NonEmptyList time0 body -> Bool
mapTimeInfinite f =
   checkInfinite . TimeTimeList.mapTime f . makeInfiniteEventList



{- |
Does only hold for monotonic functions.
-}
mapNormalize :: (NonNeg.C time, Ord body0, Ord body1) =>
   (body0 -> body1) -> TimeTimeList.T time body0 -> Bool
mapNormalize f =
   isNormalized . TimeTimeList.mapBody f . TimeTimeList.normalize



appendLeftIdentity :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
appendLeftIdentity xs =
   TimeTimeList.append (TimeTimeList.pause zero) xs  ==  xs

appendRightIdentity :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
appendRightIdentity xs =
   TimeTimeList.append xs (TimeTimeList.pause zero)  ==  xs

appendAssociative :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
appendAssociative xs ys zs =
   TimeTimeList.append xs (TimeTimeList.append ys zs)  ==
   TimeTimeList.append (TimeTimeList.append xs ys) zs

appendCons :: (NonNeg.C time, Eq body) =>
   time -> body -> TimeTimeList.T time body -> Bool
appendCons time body xs =
   TimeTimeList.cons time body xs  ==
   TimeTimeList.append (TimeTimeList.cons time body (TimeTimeList.pause zero)) xs

appendSplitAtTime :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
appendSplitAtTime t xs =
   xs == uncurry TimeTimeList.append (TimeTimeList.splitAtTime t xs)

mapBodyAppend :: (Eq body1, NonNeg.C time) =>
   (body0 -> body1) -> TimeTimeList.T time body0 -> TimeTimeList.T time body0 -> Bool
mapBodyAppend f xs ys =
   TimeTimeList.mapBody f (TimeTimeList.append xs ys)  ==
   TimeTimeList.append (TimeTimeList.mapBody f xs) (TimeTimeList.mapBody f ys)


appendFirstInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> TimeTimeList.T time body -> Bool
appendFirstInfinite xs =
   checkInfinite . TimeTimeList.append (makeInfiniteEventList xs)

appendSecondInfinite :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> NonEmptyList time body -> Bool
appendSecondInfinite xs =
   checkInfinite . TimeTimeList.append xs . makeInfiniteEventList


decreaseStartDelay :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
decreaseStartDelay dif xs =
   xs == TimeTimeList.decreaseStart dif (TimeTimeList.delay dif xs)

decreaseStartInfinite :: (NonNeg.C time, Eq body) =>
   time -> NonEmptyList time body -> Bool
decreaseStartInfinite dif =
   checkInfinite .
   TimeTimeList.decreaseStart dif .
   TimeTimeList.delay dif .
   makeInfiniteEventList

delayAdditive :: (NonNeg.C time, Eq body) =>
   time -> time -> TimeTimeList.T time body -> Bool
delayAdditive dif0 dif1 xs =
   TimeTimeList.delay (add dif0 dif1) xs ==
   TimeTimeList.delay dif0 (TimeTimeList.delay dif1 xs)

delayPause :: (NonNeg.C time) =>
   time -> time -> Bool
delayPause dif0 dif1 =
   let pause = TimeTimeList.pause (add dif0 dif1)
   in  TimeTimeList.delay dif0 (TimeTimeList.pause dif1) ==
       (asTypeOf pause (TimeTimeList.cons dif0 () pause))

delayAppendPause :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
delayAppendPause dif xs =
   TimeTimeList.delay dif xs == TimeTimeList.append (TimeTimeList.pause dif) xs

delayInfinite :: (NonNeg.C time, Eq body) =>
   time -> NonEmptyList time body -> Bool
delayInfinite dif =
   checkInfinite .
   TimeTimeList.delay dif .
   makeInfiniteEventList



splitAtTakeDropTime :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
splitAtTakeDropTime t xs =
   (TimeTimeList.takeTime t xs, TimeTimeList.dropTime t xs) ==
   TimeTimeList.splitAtTime t xs

takeTimeEndPause :: (NonNeg.C time, Ord body) =>
   time -> TimeTimeList.T time body -> Bool
takeTimeEndPause t xs =
   t == zero ||
   t >= TimeTimeList.duration xs ||
   zero <  snd (TimeMixedList.viewTimeR (TimeTimeList.takeTime t xs))

takeTimeAppendFirst :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
takeTimeAppendFirst t xs ys =
   TimeTimeList.takeTime t (TimeTimeList.append xs ys) ==
   TimeTimeList.append
      (TimeTimeList.takeTime t xs)
      (TimeTimeList.takeTime (t -| TimeTimeList.duration xs) ys)

takeTimeAppendSecond :: (NonNeg.C time, Num time, Eq body) =>
   time -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
takeTimeAppendSecond t xs0 ys =
   -- the first list must not end with a zero pause
   let xs = TimeTimeList.append xs0 (TimeTimeList.pause 1)
   in  TimeTimeList.takeTime (TimeTimeList.duration xs + t) (TimeTimeList.append xs ys) ==
       TimeTimeList.append xs (TimeTimeList.takeTime t ys)

takeTimeNormalize :: (NonNeg.C time, Ord body) =>
   time -> TimeTimeList.T time body -> Bool
takeTimeNormalize t =
   isNormalized . TimeTimeList.takeTime t . TimeTimeList.normalize

dropTimeNormalize :: (NonNeg.C time, Ord body) =>
   time -> TimeTimeList.T time body -> Bool
dropTimeNormalize t =
   isNormalized . TimeTimeList.dropTime t . TimeTimeList.normalize

takeTimeInfinite :: (NonNeg.C time, Num time, Ord body) =>
   time -> NonEmptyList time body -> Bool
takeTimeInfinite t =
   (t == ) . TimeTimeList.duration .
   TimeTimeList.takeTime t . makeUncollapsedInfiniteEventList

dropTimeInfinite :: (NonNeg.C time, Num time, Ord body) =>
   time -> NonEmptyList time body -> Bool
dropTimeInfinite t =
   checkInfinite . TimeTimeList.dropTime t . makeUncollapsedInfiniteEventList

dropTimeLargeInfinite ::
   (NonNeg.C time, Num time, Ord body) =>
   NonEmptyList time body -> Bool
dropTimeLargeInfinite =
   checkInfinite .
   TimeTimeList.dropTime 10000 .
   makeUncollapsedInfiniteEventList


splitAtTimeLazyInfinite ::
   (NonNeg.C time, Num time, Ord body, Show time, Show body) =>
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
splitAtTimeLazyInfinite =
   not . null . show . snd .
   TimeTimeList.splitAtTime 1000000 .
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)

dropTimeLazyInfinite ::
   (NonNeg.C time, Num time, Ord body, Show time, Show body) =>
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
dropTimeLazyInfinite =
   not . null . show .
   TimeTimeList.dropTime 1000000 .
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)

{-
dropTimeLazyInfinite ::
   (NonNeg.C time, Num time, Ord body) =>
   BodyTimeList.T (NonNegChunky.T time) body -> Bool
dropTimeLazyInfinite =
   (\t -> t==t) . take 100 . NonNegChunky.toChunks .
   MixedTimeList.switchTimeL const .
   TimeTimeList.dropTime 1000000 .
   MixedTimeList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)
-}



durationPause :: (NonNeg.C time) =>
   time -> Bool
durationPause t =
   t == TimeTimeList.duration (TimeTimeList.pause t)

durationAppend :: (NonNeg.C time) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
durationAppend xs ys =
   TimeTimeList.duration (TimeTimeList.append xs ys)  ==
   TimeTimeList.duration xs `add` TimeTimeList.duration ys

durationMerge :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
durationMerge xs ys =
   TimeTimeList.duration (TimeTimeList.merge xs ys)  ==
   max (TimeTimeList.duration xs) (TimeTimeList.duration ys)

durationTakeTime :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
durationTakeTime t xs =
   min (TimeTimeList.duration xs) t ==
   TimeTimeList.duration (TimeTimeList.takeTime t xs)

durationDropTime :: (NonNeg.C time, Eq body) =>
   time -> TimeTimeList.T time body -> Bool
durationDropTime t xs =
   TimeTimeList.duration xs -| t ==
   TimeTimeList.duration (TimeTimeList.dropTime t xs)



concatNaive :: (NonNeg.C time, Eq body) =>
   [TimeTimeList.T time body] -> Bool
concatNaive xs =
   TimeTimeList.concat xs == TimeTimeList.concatNaive xs


equalPrefix :: (Eq time, Eq body) =>
   Int -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
equalPrefix n xs ys =
   Mixed.takeDisparate n $~~ xs ==
   Mixed.takeDisparate n $~~ ys

cycleNaive :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> Bool
cycleNaive xs0 =
   let xs = makeNonEmptyEventList xs0
   in  equalPrefix 100 (TimeTimeList.cycle xs) (TimeTimeList.cycleNaive xs)

cycleInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> Bool
cycleInfinite xs0 =
   let xs = makeInfiniteEventList xs0
   in  equalPrefix 100 xs (TimeTimeList.cycle xs)


filterSatisfy :: (NonNeg.C time) =>
   (body -> Bool) ->
   TimeTimeList.T time body -> Bool
filterSatisfy p =
   all p . TimeTimeList.getBodies . TimeTimeList.filter p

filterProjection :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   TimeTimeList.T time body -> Bool
filterProjection p xs =
   TimeTimeList.filter p xs ==
   TimeTimeList.filter p (TimeTimeList.filter p xs)

filterCommutative :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   (body -> Bool) ->
   TimeTimeList.T time body -> Bool
filterCommutative p q xs =
   TimeTimeList.filter p (TimeTimeList.filter q xs) ==
   TimeTimeList.filter q (TimeTimeList.filter p xs)

filterComposition :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   (body -> Bool) ->
   TimeTimeList.T time body -> Bool
filterComposition p q xs =
   TimeTimeList.filter p (TimeTimeList.filter q xs) ==
   TimeTimeList.filter (\b -> p b && q b) xs

filterNormalize :: (NonNeg.C time, Ord body) =>
   (body -> Bool) ->
   TimeTimeList.T time body -> Bool
filterNormalize p =
   isNormalized . TimeTimeList.filter p . TimeTimeList.normalize

filterAppend :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
filterAppend p xs ys =
   TimeTimeList.filter p (TimeTimeList.append xs ys)  ==
   TimeTimeList.append (TimeTimeList.filter p xs) (TimeTimeList.filter p ys)

filterDuration :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> TimeTimeList.T time body -> Bool
filterDuration p xs =
   TimeTimeList.duration xs == TimeTimeList.duration (TimeTimeList.filter p xs)

filterPartition :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeTimeList.T time body -> Bool
filterPartition p xs =
   (TimeTimeList.filter p xs, TimeTimeList.filter (not . p) xs) ==
   TimeTimeList.partition p xs


filterInfinite :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> NonEmptyList time body -> Bool
filterInfinite p xs =
   null (TimeTimeList.getBodies (TimeTimeList.filter p (makeNonEmptyEventList xs)))
   ||
   (checkInfinite .
    TimeTimeList.filter p .
    makeInfiniteEventList) xs

catMaybesAppend :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time (Maybe body) -> TimeTimeList.T time (Maybe body) -> Bool
catMaybesAppend xs ys =
   TimeTimeList.catMaybes (TimeTimeList.append xs ys)  ==
   TimeTimeList.append (TimeTimeList.catMaybes xs) (TimeTimeList.catMaybes ys)


catMaybesRInfinite :: (NonNeg.C time, Num time, Eq body) =>
   NonEmptyList time (Maybe body) -> Bool
catMaybesRInfinite xs =
   {-
   @(add 1) is needed in order to assert that the accumulated time is infinite
   and can be clipped by @min 100@.
   -}
   let t =
         min 100 $
         List.foldr add zero $
         TimeTimeList.getTimes $
         TimeTimeList.catMaybesR $
         TimeTimeList.mapTime (NonNegChunky.fromNumber . (add 1)) $
         makeInfiniteEventList xs
   in  t == t

catMaybesRInitInfinite :: (NonNeg.C time, Num time, Eq body) =>
   NonEmptyList time body -> Bool
catMaybesRInitInfinite xs =
   let t =
         min 100 $
         MixedTimeList.switchTimeL const $
         TimeTimeList.catMaybesR $
         TimeTimeList.mapBody (const (Nothing::Maybe())) $
         TimeTimeList.mapTime (NonNegChunky.fromNumber . (add 1)) $
         makeInfiniteEventList xs
   in  t == t


partitionMaybe :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> TimeTimeList.T time body -> Bool
partitionMaybe p xs =
   TimeTimeList.partitionMaybe (\x -> toMaybe (p x) x) xs ==
   TimeTimeList.partition p xs

partitionMaybeR :: (NonNeg.C time, Eq body0, Eq body1) =>
   (body0 -> Maybe body1) -> TimeTimeList.T time body0 -> Bool
partitionMaybeR f xs =
   TimeTimeList.partitionMaybe f xs ==
   TimeTimeList.partitionMaybeR f xs

partitionMaybeRInfinite ::
   (NonNeg.C time, Num time, Eq body0, Eq body1) =>
   (body0 -> Maybe body1) ->
   NonEmptyList time body0 -> Bool
partitionMaybeRInfinite f xs =
   {-
   @(add 1) is needed in order to assert that the accumulated time is infinite
   and can be clipped by @min 100@.
   -}
   let timeSum =
         min 100 .
         List.foldr add zero .
         TimeTimeList.getTimes
       t =
         mapPair (timeSum, timeSum) $
         TimeTimeList.partitionMaybeR f $
         TimeTimeList.mapTime (NonNegChunky.fromNumber . add 1) $
         makeInfiniteEventList xs
   in  t == t

partitionMaybeRInitInfinite ::
   (NonNeg.C time, Num time, Eq body0, Eq body1) =>
   (body0 -> Maybe body1) ->
   NonEmptyList time body0 -> Bool
partitionMaybeRInitInfinite f xs =
   let initTime =
         min 100 .
         MixedTimeList.switchTimeL const
       t =
         mapPair (initTime, initTime) $
         TimeTimeList.partitionMaybeR f $
         TimeTimeList.mapTime (NonNegChunky.fromNumber . add 1) $
         makeInfiniteEventList xs
   in  t == t


{- |
'TimeTimeList.merge' preserves normalization of its operands.
-}
mergeNormalize :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeNormalize xs0 ys0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
   in  isNormalized $ TimeTimeList.merge xs ys

mergeLeftIdentity :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> Bool
mergeLeftIdentity xs =
   TimeTimeList.merge (TimeTimeList.pause zero) xs  ==  xs

mergeRightIdentity :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> Bool
mergeRightIdentity xs =
   TimeTimeList.merge xs (TimeTimeList.pause zero)  ==  xs

mergeCommutative :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeCommutative xs0 ys0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
   in  TimeTimeList.merge xs ys  ==  TimeTimeList.merge ys xs
{-
merge commutative: Falsifiable, after 8 tests:
3 ./ '!' /. 0 ./ ' ' /. 1 ./ ' ' /. 2 ./ empty
3 ./ '!' /. 3 ./ '!' /. 1 ./ empty
-}

mergeAssociative :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeAssociative xs0 ys0 zs0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
       zs = TimeTimeList.normalize zs0
   in  TimeTimeList.merge xs (TimeTimeList.merge ys zs)  ==
       TimeTimeList.merge (TimeTimeList.merge xs ys) zs

{-
Prior normalization is not enough,
because 'append' does not preserve normalization
if the first list ends with time difference 0
and the second one starts with time difference 0.

Without posterior normalization you get

merge append: Falsifiable, after 30 tests:
1 ./ 'a' /. 0 ./ empty
1 ./ ' ' /. 1 ./ empty
0 ./ ' ' /. 1 ./ empty

-}
mergeAppend :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeAppend xs ys zs =
   TimeTimeList.normalize (TimeTimeList.append xs (TimeTimeList.merge ys zs))  ==
   TimeTimeList.normalize
      (TimeTimeList.merge (TimeTimeList.append xs ys)
          (TimeTimeList.delay (TimeTimeList.duration xs) zs))

appendByMerge :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
appendByMerge xs ys =
   TimeTimeList.normalize (TimeTimeList.append xs ys)  ==
   TimeTimeList.normalize (TimeTimeList.merge xs
      (TimeTimeList.delay (TimeTimeList.duration xs) ys))

{-
Normalization is important, otherwise the following counter-examples exist:

merge associative: Falsifiable, after 99 tests:
0 ./ '\DEL' /. 2 ./ '\DEL' /. 2 ./ empty
0 ./ '\DEL' /. 2 ./ '\DEL' /. 0 ./ '~' /. 3 ./ empty
2 ./ ' ' /. 2 ./ '\DEL' /. 3 ./ empty

merge associative: Falsifiable, after 99 tests:
6 ./ '~' /. 2 ./ '%' /. 1 ./ '#' /. 3 ./ '$' /. 2 ./ empty
6 ./ '~' /. 0 ./ '"' /. 2 ./ '{' /. 0 ./ '"' /. 6 ./ empty
0 ./ '{' /. 5 ./ '$' /. 3 ./ empty

merge associative: Falsifiable, after 41 tests:
2 ./ '~' /. 0 ./ empty
2 ./ '~' /. 0 ./ '$' /. 3 ./ empty
1 ./ '#' /. 4 ./ '"' /. 4 ./ empty
-}

-- does only hold for monotonic functions
-- toUpper and toLower are not monotonic
mergeMap :: (NonNeg.C time, Ord body0 ,Ord body1) =>
   (body0 -> body1) -> TimeTimeList.T time body0 -> TimeTimeList.T time body0 -> Bool
mergeMap f xs0 ys0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
   in  TimeTimeList.mapBody f (TimeTimeList.merge xs ys)  ==
       TimeTimeList.merge (TimeTimeList.mapBody f xs) (TimeTimeList.mapBody f ys)

mergeFilter :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeFilter p xs0 ys0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
   in  TimeTimeList.filter p (TimeTimeList.merge xs ys)  ==
       TimeTimeList.merge (TimeTimeList.filter p xs) (TimeTimeList.filter p ys)

mergePartition :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeTimeList.T time body -> Bool
mergePartition p xs0 =
   let xs = TimeTimeList.normalize xs0
   in  xs  ==  uncurry TimeTimeList.merge (TimeTimeList.partition p xs)

mergeEitherMapMaybe :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
mergeEitherMapMaybe xs0 ys0 =
   let xs = TimeTimeList.normalize xs0
       ys = TimeTimeList.normalize ys0
       zs = TimeTimeList.merge
               (TimeTimeList.mapBody Left xs)
               (TimeTimeList.mapBody Right ys)
       dur = TimeTimeList.duration zs
       longXs = TimeTimeList.pad dur xs
       longYs = TimeTimeList.pad dur ys
   in  longXs  ==  TimeTimeList.mapMaybe (either Just (const Nothing)) zs
       &&
       longYs  ==  TimeTimeList.mapMaybe (either (const Nothing) Just) zs


mergeInfinite :: (NonNeg.C time, Ord body) =>
   NonEmptyList time body ->
   NonEmptyList time body -> Bool
mergeInfinite xs0 ys0 =
   let xs = makeInfiniteEventList xs0
       ys = makeInfiniteEventList ys0
   in  checkInfinite (TimeTimeList.merge xs ys)



insertCommutative :: (NonNeg.C time, Ord body) =>
   (time, body) -> (time, body) -> TimeTimeList.T time body -> Bool
insertCommutative (time0, body0) (time1, body1) evs =
   TimeTimeList.insert time0 body0 (TimeTimeList.insert time1 body1 evs)  ==
   TimeTimeList.insert time1 body1 (TimeTimeList.insert time0 body0 evs)

{-
Normalization is important, otherwise we have the counterexample:
Relative.TimeEnd.insertMerge: Falsifiable, after 6 tests:
2
'~'
0 /. '"' ./ 2 /. '~' ./ 0 /. '#' ./ 1 /. empty
-}
insertMerge :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeTimeList.T time body -> Bool
insertMerge time body evs0 =
   let evs = TimeTimeList.normalize evs0
   in  TimeTimeList.insert time body evs  ==
       TimeTimeList.merge (TimeTimeList.cons time body $ TimeTimeList.pause zero) evs

insertNormalize :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeTimeList.T time body -> Bool
insertNormalize time body =
   isNormalized . TimeTimeList.insert time body . TimeTimeList.normalize

insertSplitAtTime :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeTimeList.T time body -> Bool
insertSplitAtTime time body evs =
   TimeTimeList.insert
      (min time (TimeTimeList.duration evs)) body
      (TimeTimeList.normalize evs)
   ==
      let (prefix,suffix) = TimeTimeList.splitAtTime time evs
      in  TimeTimeList.normalize
             (TimeTimeList.append prefix (TimeTimeList.cons zero body suffix))
      --  append prefix (MixedTimeList.consBody body suffix)

insertInfinite :: (NonNeg.C time, Ord body) =>
   time -> body -> NonEmptyList time body -> Bool
insertInfinite time body =
   checkInfinite . TimeTimeList.insert time body . makeInfiniteEventList




moveForwardIdentity :: (NonNeg.C time, Num time, Ord body) =>
   TimeTimeList.T time body -> Bool
moveForwardIdentity evs =
   evs ==
   TimeTimeList.moveForward (TimeTimeList.mapBody ((,) zero) evs)

moveForwardAdditive :: (NonNeg.C time, Num time, Ord body) =>
   TimeTimeList.T time ((time,time),body) -> Bool
moveForwardAdditive evs =
   TimeTimeList.normalize (moveForwardLimited (moveForwardLimited
      (TimeTimeList.mapBody (\((t0,t1),b) -> (t0,(t1,b))) evs))) ==
   TimeTimeList.normalize (moveForwardLimited
      (TimeTimeList.mapBody (mapFst (uncurry add)) evs))

moveForwardCommutative :: (NonNeg.C time, Num time, Ord body) =>
   TimeTimeList.T time ((time,time),body) -> Bool
moveForwardCommutative evs =
   TimeTimeList.normalize (moveForwardLimited (moveForwardLimited
      (TimeTimeList.mapBody (\((t0,t1),b) -> (t0,(t1,b))) evs))) ==
   TimeTimeList.normalize (moveForwardLimited (moveForwardLimited
      (TimeTimeList.mapBody (\((t0,t1),b) -> (t1,(t0,b))) evs)))

moveForwardRestricted :: (NonNeg.C time, Num time, Ord body) =>
   time -> TimeTimeList.T time (time,body) -> Bool
moveForwardRestricted maxTime evs0 =
   let evs =
          TimeTimeList.mapBody (mapFst (min maxTime)) $
          restrictMoveTimes (TimeTimeList.normalize evs0)
       mevs = TimeTimeList.moveForward evs
   in      mevs  ==  TimeTimeList.moveForwardRestrictedBy (\_ _ -> True) maxTime evs
       &&  mevs  ==  TimeTimeList.moveForwardRestrictedByStrict (\_ _ -> True) maxTime evs
       &&  mevs  ==  TimeTimeList.moveForwardRestrictedByQueue (\_ _ -> False) maxTime evs

moveForwardRestrictedInfinity ::
   (NonNeg.C time, Num time, Ord body) =>
   time -> NonEmptyList time (time,body) -> Bool
moveForwardRestrictedInfinity maxTime =
   checkInfinite .
   TimeTimeList.moveForwardRestricted maxTime .
   TimeTimeList.mapBody (mapFst (min maxTime)) .
   restrictMoveTimes .
   makeUncollapsedInfiniteEventList



moveForwardLimited :: (NonNeg.C time, Num time) =>
   TimeTimeList.T time (time,body) -> TimeTimeList.T time body
moveForwardLimited = TimeTimeList.moveForward . restrictMoveTimes

restrictMoveTimes :: (NonNeg.C time) =>
   TimeTimeList.T time (time,body) -> TimeTimeList.T time (time,body)
restrictMoveTimes =
   flip evalState zero .
   TimeTimeList.mapM
      (\t -> modify (add t) >> return t)
      (\(t,b) -> gets (\tm -> (min t tm, b)))


arrangeSingletons :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time body -> Bool
arrangeSingletons evs =
   evs ==
   TimeTimeList.arrange
      (TimeTimeList.mapBody
          (\x -> TimeTimeList.cons zero x (TimeTimeList.pause zero)) evs)

arrangeDelay :: (NonNeg.C time, Ord body) =>
   time -> NonEmptyList time body -> Bool
arrangeDelay delay evs0 =
   let evs = makeNonEmptyEventList evs0
   in  TimeTimeList.delay delay evs ==
       TimeTimeList.arrange
          (TimeTimeList.mapBody
              (\x -> TimeTimeList.cons delay x (TimeTimeList.pause zero)) $
           TimeTimePriv.mapTimeLast (add delay) evs)

arrangeSimple :: (NonNeg.C time, Ord body) =>
   TimeTimeList.T time (TimeTimeList.T time body) ->
   Bool
arrangeSimple evs =
   TimeTimeList.normalize (TimeTimeList.arrange evs) ==
   -- implementation not lazy enough for regular use
   TimeTimeList.foldr
      (TimeTimeList.delay)
      (TimeTimeList.merge)
      (TimeTimeList.pause zero)
      (TimeTimeList.mapBody TimeTimeList.normalize evs)

arrangeAbsolute :: (NonNeg.C time, Num time, Ord body) =>
   TimeTimeList.T time (TimeTimeList.T time body) ->
   Bool
arrangeAbsolute evs =
   TimeTimeList.normalize (TimeTimeList.arrange evs) ==
   AbsTimeTimeList.foldr
      (\t (xs,ys) ->
          TimeTimeList.merge
             (TimeTimeList.delay t (TimeTimeList.normalize xs)) ys)
      (,)
      (TimeTimeList.pause zero, TimeTimeList.pause zero)
      (TimeTimeList.toAbsoluteEventList zero evs)

arrangeInfinity :: (NonNeg.C time, Num time, Ord body) =>
   NonEmptyList time (NonEmptyList time body) -> Bool
arrangeInfinity =
   checkInfinite .
   TimeTimeList.arrange .
   TimeTimeList.mapBody makeUncollapsedInfiniteEventList .
   makeUncollapsedInfiniteEventList




coincidentFlatten :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
coincidentFlatten xs =
   xs  ==  TimeTimeList.flatten (TimeTimeList.collectCoincident xs)

collectCoincidentGaps :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
collectCoincidentGaps xs =
   let times = tail (TimeTimeList.getTimes (TimeTimeList.collectCoincident xs))
   in  null times || all (zero<) (init times)

collectCoincidentNonEmpty :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
collectCoincidentNonEmpty =
   all (not . null) . TimeTimeList.getBodies . TimeTimeList.collectCoincident

collectCoincidentInfinite :: (NonNeg.C time, Num time, Eq body) =>
   NonEmptyList time body -> Bool
collectCoincidentInfinite =
   checkInfinite .
   TimeTimeList.collectCoincident .
   makeUncollapsedInfiniteEventList


mapCoincidentMap :: (NonNeg.C time, Eq body1) =>
   (body0 -> body1) -> TimeTimeList.T time body0 -> Bool
mapCoincidentMap f xs =
   TimeTimeList.mapBody f xs  ==
   TimeTimeList.mapCoincident (map f) xs

mapCoincidentComposition :: (NonNeg.C time, Eq body2) =>
   ([body0] -> [body1]) -> ([body1] -> [body2]) -> TimeTimeList.T time body0 -> Bool
mapCoincidentComposition f g xs =
   TimeTimeList.mapCoincident (g . f) xs  ==
   (TimeTimeList.mapCoincident g . TimeTimeList.mapCoincident f) xs

mapCoincidentReverse :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> Bool
mapCoincidentReverse xs =
   xs  ==  TimeTimeList.mapCoincident reverse (TimeTimeList.mapCoincident reverse xs)



reverseAppend :: (NonNeg.C time, Eq body) =>
   TimeTimeList.T time body -> TimeTimeList.T time body -> Bool
reverseAppend xs ys =
   TimeTimeList.reverse (TimeTimeList.append xs ys)  ==
   TimeTimeList.append (TimeTimeList.reverse ys) (TimeTimeList.reverse xs)


mapBodyMAppend ::
   (Monad m, Eq body1, NonNeg.C time) =>
   (m (TimeTimeList.T time body1) -> TimeTimeList.T time body1) ->
   (body0 -> m body1) -> TimeTimeList.T time body0 -> TimeTimeList.T time body0 -> Bool
mapBodyMAppend run f xs ys =
   run (TimeTimeList.mapM return f (TimeTimeList.append xs ys))  ==
   run (liftM2 TimeTimeList.append (TimeTimeList.mapM return f xs) (TimeTimeList.mapM return f ys))

mapBodyMAppendRandom ::
   (Random body, NonNeg.C time, Eq body) =>
   Int -> TimeTimeList.T time (body,body) -> TimeTimeList.T time (body,body) -> Bool
mapBodyMAppendRandom seed =
   mapBodyMAppend
      (flip evalState (mkStdGen seed))
      (state . randomR)


mapBodyMInfinite ::
   (Random body, NonNeg.C time, Eq body) =>
   Int -> NonEmptyList time (body,body) -> Bool
mapBodyMInfinite seed =
   checkInfinite .
   flip evalState (mkStdGen seed) .
   TimeTimeList.mapM return (state . randomR) .
   makeInfiniteEventList


{-

mapM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   TimeTimeList.T time0 body0 -> m (TimeTimeList.T time1 body1)
mapM timeAction bodyAction =
   Uniform.mapM bodyAction timeAction

mapImmM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   Immediate time0 body0 -> m (Immediate time1 body1)
mapImmM timeAction bodyAction =
   Disp.mapM bodyAction timeAction


getBodies :: TimeTimeList.T time body -> [body]
getBodies = Uniform.getFirsts

getTimes :: TimeTimeList.T time body -> [time]
getTimes = Uniform.getSeconds


empty :: Immediate time body
empty = Disp.empty


cons :: time -> body -> TimeTimeList.T time body -> TimeTimeList.T time body
cons = Uniform.cons


snoc :: TimeTimeList.T time body -> body -> time -> TimeTimeList.T time body
snoc = Uniform.snoc


{-
propInsertPadded :: Event time body -> TimeTimeList.T time body -> Bool
propInsertPadded (Event time body) evs =
   EventList.insert time body (fst evs)  ==  fst (insert time body evs)
-}

appendSingle :: -- (Num time, Ord time, Ord body) =>
   body -> TimeTimeList.T time body -> EventList.T time body
appendSingle body xs =
   Disp.foldr EventList.consTime EventList.consBody EventList.empty $
   Uniform.snocFirst xs body

fromEventList :: time -> EventList.T time body -> TimeTimeList.T time body
fromEventList t =
   EventList.foldr consTime consBody (pause t)

toEventList :: TimeTimeList.T time body -> EventList.T time body
toEventList xs =
   zipWith EventList.Event (getTimes xs) (getBodies xs)

{- |

-}


discretize :: (RealFrac time, Integral i) =>
   TimeTimeList.T time body -> TimeTimeList.T i body
discretize es =
   evalState (Uniform.mapSecondM roundDiff es) zero

resample :: (RealFrac time, Integral i) =>
   time -> TimeTimeList.T time body -> TimeTimeList.T i body
resample rate es =
   discretize (mapTime (rate*) es)
-}

resampleInfinite :: (Eq body) =>
   TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) body -> Bool
resampleInfinite rateInt =
   let rate = timeToDouble rateInt + 1
   in  checkInfinite . intTimeList . TimeTimeList.resample rate .
       makeInfiniteEventList .
       mapPair (mapFst makeFracTime, TimeTimeList.mapTime makeFracTime)


{-
toAbsoluteEventList :: (Num time) =>
   time -> TimeTimeList.T time body -> AbsoluteEventList.T time body
toAbsoluteEventList start xs =
   let ts = Uniform.getSeconds xs
       bs = Uniform.getFirsts  xs
       ats = List.scanl add start ts
   in  maybe
          (error "padded list always contains one time value")
          (\ ~(ats0,lt) -> (zip ats0 bs, lt))
          (viewR ats)
-}




type NonEmptyList time body = ((time, body), TimeTimeList.T time body)

makeUncollapsedInfiniteEventList ::
   (NonNeg.C time, Num time) =>
   NonEmptyList time body -> TimeTimeList.T time body
makeUncollapsedInfiniteEventList =
   makeInfiniteEventList .
   mapFst (\(time,body) -> (add time 1, body))

makeInfiniteEventList :: (NonNeg.C time) =>
   NonEmptyList time body -> TimeTimeList.T time body
makeInfiniteEventList =
   TimeTimeList.cycle . makeNonEmptyEventList

makeNonEmptyEventList :: (NonNeg.C time) =>
   NonEmptyList time body -> TimeTimeList.T time body
makeNonEmptyEventList ((t, b), evs) =
   TimeTimeList.cons t b evs

{- |
Pick an arbitrary element from an infinite list
and check if it can be evaluated.
-}
checkInfinite :: (Eq time, Eq body) =>
   TimeTimeList.T time body -> Bool
checkInfinite xs0 =
   let (x,xs) = MixedTimeList.viewTimeL (lift (Mixed.dropUniform 100) xs0)
       y = MixedTimeList.switchBodyL
              (error "checkInfinite: finite list")
              const xs
   in  x == x && y == y



tests :: [(String, IO ())]
tests =
   ("viewTimeL consTime",
     quickCheck (viewLConsTime :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("viewBodyL consBody",
     quickCheck (viewLConsBody :: BodyTimeList.T TimeDiff ArbChar -> Bool)) :
   ("viewTimeR snocTime",
     quickCheck (viewRSnocTime :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("viewBodyR snocBody",
     quickCheck (viewRSnocBody :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("viewLInfinite",
     quickCheck (viewLInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("viewRInfinite",
     quickCheck (viewRInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("switchTimeL consTime",
     quickCheck (switchLConsTime :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("switchBodyL consBody",
     quickCheck (switchLConsBody :: BodyTimeList.T TimeDiff ArbChar -> Bool)) :
   ("switchTimeR snocTime",
     quickCheck (switchRSnocTime :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("switchBodyR snocBody",
     quickCheck (switchRSnocBody :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("switchLInfinite",
     quickCheck (switchLInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("switchRInfinite",
     quickCheck (switchRInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("consInfinite",
     quickCheck (consInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("consTimeBodyInfinite",
     quickCheck (consTimeBodyInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("snocInfinite",
     quickCheck (snocInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("snocTimeBodyInfinite",
     quickCheck (snocTimeBodyInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("consInfix",
     quickCheck (consInfix :: TimeDiff -> ArbChar -> TimeDiff -> TimeDiff -> ArbChar -> TimeDiff -> Bool)) :
   ("chunkyShow",
     quickCheck chunkyShow) :
{-
   ("chunkyCheck",
     quickCheck chunkyCheck) :
-}
   ("mapTimeTailChunkyInfinite",
     quickCheck (mapTimeTailChunkyInfinite succ :: BodyTimeList.T (NonNegChunky.T TimeDiff) ArbChar -> Bool)) :


   ("map body composition",
     quickCheck (mapBodyComposition toUpper toLower
               :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("map time composition",
     quickCheck ((\dt0 dt1 -> mapTimeComposition (add dt0) (add dt1))
               :: TimeDiff -> TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("map time body commutative",
     quickCheck ((\dt -> mapTimeBodyCommutative (add dt) toUpper)
               :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :

   ("mapBodyInfinite",
     quickCheck (mapBodyInfinite toUpper
               :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("mapTimeInfinite",
     quickCheck (\dt -> mapTimeInfinite (add dt)
               :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("mapNormalize",
     quickCheck (mapNormalize succ
               :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :

   ("append left identity",
     quickCheck (appendLeftIdentity :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("append right identity",
     quickCheck (appendRightIdentity :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("append associative",
     quickCheck (appendAssociative
              :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar ->
                 TimeTimeList.T TimeDiff ArbChar -> Bool)) :

   ("appendCons",
     quickCheck (appendCons :: TimeDiff -> ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mapBodyAppend",
     quickCheck (mapBodyAppend toUpper
               :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("appendSplitAtTime",
     quickCheck (appendSplitAtTime :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("appendFirstInfinite",
     quickCheck (appendFirstInfinite :: NonEmptyList TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("appendSecondInfinite",
     quickCheck (appendSecondInfinite :: TimeTimeList.T TimeDiff ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("concatNaive",
     quickCheck (concatNaive :: [TimeTimeList.T TimeDiff ArbChar] -> Bool)) :
   ("cycleNaive",
     quickCheck (cycleNaive :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("cycleInfinite",
     quickCheck (cycleInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("decreaseStart delay",
     quickCheck (decreaseStartDelay :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("decreaseStartInfinite",
     quickCheck (decreaseStartInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("delay additive",
     quickCheck (delayAdditive :: TimeDiff -> TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("delay pause",
     quickCheck (delayPause :: TimeDiff -> TimeDiff -> Bool)) :
   ("delay append pause",
     quickCheck (delayAppendPause :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("delayInfinite",
     quickCheck (delayInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("splitAtTakeDropTime",
     quickCheck (splitAtTakeDropTime :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeEndPause",
     quickCheck (takeTimeEndPause :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeAppendFirst",
     quickCheck (takeTimeAppendFirst :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeAppendSecond",
     quickCheck (takeTimeAppendSecond :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeNormalize",
     quickCheck (takeTimeNormalize :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("dropTimeNormalize",
     quickCheck (dropTimeNormalize :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeInfinite",
     quickCheck (takeTimeInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("dropTimeInfinite",
     quickCheck (dropTimeInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("dropTimeLargeInfinite",
     quickCheck (dropTimeLargeInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("splitAtTimeLazyInfinite",
     quickCheck (splitAtTimeLazyInfinite :: BodyTimeList.T (NonNegChunky.T TimeDiff) ArbChar -> Bool)) :
   ("dropTimeLazyInfinite",
     quickCheck (dropTimeLazyInfinite :: BodyTimeList.T (NonNegChunky.T TimeDiff) ArbChar -> Bool)) :

   ("duration pause",
     quickCheck (durationPause :: TimeDiff -> Bool)) :
   ("duration append",
     quickCheck (durationAppend :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("duration merge",
     quickCheck (durationMerge :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("durationTakeTime",
     quickCheck (durationTakeTime :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("durationDropTime",
     quickCheck (durationDropTime :: TimeDiff -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :

   ("filterSatisfy",
     quickCheck (\c -> filterSatisfy (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterProjection",
     quickCheck (\c -> filterProjection (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterCommutative",
     quickCheck (\c0 c1 -> filterCommutative (c0<) (c1>) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterComposition",
     quickCheck (\c0 c1 -> filterComposition (c0<) (c1>) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterNormalize",
     quickCheck (\c -> filterNormalize (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterAppend",
     quickCheck (\c -> filterAppend (c<) :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterDuration",
     quickCheck (\c -> filterDuration (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterPartition",
     quickCheck (\c -> filterPartition (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterInfinite",
     quickCheck (\c -> filterInfinite (c<) :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("catMaybesAppend",
     quickCheck (catMaybesAppend :: TimeTimeList.T TimeDiff (Maybe ArbChar) -> TimeTimeList.T TimeDiff (Maybe ArbChar) -> Bool)) :
   ("catMaybesRInfinite",
     quickCheck (catMaybesRInfinite :: NonEmptyList TimeDiff (Maybe ArbChar) -> Bool)) :
   ("catMaybesRInitInfinite",
     quickCheck (catMaybesRInitInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("partitionMaybe",
     quickCheck (\c -> partitionMaybe (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("partitionMaybeR",
     quickCheck (\c -> partitionMaybeR (\x -> toMaybe (c<x) (fromEnum x)) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("partitionMaybeRInfinite",
     quickCheck (\c -> partitionMaybeRInfinite (\x -> toMaybe (c<x) (fromEnum x)) :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("partitionMaybeRInitInfinite",
     quickCheck (\c -> partitionMaybeRInitInfinite (\x -> toMaybe (c<x) (fromEnum x)) :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("mergeNormalize",
     quickCheck (mergeNormalize :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("merge left identity",
     quickCheck (mergeLeftIdentity :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("merge right identity",
     quickCheck (mergeRightIdentity :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("merge commutative",
     quickCheck (mergeCommutative :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("merge associative",
     quickCheck (mergeAssociative :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("merge append",
     quickCheck (mergeAppend :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("appendByMerge",
     quickCheck (appendByMerge :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mergeMap",
     quickCheck (mergeMap succ :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mergeFilter",
     quickCheck (\c -> mergeFilter (c>)
             :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mergePartition",
     quickCheck (\c -> mergePartition (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mergeEitherMapMaybe",
     quickCheck (mergeEitherMapMaybe
         :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mergeInfinite",
     quickCheck (mergeInfinite
         :: NonEmptyList TimeDiff ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("insertCommutative",
     quickCheck (insertCommutative :: (TimeDiff, ArbChar) -> (TimeDiff, ArbChar) -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("insertMerge",
     quickCheck (insertMerge :: TimeDiff -> ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("insertNormalize",
     quickCheck (insertNormalize :: TimeDiff -> ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("insertSplitAtTime",
     quickCheck (insertSplitAtTime :: TimeDiff -> ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("insertInfinite",
     quickCheck (insertInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("moveForwardIdentity",
     quickCheck (moveForwardIdentity :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("moveForwardAdditive",
     quickCheck (moveForwardAdditive :: TimeTimeList.T TimeDiff ((TimeDiff,TimeDiff),ArbChar) -> Bool)) :
   ("moveForwardCommutative",
     quickCheck (moveForwardCommutative :: TimeTimeList.T TimeDiff ((TimeDiff,TimeDiff),ArbChar) -> Bool)) :
   ("moveForwardRestricted",
     quickCheck (moveForwardRestricted :: TimeDiff -> TimeTimeList.T TimeDiff (TimeDiff,ArbChar) -> Bool)) :
   ("moveForwardRestrictedInfinity",
     quickCheck (moveForwardRestrictedInfinity :: TimeDiff -> NonEmptyList TimeDiff (TimeDiff,ArbChar) -> Bool)) :

   ("arrangeSingletons",
     quickCheck (arrangeSingletons ::
        TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("arrangeDelay",
     quickCheck (arrangeDelay ::
        TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("arrangeSimple",
     quickCheck (arrangeSimple ::
        TimeTimeList.T TimeDiff (TimeTimeList.T TimeDiff ArbChar) -> Bool)) :
   ("arrangeAbsolute",
     quickCheck (arrangeAbsolute ::
        TimeTimeList.T TimeDiff (TimeTimeList.T TimeDiff ArbChar) -> Bool)) :
   ("arrangeInfinity",
     quickCheck (arrangeInfinity ::
        NonEmptyList TimeDiff (NonEmptyList TimeDiff ArbChar) -> Bool)) :

   ("coincidentFlatten",
     quickCheck (coincidentFlatten :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentGaps",
     quickCheck (collectCoincidentGaps :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentNonEmpty",
     quickCheck (collectCoincidentNonEmpty :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentInfinite",
     quickCheck (collectCoincidentInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("mapCoincidentMap",
     quickCheck (mapCoincidentMap toUpper :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mapCoincidentComposition",
     quickCheck (mapCoincidentComposition reverse reverse :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("mapCoincidentReverse",
     quickCheck (mapCoincidentReverse :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("reverseAppend",
     quickCheck (reverseAppend :: TimeTimeList.T TimeDiff ArbChar -> TimeTimeList.T TimeDiff ArbChar -> Bool)) :

   ("mapBodyMAppendRandom",
     quickCheck (mapBodyMAppendRandom :: Int -> TimeTimeList.T TimeDiff (ArbChar,ArbChar) -> TimeTimeList.T TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("mapBodyMInfinite",
     quickCheck (mapBodyMInfinite :: Int -> NonEmptyList TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("resampleInfinite",
     quickCheck (resampleInfinite :: TimeDiff -> NonEmptyList (TimeDiff,TimeDiff) ArbChar -> Bool)) :

   []
