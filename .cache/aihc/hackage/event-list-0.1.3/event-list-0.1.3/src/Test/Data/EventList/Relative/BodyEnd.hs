{- |
Copyright   :  (c) Henning Thielemann 2008

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Test.Data.EventList.Relative.BodyEnd (tests) where

import Test.Utility
import Test.QuickCheck (quickCheck)

import qualified Data.EventList.Relative.TimeBody as TimeBodyList
import qualified Data.EventList.Relative.TimeTime as TimeTimeList
import qualified Data.EventList.Relative.TimeMixed as TimeMixedList
import qualified Data.EventList.Relative.MixedBody as MixedBodyList
import qualified Data.EventList.Relative.BodyBody as BodyBodyList
import qualified Data.AlternatingList.List.Disparate as Disp

import qualified Data.EventList.Relative.TimeBodyPrivate as TimeBodyPriv

import Data.EventList.Relative.MixedBody ((/.), (./), empty)

import qualified Numeric.NonNegative.Chunky as NonNegChunky
import qualified Numeric.NonNegative.Class as NonNeg
import Numeric.NonNegative.Class ((-|), zero, add, )
import Data.EventList.Relative.TimeBody (isNormalized)

import Data.Tuple.HT (mapFst, mapPair, )
import Data.Bool.HT (implies, )
import System.Random (Random, randomR, mkStdGen, )
import Control.Monad.Trans.State (state, evalState, gets, modify, )
import Control.Monad (liftM2)
import Data.Maybe (isJust)



viewLConsTime :: (Eq body, Eq time) =>
   TimeBodyList.T time body -> Bool
viewLConsTime xs =
   xs == maybe TimeBodyList.empty (uncurry MixedBodyList.consTime) (MixedBodyList.viewTimeL xs)

viewLConsBody :: (Eq body, Eq time) =>
   BodyBodyList.T time body -> Bool
viewLConsBody xs =
   xs == uncurry MixedBodyList.consBody (MixedBodyList.viewBodyL xs)


viewLInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> Bool
viewLInfinite =
   checkInfinite .
   maybe (error "viewBodyL: empty list") snd .
   TimeBodyList.viewL .
   makeInfiniteEventList


switchLConsTime :: (Eq body, Eq time) =>
   TimeBodyList.T time body -> Bool
switchLConsTime xs =
   xs == MixedBodyList.switchTimeL TimeBodyList.empty MixedBodyList.consTime xs

switchLConsBody :: (Eq body, Eq time) =>
   BodyBodyList.T time body -> Bool
switchLConsBody xs =
   xs == MixedBodyList.switchBodyL MixedBodyList.consBody xs


switchLInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> Bool
switchLInfinite =
   checkInfinite .
   TimeBodyList.switchL (error "switchBodyL: empty list") (flip const) .
   makeInfiniteEventList


consInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
consInfinite time body =
   checkInfinite .
   TimeBodyList.cons time body .
   makeInfiniteEventList

consTimeBodyInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
consTimeBodyInfinite time body =
   checkInfinite .
   MixedBodyList.consTime time .
   MixedBodyList.consBody body .
   makeInfiniteEventList


snocInfinite :: (NonNeg.C time, Eq body) =>
   time -> body -> NonEmptyList time body -> Bool
snocInfinite time body =
   checkInfinite .
   flip (flip TimeBodyList.snoc time) body .
   makeInfiniteEventList


consInfix :: (NonNeg.C time, Eq body) =>
   time -> body -> time -> body -> Bool
consInfix t0 b0 t1 b1 =
   TimeBodyList.append (t0 /. b0 ./ empty) (t1 /. b1 ./ empty)
      == (t0 /. b0 ./ t1 /. b1 ./ empty)


mapBodyComposition :: (Eq body2, Eq time) =>
   (body0 -> body1) -> (body1 -> body2) -> TimeBodyList.T time body0 -> Bool
mapBodyComposition f g evs =
   TimeBodyList.mapBody (g . f) evs  ==
   TimeBodyList.mapBody g (TimeBodyList.mapBody f evs)

mapTimeComposition :: (Eq body, Eq time2) =>
   (time0 -> time1) -> (time1 -> time2) -> TimeBodyList.T time0 body -> Bool
mapTimeComposition f g evs =
   TimeBodyList.mapTime (g . f) evs  ==
   TimeBodyList.mapTime g (TimeBodyList.mapTime f evs)


mapTimeBodyCommutative :: (Eq body1, Eq time1) =>
   (time0 -> time1) -> (body0 -> body1) -> TimeBodyList.T time0 body0 -> Bool
mapTimeBodyCommutative f g evs =
   TimeBodyList.mapBody g (TimeBodyList.mapTime f evs)  ==
   TimeBodyList.mapTime f (TimeBodyList.mapBody g evs)



mapBodyInfinite :: (NonNeg.C time, Eq body1) =>
   (body0 -> body1) -> NonEmptyList time body0 -> Bool
mapBodyInfinite f =
   checkInfinite . TimeBodyList.mapBody f . makeInfiniteEventList

mapTimeInfinite :: (NonNeg.C time0, Eq time1, Eq body) =>
   (time0 -> time1) -> NonEmptyList time0 body -> Bool
mapTimeInfinite f =
   checkInfinite . TimeBodyList.mapTime f . makeInfiniteEventList



{- |
Does only hold for monotonic functions.
-}
mapNormalize :: (NonNeg.C time, Ord body0, Ord body1) =>
   (body0 -> body1) -> TimeBodyList.T time body0 -> Bool
mapNormalize f =
   isNormalized . TimeBodyList.mapBody f . TimeBodyList.normalize



appendLeftIdentity :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
appendLeftIdentity xs =
   TimeBodyList.append TimeBodyList.empty xs  ==  xs

appendRightIdentity :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
appendRightIdentity xs =
   TimeBodyList.append xs TimeBodyList.empty  ==  xs

appendAssociative :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
appendAssociative xs ys zs =
   TimeBodyList.append xs (TimeBodyList.append ys zs)  ==
   TimeBodyList.append (TimeBodyList.append xs ys) zs

appendCons :: (NonNeg.C time, Eq body) =>
   time -> body -> TimeBodyList.T time body -> Bool
appendCons time body xs =
   TimeBodyList.cons time body xs  ==
   TimeBodyList.append (TimeBodyList.cons time body TimeBodyList.empty) xs

appendSplitAtTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
appendSplitAtTime t xs =
   xs == uncurry TimeMixedList.appendBodyEnd (TimeMixedList.splitAtTime t xs)

appendSplitAfterTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
appendSplitAfterTime t xs =
   xs == uncurry TimeMixedList.appendBodyEnd (TimeMixedList.splitAfterTime t xs)

nonZeroTakeTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
nonZeroTakeTime t xs =
   NonNeg.zero < t && t < TimeBodyList.duration xs
   `implies`
   (TimeMixedList.switchTimeR (\_ t0 -> t0 > NonNeg.zero) $
      TimeMixedList.takeTime t xs)

nonZeroDropAfterTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
nonZeroDropAfterTime t xs =
   MixedBodyList.switchTimeL True (\t0 _ -> t0 > NonNeg.zero) $
      TimeMixedList.dropAfterTime t xs

mapBodyAppend :: (Eq body1, NonNeg.C time) =>
   (body0 -> body1) -> TimeBodyList.T time body0 -> TimeBodyList.T time body0 -> Bool
mapBodyAppend f xs ys =
   TimeBodyList.mapBody f (TimeBodyList.append xs ys)  ==
   TimeBodyList.append (TimeBodyList.mapBody f xs) (TimeBodyList.mapBody f ys)


appendFirstInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> TimeBodyList.T time body -> Bool
appendFirstInfinite xs =
   checkInfinite . TimeBodyList.append (makeInfiniteEventList xs)

appendSecondInfinite :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> NonEmptyList time body -> Bool
appendSecondInfinite xs =
   checkInfinite . TimeBodyList.append xs . makeInfiniteEventList


decreaseStartDelay :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
decreaseStartDelay dif xs =
   xs == TimeBodyList.decreaseStart dif (TimeBodyList.delay dif xs)

decreaseStartInfinite :: (NonNeg.C time, Eq body) =>
   time -> NonEmptyList time body -> Bool
decreaseStartInfinite dif =
   checkInfinite .
   TimeBodyList.decreaseStart dif .
   TimeBodyList.delay dif .
   makeInfiniteEventList

delayAdditive :: (NonNeg.C time, Eq body) =>
   time -> time -> TimeBodyList.T time body -> Bool
delayAdditive dif0 dif1 xs =
   TimeBodyList.delay (add dif0 dif1) xs ==
   TimeBodyList.delay dif0 (TimeBodyList.delay dif1 xs)

delayAppendPause :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
delayAppendPause dif xs =
   TimeBodyList.delay dif xs == TimeMixedList.appendBodyEnd (TimeTimeList.pause dif) xs

delayInfinite :: (NonNeg.C time, Eq body) =>
   time -> NonEmptyList time body -> Bool
delayInfinite dif =
   checkInfinite .
   TimeBodyList.delay dif .
   makeInfiniteEventList



splitAtTakeDropTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
splitAtTakeDropTime t xs =
   (TimeMixedList.takeTime t xs, TimeMixedList.dropTime t xs) ==
   TimeMixedList.splitAtTime t xs

takeTimeEndPause :: (NonNeg.C time, Ord body) =>
   time -> TimeBodyList.T time body -> Bool
takeTimeEndPause t xs =
   t == zero ||
   t >= TimeBodyList.duration xs ||
   zero <  snd (TimeMixedList.viewTimeR (TimeMixedList.takeTime t xs))

takeTimeAppendFirst :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
takeTimeAppendFirst t xs ys =
   TimeMixedList.takeTime t (TimeBodyList.append xs ys) ==
   TimeTimeList.append
      (TimeMixedList.takeTime t xs)
      (TimeMixedList.takeTime (t -| TimeBodyList.duration xs) ys)

takeTimeAppendSecond :: (NonNeg.C time, Num time, Eq body) =>
   time -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
takeTimeAppendSecond t xs ys0 =
   -- the second list must not start with a zero pause
   let ys = TimeBodyList.delay 1 ys0
       t1 = add t 1
   in  TimeMixedList.takeTime (TimeBodyList.duration xs + t1) (TimeBodyList.append xs ys) ==
       TimeMixedList.prependBodyEnd xs (TimeMixedList.takeTime t1 ys)

takeTimeNormalize :: (NonNeg.C time, Ord body) =>
   time -> TimeBodyList.T time body -> Bool
takeTimeNormalize t =
   TimeTimeList.isNormalized . TimeMixedList.takeTime t . TimeBodyList.normalize

dropTimeNormalize :: (NonNeg.C time, Ord body) =>
   time -> TimeBodyList.T time body -> Bool
dropTimeNormalize t =
   isNormalized . TimeMixedList.dropTime t . TimeBodyList.normalize

takeTimeInfinite :: (NonNeg.C time, Num time, Ord body) =>
   time -> NonEmptyList time body -> Bool
takeTimeInfinite t =
   (t == ) . TimeTimeList.duration .
   TimeMixedList.takeTime t . makeUncollapsedInfiniteEventList

dropTimeInfinite :: (NonNeg.C time, Num time, Ord body) =>
   time -> NonEmptyList time body -> Bool
dropTimeInfinite t =
   checkInfinite . TimeMixedList.dropTime t . makeUncollapsedInfiniteEventList

_splitAtTimeLazyInfinite ::
   (NonNeg.C time, Num time, Ord body, Show time, Show body) =>
   BodyBodyList.T (NonNegChunky.T time) body -> Bool
_splitAtTimeLazyInfinite =
   not . null . show . snd .
   TimeMixedList.splitAtTime 1000000 .
   MixedBodyList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)

_dropTimeLazyInfinite ::
   (NonNeg.C time, Num time, Ord body, Show time, Show body) =>
   BodyBodyList.T (NonNegChunky.T time) body -> Bool
_dropTimeLazyInfinite =
   not . null . show .
   TimeMixedList.dropTime 1000000 .
   MixedBodyList.consTime
      (NonNegChunky.fromChunks $ iterate (2-) 1)



durationPause :: (NonNeg.C time) =>
   time -> Bool
durationPause t =
   t == TimeBodyList.duration (TimeBodyList.singleton t (error "durationPause: no need to access body"))

durationAppend :: (NonNeg.C time, Num time) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
durationAppend xs ys =
   TimeBodyList.duration (TimeBodyList.append xs ys)  ==
   TimeBodyList.duration xs + TimeBodyList.duration ys

durationMerge :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
durationMerge xs ys =
   TimeBodyList.duration (TimeBodyList.merge xs ys)  ==
   max (TimeBodyList.duration xs) (TimeBodyList.duration ys)

durationTakeTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
durationTakeTime t xs =
   min (TimeBodyList.duration xs) t ==
   TimeTimeList.duration (TimeMixedList.takeTime t xs)

durationDropTime :: (NonNeg.C time, Eq body) =>
   time -> TimeBodyList.T time body -> Bool
durationDropTime t xs =
   TimeBodyList.duration xs -| t ==
   TimeBodyList.duration (TimeMixedList.dropTime t xs)



equalPrefix :: (Eq time, Eq body) =>
   Int -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
equalPrefix n xs ys =
   TimeBodyPriv.lift (Disp.take n) xs ==
   TimeBodyPriv.lift (Disp.take n) ys

cycleInfinite :: (NonNeg.C time, Eq body) =>
   NonEmptyList time body -> Bool
cycleInfinite xs0 =
   let xs = makeInfiniteEventList xs0
   in  equalPrefix 100 xs (TimeBodyList.cycle xs)


filterSatisfy :: (NonNeg.C time) =>
   (body -> Bool) ->
   TimeBodyList.T time body -> Bool
filterSatisfy p =
   all p . TimeBodyList.getBodies . TimeBodyList.filter p

filterProjection :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   TimeBodyList.T time body -> Bool
filterProjection p xs =
   TimeBodyList.filter p xs ==
   TimeBodyList.filter p (TimeBodyList.filter p xs)

filterCommutative :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   (body -> Bool) ->
   TimeBodyList.T time body -> Bool
filterCommutative p q xs =
   TimeBodyList.filter p (TimeBodyList.filter q xs) ==
   TimeBodyList.filter q (TimeBodyList.filter p xs)

filterComposition :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   (body -> Bool) ->
   TimeBodyList.T time body -> Bool
filterComposition p q xs =
   TimeBodyList.filter p (TimeBodyList.filter q xs) ==
   TimeBodyList.filter (\b -> p b && q b) xs

filterNormalize :: (NonNeg.C time, Ord body) =>
   (body -> Bool) ->
   TimeBodyList.T time body -> Bool
filterNormalize p =
   isNormalized . TimeBodyList.filter p . TimeBodyList.normalize

filterAppend :: (NonNeg.C time, Eq body) =>
   (body -> Bool) ->
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
filterAppend p xs0 ys =
   let xs = TimeBodyList.filter p xs0
   in  TimeBodyList.filter p (TimeBodyList.append xs ys)  ==
       TimeBodyList.append xs (TimeBodyList.filter p ys)

filterDuration :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> TimeTimeList.T time body -> Bool
filterDuration p xs =
   TimeTimeList.duration xs >= TimeTimeList.duration (TimeTimeList.filter p xs)

filterPartition :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeBodyList.T time body -> Bool
filterPartition p xs =
   (TimeBodyList.filter p xs, TimeBodyList.filter (not . p) xs) ==
   TimeBodyList.partition p xs


filterInfinite :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> NonEmptyList time body -> Bool
filterInfinite p xs =
   null (TimeBodyList.getBodies (TimeBodyList.filter p (makeNonEmptyEventList xs)))
   ||
   (checkInfinite .
    TimeBodyList.filter p .
    makeInfiniteEventList) xs

catMaybesAppend :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time (Maybe body) -> TimeBodyList.T time (Maybe body) -> Bool
catMaybesAppend xs0 ys =
   let xs = TimeBodyList.filter isJust xs0
   in  TimeBodyList.catMaybes (TimeBodyList.append xs ys)  ==
       TimeBodyList.append (TimeBodyList.catMaybes xs) (TimeBodyList.catMaybes ys)


{- |
'TimeBodyList.merge' preserves normalization of its operands.
-}
mergeNormalize :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeNormalize xs0 ys0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
   in  isNormalized $ TimeBodyList.merge xs ys

mergeLeftIdentity :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> Bool
mergeLeftIdentity xs =
   TimeBodyList.merge TimeBodyList.empty xs  ==  xs

mergeRightIdentity :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> Bool
mergeRightIdentity xs =
   TimeBodyList.merge xs TimeBodyList.empty  ==  xs

mergeCommutative :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeCommutative xs0 ys0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
   in  TimeBodyList.merge xs ys  ==  TimeBodyList.merge ys xs

mergeAssociative :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeAssociative xs0 ys0 zs0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
       zs = TimeBodyList.normalize zs0
   in  TimeBodyList.merge xs (TimeBodyList.merge ys zs)  ==
       TimeBodyList.merge (TimeBodyList.merge xs ys) zs

{-
Prior normalization is not enough,
because 'append' does not preserve normalization
if the first list ends with time difference 0
and the second one starts with time difference 0.
-}
mergeAppend :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeAppend xs ys zs =
   TimeBodyList.normalize (TimeBodyList.append xs (TimeBodyList.merge ys zs))  ==
   TimeBodyList.normalize
      (TimeBodyList.merge (TimeBodyList.append xs ys)
          (TimeBodyList.delay (TimeBodyList.duration xs) zs))

{-
Normalization is important

does only hold for monotonic functions
toUpper and toLower are not monotonic
-}

mergeMap :: (NonNeg.C time, Ord body0 ,Ord body1) =>
   (body0 -> body1) -> TimeBodyList.T time body0 -> TimeBodyList.T time body0 -> Bool
mergeMap f xs0 ys0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
   in  TimeBodyList.mapBody f (TimeBodyList.merge xs ys)  ==
       TimeBodyList.merge (TimeBodyList.mapBody f xs) (TimeBodyList.mapBody f ys)

mergeFilter :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeFilter p xs0 ys0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
   in  TimeBodyList.filter p (TimeBodyList.merge xs ys)  ==
       TimeBodyList.merge (TimeBodyList.filter p xs) (TimeBodyList.filter p ys)

mergePartition :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> TimeBodyList.T time body -> Bool
mergePartition p xs0 =
   let xs = TimeBodyList.normalize xs0
   in  xs  ==  uncurry TimeBodyList.merge (TimeBodyList.partition p xs)

mergeEitherMapMaybe :: (NonNeg.C time, Ord body) =>
   TimeBodyList.T time body -> TimeBodyList.T time body -> Bool
mergeEitherMapMaybe xs0 ys0 =
   let xs = TimeBodyList.normalize xs0
       ys = TimeBodyList.normalize ys0
       zs = TimeBodyList.merge
               (TimeBodyList.mapBody Left xs)
               (TimeBodyList.mapBody Right ys)
   in  xs  ==  TimeBodyList.mapMaybe (either Just (const Nothing)) zs
       &&
       ys  ==  TimeBodyList.mapMaybe (either (const Nothing) Just) zs


mergeInfinite :: (NonNeg.C time, Ord body) =>
   NonEmptyList time body ->
   NonEmptyList time body -> Bool
mergeInfinite xs0 ys0 =
   let xs = makeInfiniteEventList xs0
       ys = makeInfiniteEventList ys0
   in  checkInfinite (TimeBodyList.merge xs ys)



insertCommutative :: (NonNeg.C time, Ord body) =>
   (time, body) -> (time, body) -> TimeBodyList.T time body -> Bool
insertCommutative (time0, body0) (time1, body1) evs =
   TimeBodyList.insert time0 body0 (TimeBodyList.insert time1 body1 evs)  ==
   TimeBodyList.insert time1 body1 (TimeBodyList.insert time0 body0 evs)

{-
Relative.BodyEnd.insert merge: Falsifiable, after 12 tests:
2
'}'
1 /. '%' ./ 1 /. '}' ./ 0 /. ' ' ./ 5 /. 'z' ./ 5 /. '\'' ./ 2 /. '\DEL' ./ 2 /. 'x' ./ 3 /. '\DEL' ./ empty
-}
insertMerge :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeBodyList.T time body -> Bool
insertMerge time body evs =
   TimeBodyList.insert time body evs  ==
   TimeBodyList.merge (TimeBodyList.cons time body TimeBodyList.empty) evs

insertNormalize :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeBodyList.T time body -> Bool
insertNormalize time body =
   isNormalized . TimeBodyList.insert time body . TimeBodyList.normalize

insertSplitAtTime :: (NonNeg.C time, Ord body) =>
   time -> body -> TimeBodyList.T time body -> Bool
insertSplitAtTime time body evs =
   TimeBodyList.insert
      (min time (TimeBodyList.duration evs)) body
      (TimeBodyList.normalize evs)
   ==
      let (prefix,suffix) = TimeMixedList.splitAtTime time evs
      in  TimeBodyList.normalize (TimeMixedList.appendBodyEnd prefix
             (MixedBodyList.consTime zero (MixedBodyList.consBody body suffix)))

insertInfinite :: (NonNeg.C time, Ord body) =>
   time -> body -> NonEmptyList time body -> Bool
insertInfinite time body =
   checkInfinite . TimeBodyList.insert time body . makeInfiniteEventList



moveForwardIdentity :: (NonNeg.C time, Num time, Ord body) =>
   TimeBodyList.T time body -> Bool
moveForwardIdentity evs =
   evs ==
   TimeBodyList.moveForward (TimeBodyList.mapBody ((,) zero) evs)

moveForwardAdditive :: (NonNeg.C time, Num time, Ord body) =>
   TimeBodyList.T time ((time,time),body) -> Bool
moveForwardAdditive evs =
   TimeBodyList.normalize (moveForwardLimited (moveForwardLimited
      (TimeBodyList.mapBody (\((t0,t1),b) -> (t0,(t1,b))) evs))) ==
   TimeBodyList.normalize (moveForwardLimited
      (TimeBodyList.mapBody (mapFst (uncurry add)) evs))

moveForwardCommutative :: (NonNeg.C time, Num time, Ord body) =>
   TimeBodyList.T time ((time,time),body) -> Bool
moveForwardCommutative evs =
   TimeBodyList.normalize (moveForwardLimited (moveForwardLimited
      (TimeBodyList.mapBody (\((t0,t1),b) -> (t0,(t1,b))) evs))) ==
   TimeBodyList.normalize (moveForwardLimited (moveForwardLimited
      (TimeBodyList.mapBody (\((t0,t1),b) -> (t1,(t0,b))) evs)))

{-
moveForwardRestricted :: (NonNeg.C time, Ord body) =>
   time -> TimeBodyList.T time (time,body) -> Bool
moveForwardRestricted maxTime evs0 =
   let evs =
          TimeBodyList.mapBody (mapFst (min maxTime)) $
          restrictMoveTimes (TimeBodyList.normalize evs0)
   in  TimeBodyList.moveForward evs  ==
       TimeBodyList.moveForwardRestricted maxTime evs

moveForwardRestrictedInfinity :: (NonNeg.C time, Eq body) =>
   time -> NonEmptyList time (time,body) -> Bool
moveForwardRestrictedInfinity maxTime =
   checkInfinite .
   TimeBodyList.moveForwardRestricted maxTime .
   TimeBodyList.mapBody (mapFst (min maxTime)) .
   restrictMoveTimes .
   makeUncollapsedInfiniteEventList
-}


moveForwardLimited :: (NonNeg.C time, Num time) =>
   TimeBodyList.T time (time,body) -> TimeBodyList.T time body
moveForwardLimited = TimeBodyList.moveForward . restrictMoveTimes

restrictMoveTimes :: (NonNeg.C time) =>
   TimeBodyList.T time (time,body) -> TimeBodyList.T time (time,body)
restrictMoveTimes =
   flip evalState zero .
   TimeBodyList.mapM
      (\t -> modify (add t) >> return t)
      (\(t,b) -> gets (\tm -> (min t tm, b)))



spanSatisfy :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> TimeBodyList.T time body -> Bool
spanSatisfy p =
   uncurry (&&) .
   mapPair
     (all p . TimeBodyList.getBodies,
      TimeBodyList.switchL True (const . not . p . snd)) .
   TimeBodyList.span p

spanAppend :: (NonNeg.C time, Eq body) =>
   (body -> Bool) -> TimeBodyList.T time body -> Bool
spanAppend p xs =
   uncurry TimeBodyList.append (TimeBodyList.span p xs)  ==  xs

spanInfinite :: (NonNeg.C time, Ord body) =>
   (body -> Bool) -> NonEmptyList time body -> Bool
spanInfinite p =
   checkInfinite . uncurry TimeBodyList.append .
   TimeBodyList.span p . makeInfiniteEventList


coincidentFlatten :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
coincidentFlatten xs =
   xs  ==  TimeBodyList.flatten (TimeBodyList.collectCoincident xs)

collectCoincidentGaps :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
collectCoincidentGaps xs =
   let times = TimeBodyList.getTimes (TimeBodyList.collectCoincident xs)
   in  null times || all (zero<) (tail times)

collectCoincidentNonEmpty :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
collectCoincidentNonEmpty =
   all (not . null) . TimeBodyList.getBodies . TimeBodyList.collectCoincident

collectCoincidentInfinite :: (NonNeg.C time, Num time, Eq body) =>
   NonEmptyList time body -> Bool
collectCoincidentInfinite =
   checkInfinite .
   TimeBodyList.collectCoincident .
   makeUncollapsedInfiniteEventList


mapCoincidentMap :: (NonNeg.C time, Eq body1) =>
   (body0 -> body1) -> TimeBodyList.T time body0 -> Bool
mapCoincidentMap f xs =
   TimeBodyList.mapBody f xs  ==
   TimeBodyList.mapCoincident (map f) xs

mapCoincidentComposition :: (NonNeg.C time, Eq body2) =>
   ([body0] -> [body1]) -> ([body1] -> [body2]) -> TimeBodyList.T time body0 -> Bool
mapCoincidentComposition f g xs =
   TimeBodyList.mapCoincident (g . f) xs  ==
   (TimeBodyList.mapCoincident g . TimeBodyList.mapCoincident f) xs

mapCoincidentReverse :: (NonNeg.C time, Eq body) =>
   TimeBodyList.T time body -> Bool
mapCoincidentReverse xs =
   xs  ==  TimeBodyList.mapCoincident reverse (TimeBodyList.mapCoincident reverse xs)



mapBodyMAppend ::
   (Monad m, Eq body1, NonNeg.C time) =>
   (m (TimeBodyList.T time body1) -> TimeBodyList.T time body1) ->
   (body0 -> m body1) -> TimeBodyList.T time body0 -> TimeBodyList.T time body0 -> Bool
mapBodyMAppend run f xs ys =
   run (TimeBodyList.mapM return f (TimeBodyList.append xs ys))  ==
   run (liftM2 TimeBodyList.append (TimeBodyList.mapM return f xs) (TimeBodyList.mapM return f ys))

mapBodyMAppendRandom ::
   (Random body, NonNeg.C time, Eq body) =>
   Int -> TimeBodyList.T time (body,body) -> TimeBodyList.T time (body,body) -> Bool
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
   TimeBodyList.mapM return (state . randomR) .
   makeInfiniteEventList


{-

mapM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   TimeBodyList.T time0 body0 -> m (TimeBodyList.T time1 body1)
mapM timeAction bodyAction =
   Uniform.mapM bodyAction timeAction

mapImmM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   Immediate time0 body0 -> m (Immediate time1 body1)
mapImmM timeAction bodyAction =
   Disp.mapM bodyAction timeAction


getBodies :: TimeBodyList.T time body -> [body]
getBodies = Uniform.getFirsts

getTimes :: TimeBodyList.T time body -> [time]
getTimes = Uniform.getSeconds


empty :: Immediate time body
empty = Disp.empty


cons :: time -> body -> TimeBodyList.T time body -> TimeBodyList.T time body
cons = Uniform.cons


snoc :: TimeBodyList.T time body -> body -> time -> TimeBodyList.T time body
snoc = Uniform.snoc


{-
propInsertPadded :: Event time body -> TimeBodyList.T time body -> Bool
propInsertPadded (Event time body) evs =
   EventList.insert time body (fst evs)  ==  fst (insert time body evs)
-}

appendSingle :: -- (NonNeg.C time, Ord time, Ord body) =>
   body -> TimeBodyList.T time body -> EventList.T time body
appendSingle body xs =
   Disp.foldr EventList.consTime EventList.consBody EventList.empty $
   Uniform.snocFirst xs body

fromEventList :: time -> EventList.T time body -> TimeBodyList.T time body
fromEventList t =
   EventList.foldr consTime consBody (pause t)

toEventList :: TimeBodyList.T time body -> EventList.T time body
toEventList xs =
   zipWith EventList.Event (getTimes xs) (getBodies xs)

{- |

-}


discretize :: (RealFrac time, Integral i) =>
   TimeBodyList.T time body -> TimeBodyList.T i body
discretize es =
   evalState (Uniform.mapSecondM roundDiff es) zero

resample :: (RealFrac time, Integral i) =>
   time -> TimeBodyList.T time body -> TimeBodyList.T i body
resample rate es =
   discretize (mapTime (rate*) es)
-}

resampleInfinite :: (Eq body) =>
   TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) body -> Bool
resampleInfinite rateInt =
   let rate = timeToDouble rateInt + 1
   in  checkInfinite . intTimeList . TimeBodyList.resample rate .
       makeInfiniteEventList .
       mapPair (mapFst makeFracTime, TimeBodyList.mapTime makeFracTime)


{-
toAbsoluteEventList :: (Num time) =>
   time -> TimeBodyList.T time body -> AbsoluteEventList.T time body
toAbsoluteEventList start xs =
   let ts = Uniform.getSeconds xs
       bs = Uniform.getFirsts  xs
       ats = List.scanl add start ts
   in  maybe
          (error "padded list always contains one time value")
          (\ ~(ats0,lt) -> (zip ats0 bs, lt))
          (viewR ats)
-}




type NonEmptyList time body = ((time, body), TimeBodyList.T time body)

makeUncollapsedInfiniteEventList ::
   (NonNeg.C time, Num time) =>
   NonEmptyList time body -> TimeBodyList.T time body
makeUncollapsedInfiniteEventList =
   makeInfiniteEventList .
   mapFst (\(time,body) -> (add time 1, body))

makeInfiniteEventList :: (NonNeg.C time) =>
   NonEmptyList time body -> TimeBodyList.T time body
makeInfiniteEventList =
   TimeBodyList.cycle . makeNonEmptyEventList

makeNonEmptyEventList :: (NonNeg.C time) =>
   NonEmptyList time body -> TimeBodyList.T time body
makeNonEmptyEventList ((t, b), evs) =
   TimeBodyList.cons t b evs

{- |
Pick an arbitrary element from an infinite list
and check if it can be evaluated.
-}
checkInfinite :: (Eq time, Eq body) =>
   TimeBodyList.T time body -> Bool
checkInfinite xs0 =
   let x = TimeBodyList.switchL
              (error "BodyEnd.checkInfinite: empty list") const $
              TimeBodyPriv.lift (Disp.drop 100) xs0
   in  x == x



tests :: [(String, IO ())]
tests =
   ("viewTimeL consTime",
     quickCheck (viewLConsTime :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("viewBodyL consBody",
     quickCheck (viewLConsBody :: BodyBodyList.T TimeDiff ArbChar -> Bool)) :
   ("switchTimeL consTime",
     quickCheck (switchLConsTime :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("switchBodyL consBody",
     quickCheck (switchLConsBody :: BodyBodyList.T TimeDiff ArbChar -> Bool)) :

   ("viewLInfinite",
     quickCheck (viewLInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("switchLInfinite",
     quickCheck (switchLInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("consInfinite",
     quickCheck (consInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("consTimeBodyInfinite",
     quickCheck (consTimeBodyInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("snocInfinite",
     quickCheck (snocInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("consInfix",
     quickCheck (consInfix :: TimeDiff -> ArbChar -> TimeDiff -> ArbChar -> Bool)) :


   ("map body composition",
     quickCheck (mapBodyComposition toUpper toLower
               :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("map time composition",
     quickCheck ((\dt0 dt1 -> mapTimeComposition (add dt0) (add dt1))
               :: TimeDiff -> TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("map time body commutative",
     quickCheck ((\dt -> mapTimeBodyCommutative (add dt) toUpper)
               :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("mapBodyInfinite",
     quickCheck (mapBodyInfinite toUpper
               :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("mapTimeInfinite",
     quickCheck (\dt -> mapTimeInfinite (add dt)
               :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("mapNormalize",
     quickCheck (mapNormalize succ
               :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("append left identity",
     quickCheck (appendLeftIdentity :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("append right identity",
     quickCheck (appendRightIdentity :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("append associative",
     quickCheck (appendAssociative
              :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar ->
                 TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("appendCons",
     quickCheck (appendCons :: TimeDiff -> ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mapBodyAppend",
     quickCheck (mapBodyAppend toUpper
               :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("appendSplitAtTime",
     quickCheck (appendSplitAtTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("appendSplitAfterTime",
     quickCheck (appendSplitAfterTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("nonZeroTakeTime",
     quickCheck (nonZeroTakeTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("nonZeroDropAfterTime",
     quickCheck (nonZeroDropAfterTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("appendFirstInfinite",
     quickCheck (appendFirstInfinite :: NonEmptyList TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("appendSecondInfinite",
     quickCheck (appendSecondInfinite :: TimeBodyList.T TimeDiff ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("cycleInfinite",
     quickCheck (cycleInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("decreaseStart delay",
     quickCheck (decreaseStartDelay :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("decreaseStartInfinite",
     quickCheck (decreaseStartInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("delay additive",
     quickCheck (delayAdditive :: TimeDiff -> TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("delay append pause",
     quickCheck (delayAppendPause :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("delayInfinite",
     quickCheck (delayInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("splitAtTakeDropTime",
     quickCheck (splitAtTakeDropTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeEndPause",
     quickCheck (takeTimeEndPause :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeAppendFirst",
     quickCheck (takeTimeAppendFirst :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeAppendSecond",
     quickCheck (takeTimeAppendSecond :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeNormalize",
     quickCheck (takeTimeNormalize :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("dropTimeNormalize",
     quickCheck (dropTimeNormalize :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("takeTimeInfinite",
     quickCheck (takeTimeInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("dropTimeInfinite",
     quickCheck (dropTimeInfinite :: TimeDiff -> NonEmptyList TimeDiff ArbChar -> Bool)) :
{-
   ("splitAtTimeLazyInfinite",
     quickCheck (splitAtTimeLazyInfinite :: BodyBodyList.T (NonNegChunky.T TimeDiff) ArbChar -> Bool)) :
   ("dropTimeLazyInfinite",
     quickCheck (dropTimeLazyInfinite :: BodyBodyList.T (NonNegChunky.T TimeDiff) ArbChar -> Bool)) :
-}

   ("duration pause",
     quickCheck (durationPause :: TimeDiff -> Bool)) :
   ("duration append",
     quickCheck (durationAppend :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("duration merge",
     quickCheck (durationMerge :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("durationTakeTime",
     quickCheck (durationTakeTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("durationDropTime",
     quickCheck (durationDropTime :: TimeDiff -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("filterSatisfy",
     quickCheck (\c -> filterSatisfy (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterProjection",
     quickCheck (\c -> filterProjection (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterCommutative",
     quickCheck (\c0 c1 -> filterCommutative (c0<) (c1>) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterComposition",
     quickCheck (\c0 c1 -> filterComposition (c0<) (c1>) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterNormalize",
     quickCheck (\c -> filterNormalize (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterAppend",
     quickCheck (\c -> filterAppend (c<) :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterDuration",
     quickCheck (\c -> filterDuration (c<) :: TimeTimeList.T TimeDiff ArbChar -> Bool)) :
   ("filterPartition",
     quickCheck (\c -> filterPartition (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("filterInfinite",
     quickCheck (\c -> filterInfinite (c<) :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("catMaybesAppend",
     quickCheck (catMaybesAppend :: TimeBodyList.T TimeDiff (Maybe ArbChar) -> TimeBodyList.T TimeDiff (Maybe ArbChar) -> Bool)) :

   ("mergeNormalize",
     quickCheck (mergeNormalize :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("merge left identity",
     quickCheck (mergeLeftIdentity :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("merge right identity",
     quickCheck (mergeRightIdentity :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("merge commutative",
     quickCheck (mergeCommutative :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("merge associative",
     quickCheck (mergeAssociative :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("merge append",
     quickCheck (mergeAppend :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mergeMap",
     quickCheck (mergeMap succ :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mergeFilter",
     quickCheck (\c -> mergeFilter (c>)
             :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mergePartition",
     quickCheck (\c -> mergePartition (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mergeEitherMapMaybe",
     quickCheck (mergeEitherMapMaybe
         :: TimeBodyList.T TimeDiff ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mergeInfinite",
     quickCheck (mergeInfinite
         :: NonEmptyList TimeDiff ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("insert commutative",
     quickCheck (insertCommutative :: (TimeDiff, ArbChar) -> (TimeDiff, ArbChar) -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("insert merge",
     quickCheck (insertMerge :: TimeDiff -> ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("insertNormalize",
     quickCheck (insertNormalize :: TimeDiff -> ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("insertSplitAtTime",
     quickCheck (insertSplitAtTime :: TimeDiff -> ArbChar -> TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("insertInfinite",
     quickCheck (insertInfinite :: TimeDiff -> ArbChar -> NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("moveForwardIdentity",
     quickCheck (moveForwardIdentity :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("moveForwardAdditive",
     quickCheck (moveForwardAdditive :: TimeBodyList.T TimeDiff ((TimeDiff,TimeDiff),ArbChar) -> Bool)) :
   ("moveForwardCommutative",
     quickCheck (moveForwardCommutative :: TimeBodyList.T TimeDiff ((TimeDiff,TimeDiff),ArbChar) -> Bool)) :
{-
   ("moveForwardRestricted",
     quickCheck (moveForwardRestricted :: TimeDiff -> TimeBodyList.T TimeDiff (TimeDiff,ArbChar) -> Bool)) :
   ("moveForwardRestrictedInfinity",
     quickCheck (moveForwardRestrictedInfinity :: TimeDiff -> NonEmptyList TimeDiff (TimeDiff,ArbChar) -> Bool)) :
-}

   ("spanSatisfy",
     quickCheck (\c -> spanSatisfy (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("spanAppend",
     quickCheck (\c -> spanAppend (c<) :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("spanInfinite",
     quickCheck (\c -> spanInfinite (c<) :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("coincidentFlatten",
     quickCheck (coincidentFlatten :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentGaps",
     quickCheck (collectCoincidentGaps :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentNonEmpty",
     quickCheck (collectCoincidentNonEmpty :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentInfinite",
     quickCheck (collectCoincidentInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :

   ("mapCoincidentMap",
     quickCheck (mapCoincidentMap toUpper :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mapCoincidentComposition",
     quickCheck (mapCoincidentComposition reverse reverse :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :
   ("mapCoincidentReverse",
     quickCheck (mapCoincidentReverse :: TimeBodyList.T TimeDiff ArbChar -> Bool)) :

   ("mapBodyMAppendRandom",
     quickCheck (mapBodyMAppendRandom :: Int -> TimeBodyList.T TimeDiff (ArbChar,ArbChar) -> TimeBodyList.T TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("mapBodyMInfinite",
     quickCheck (mapBodyMInfinite :: Int -> NonEmptyList TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("resampleInfinite",
     quickCheck (resampleInfinite :: TimeDiff -> NonEmptyList (TimeDiff,TimeDiff) ArbChar -> Bool)) :

   []
