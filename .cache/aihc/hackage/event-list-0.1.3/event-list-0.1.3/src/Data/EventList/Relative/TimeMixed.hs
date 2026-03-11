{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a time difference
and ending with either a data body or a time difference.
-}
module Data.EventList.Relative.TimeMixed
   (snocBody, snocTime,
--    (/.), (./),
    viewTimeR,   viewBodyR,
    switchTimeR, switchBodyR,
    mapTimeR, mapTimeLast, mapTimeInit,
    mapBodyR, mapBodyLast, mapBodyInit,
    appendBodyEnd, prependBodyEnd,
    splitAtTime, takeTime, dropTime,
    splitAfterTime, takeAfterTime, dropAfterTime,
   ) where

import qualified Data.EventList.Relative.TimeBody as TimeBodyList
import qualified Data.EventList.Relative.TimeTime as TimeTimeList

import qualified Data.EventList.Relative.TimeBodyPrivate as TimeBodyPriv
import qualified Data.EventList.Relative.TimeTimePrivate as TimeTimePriv
-- import Data.EventList.Relative.TimeBodyPrivate (($~*))

import Data.EventList.Relative.TimeTimePrivate
   (viewTimeR, viewBodyR, switchTimeR, switchBodyR,
    mapTimeR, mapTimeLast, mapTimeInit)

import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

-- import Data.AlternatingList.List.Mixed ((/.), (./))

import qualified Numeric.NonNegative.Class as NonNeg
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )


snocBody :: TimeTimeList.T time body -> body -> TimeBodyList.T time body
snocBody xs = TimeBodyPriv.Cons . Mixed.snocFirst (TimeTimePriv.decons xs)

snocTime :: TimeBodyList.T time body -> time -> TimeTimeList.T time body
snocTime xs = TimeTimePriv.Cons . Mixed.snocSecond (TimeBodyPriv.decons xs)



mapBodyR ::
   (TimeTimeList.T time0 body -> TimeTimeList.T time1 body, body -> body) ->
   TimeBodyList.T time0 body -> TimeBodyList.T time1 body
mapBodyR = TimeBodyPriv.lift . Mixed.mapFirstR . mapFst TimeTimePriv.unlift

mapBodyLast ::
   (body -> body) ->
   TimeBodyList.T time body -> TimeBodyList.T time body
mapBodyLast = TimeBodyPriv.lift . Mixed.mapFirstLast

mapBodyInit ::
   (TimeTimeList.T time0 body -> TimeTimeList.T time1 body) ->
   TimeBodyList.T time0 body -> TimeBodyList.T time1 body
mapBodyInit = TimeBodyPriv.lift . Mixed.mapFirstInit . TimeTimePriv.unlift


{-
propInsertPadded :: Event time body -> T time body -> Bool
propInsertPadded (Event time body) evs =
   TimeBodyList.insert time body (fst evs)  ==  fst (insert time body evs)
-}

{- |
This is not a good name, expect a change.
-}
appendBodyEnd :: (NonNeg.C time) =>
   TimeTimeList.T time body -> TimeBodyList.T time body -> TimeBodyList.T time body
appendBodyEnd =
   switchTimeR
   (\ xs t -> TimeBodyList.append xs . TimeBodyList.delay t)

{- |
This is not a good name, expect a change.
-}
prependBodyEnd ::
   TimeBodyList.T time body -> TimeTimeList.T time body -> TimeTimeList.T time body
prependBodyEnd =
   TimeTimePriv.lift . Mixed.appendDisparateUniform . TimeBodyPriv.decons


liftSplit ::
   (Disp.T time0 body0 -> (Uniform.T body1 time1, Disp.T time2 body2)) ->
   TimeBodyList.T time0 body0 ->
   (TimeTimeList.T time1 body1, TimeBodyList.T time2 body2)
liftSplit f =
   mapPair (TimeTimePriv.Cons, TimeBodyPriv.Cons) . f . TimeBodyPriv.decons

splitAtTimeAux :: (NonNeg.C time) =>
   (time -> time -> (time, (Bool, time))) ->
   time -> Disp.T time body ->
   (Uniform.T body time, Disp.T time body)
splitAtTimeAux splitTime =
   let go t0 =
         mapFst Uniform.forceSecondHead .
         Mixed.switchFirstL
            (Mixed.consSecond NonNeg.zero Disp.empty, Disp.empty)
            (\t1 xs ->
               let (mt,~(before,dt)) = splitTime t0 t1
               in  mapFst (Mixed.consSecond mt) $
                   if before
                     then (Disp.empty, Mixed.consFirst dt xs)
                     else
                        Mixed.switchSecondL
                           (\b ys -> mapFst (Mixed.consFirst b) $ go dt ys)
                           xs)
   in  go

{- |
At the division time move all zero time differences to the suffix part,
that is we will always split before a group of events.
-}
splitAtTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body ->
   (TimeTimeList.T time body, TimeBodyList.T time body)
splitAtTime = liftSplit . splitAtTimeAux NonNeg.split

takeTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body -> TimeTimeList.T time body
takeTime t = fst . splitAtTime t

dropTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body -> TimeBodyList.T time body
dropTime t = snd . splitAtTime t


{- |
At the division time move all zero time differences to the prefix part,
that is we will always split after a group of events.
-}
splitAfterTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body ->
   (TimeTimeList.T time body, TimeBodyList.T time body)
splitAfterTime =
   liftSplit .
   splitAtTimeAux (\t0 t1 -> mapSnd (mapFst not) $ NonNeg.split t1 t0)

takeAfterTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body -> TimeTimeList.T time body
takeAfterTime t = fst . splitAfterTime t

dropAfterTime :: (NonNeg.C time) =>
   time -> TimeBodyList.T time body -> TimeBodyList.T time body
dropAfterTime t = snd . splitAfterTime t
