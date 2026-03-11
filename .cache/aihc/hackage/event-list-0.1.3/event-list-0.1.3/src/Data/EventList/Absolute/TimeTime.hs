{- |
Copyright   :  (c) Henning Thielemann 2007-2009

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Event list with absolute times starting with a time and ending with a body
-}
module Data.EventList.Absolute.TimeTime
   (T,
    pause, isPause,
    viewL, switchL, cons, snoc,
    mapBody, mapTime,
    concatMapMonoid,
    traverse, traverse_, traverseBody, traverseTime,
    mapM, mapM_, mapBodyM, mapTimeM,
    getTimes, getBodies, duration,
    merge, mergeBy, insert, insertBy,
    moveForward,
    decreaseStart, delay, filter, partition, slice, foldr,
    mapMaybe, catMaybes,
    normalize, isNormalized,
    collectCoincident, flatten, mapCoincident,
    append, concat, cycle,
    discretize, resample,
   ) where

import Data.EventList.Absolute.TimeTimePrivate
import Data.EventList.Absolute.TimeBodyPrivate (($~))
import qualified Data.EventList.Absolute.TimeBodyPrivate as TimeBodyPriv
import qualified Data.EventList.Absolute.TimeBody as TimeBodyList

import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Data.EventList.Utility as Utility

import Data.Tuple.HT (mapSnd, mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.List.HT (isAscending, )

import qualified Control.Monad as Monad
import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), )

import Control.Monad.Trans.State (state, evalState)
import Control.Monad (Monad, (>>), )

import Data.Monoid (Monoid, )

import Data.Function (flip, (.), ($), )
import Data.Functor (fmap, )
import Data.Maybe (Maybe(Just), fromMaybe, )
import Data.Tuple (uncurry, fst, snd, )
import Data.Ord (Ord, max, (<), (>=), )
import Data.Eq (Eq, (/=), )
import Prelude
   (Num, Integral, RealFrac, round, subtract, (*), (+), (-),
    Bool, error, )



pause :: time -> T time body
pause = Cons . Uniform.singleton

isPause :: T time body -> Bool
isPause = Uniform.isSingleton . decons



getBodies :: T time body -> [body]
getBodies = Uniform.getFirsts . decons

getTimes :: T time body -> [time]
getTimes = Uniform.getSeconds . decons

duration :: Num time => T time body -> time
duration = snd . viewTimeR
-- duration = last . getTimes



cons :: time -> body -> T time body -> T time body
cons time body = lift (Uniform.cons time body)

snoc :: T time body -> body -> time -> T time body
snoc xs body time =
   Cons $ (Uniform.snoc $* xs) body time


viewL :: T time body -> (time, Maybe (body, T time body))
viewL =
   mapSnd (fmap (mapSnd Cons) . Mixed.viewFirstL) .
   Mixed.viewSecondL .
   decons

{-# INLINE switchL #-}
switchL :: (time -> a) -> ((time, body) -> T time body -> a) -> T time body -> a
switchL f g =
   Mixed.switchL f (\t b -> g (t,b) . Cons) .
   decons


mapBody :: (body0 -> body1) -> T time body0 -> T time body1
mapBody = lift . Uniform.mapFirst

mapTime :: (time0 -> time1) -> T time0 body -> T time1 body
mapTime = lift . Uniform.mapSecond



concatMapMonoid :: Monoid m =>
   (time -> m) -> (body -> m) ->
   T time body -> m
concatMapMonoid f g =
   Uniform.concatMapMonoid g f . decons


traverse :: Applicative m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
traverse f g = liftA (Uniform.traverse g f)

traverse_ :: Applicative m =>
   (time -> m ()) -> (body -> m ()) ->
   T time body -> m ()
traverse_ f g = Uniform.traverse_ g f . decons


traverseBody :: Applicative m =>
   (body0 -> m body1) -> T time body0 -> m (T time body1)
traverseBody f = liftA (Uniform.traverseFirst f)

traverseTime :: Applicative m =>
   (time0 -> m time1) -> T time0 body -> m (T time1 body)
traverseTime f = liftA (Uniform.traverseSecond f)


mapM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
mapM f g =
   unwrapMonad . traverse (WrapMonad . f) (WrapMonad . g)

mapM_ :: Monad m =>
   (time -> m ()) -> (body -> m ()) ->
   T time body -> m ()
mapM_ f g =
   unwrapMonad . traverse_ (WrapMonad . f) (WrapMonad . g)


mapBodyM :: Monad m =>
   (body0 -> m body1) -> T time body0 -> m (T time body1)
mapBodyM f = unwrapMonad . traverseBody (WrapMonad . f)

mapTimeM :: Monad m =>
   (time0 -> m time1) -> T time0 body -> m (T time1 body)
mapTimeM f = unwrapMonad . traverseTime (WrapMonad . f)



foldr :: (time -> a -> b) -> (body -> b -> a) -> a -> T time body -> b
foldr f g x = Uniform.foldr g f x . decons


filter :: (Num time) =>
   (body -> Bool) -> T time body -> T time body
filter p = mapMaybe (\b -> toMaybe (p b) b)

mapMaybe :: (Num time) =>
   (body0 -> Maybe body1) ->
   T time body0 -> T time body1
mapMaybe f = catMaybes . mapBody f

catMaybes :: (Num time) =>
   T time (Maybe body) -> T time body
catMaybes =
   mapTimeInit TimeBodyList.catMaybes

{-
Could be implemented more easily in terms of Uniform.partition
-}
partition ::
   (body -> Bool) -> T time body -> (T time body, T time body)
partition p =
   switchTimeR
   (\ xs t ->
      mapPair
         (flip snocTime t, flip snocTime t)
         (TimeBodyList.partition p xs))

slice :: (Eq a, Num time) =>
   (body -> a) -> T time body -> [(a, T time body)]
slice =
   Utility.slice
      (fmap fst . snd . viewL)
      partition


collectCoincident :: Eq time => T time body -> T time [body]
collectCoincident =
   Cons .
   Mixed.switchSecondL
   (\ t0 ->
      Mixed.consSecond t0 .
      Mixed.mapFirstInit
         (Uniform.catMaybesFirst .
          flip evalState (Just t0) .
          Uniform.traverseFirst (\time -> state $ \ oldTime ->
             (Monad.guard (time /= oldTime) >> time, time)) .
          Uniform.mapFirst Just)) .
   decons


flatten :: (Ord time) => T time [body] -> T time body
flatten = mapTimeInit TimeBodyList.flatten


{- |
Apply a function to the lists of coincident events.
-}

mapCoincident :: (Ord time) =>
   ([a] -> [b]) -> T time a -> T time b
mapCoincident f = flatten . mapBody f . collectCoincident

{- |

'List.sort' sorts a list of coinciding events,
that is all events but the first one have time difference 0.
'normalize' sorts all coinciding events in a list
thus yielding a canonical representation of a time ordered list.
-}

normalize :: (Ord time, Num time, Ord body) => T time body -> T time body
normalize = mapCoincident List.sort

isNormalized :: (Ord time, Num time, Ord body) =>
   T time body -> Bool
isNormalized =
   List.all isAscending . getBodies . collectCoincident



merge :: (Ord time, Ord body) =>
   T time body -> T time body -> T time body
merge = mergeBy (<)

mergeBy :: (Ord time) =>
   (body -> body -> Bool) ->
   T time body -> T time body -> T time body
mergeBy before xs0 ys0 =
   let (xs,xt) = viewTimeR xs0
       (ys,yt) = viewTimeR ys0
   in  snocTime
          (TimeBodyList.mergeBy before xs ys)
          (max xt yt)

insert :: (Ord time, Ord body) =>
   time -> body -> T time body -> T time body
insert = insertBy (<)

insertBy :: (Ord time) =>
   (body -> body -> Bool) ->
   time -> body -> T time body -> T time body
insertBy before t0 me0 mevs1 =
   let mev0 = (t0, me0)
   in  switchL
          (\t1 -> uncurry cons mev0 $ pause (max t0 t1))
          (\mev1 mevs ->
              if Utility.beforeBy before mev0 mev1
                then uncurry cons mev0 $ mevs1
                else uncurry cons mev1 $ uncurry (insertBy before) mev0 mevs)
          mevs1



{- |
Move events towards the front of the event list.
You must make sure, that no event is moved before time zero.
This works only for finite lists.
-}
moveForward :: (Ord time, Num time) =>
   T time (time, body) -> T time body
moveForward =
   mapTimeInit TimeBodyList.moveForward



append :: (Ord time, Num time) =>
   T time body -> T time body -> T time body
append =
   switchTimeR
   (\xs t -> lift (Mixed.appendDisparateUniform $~ xs) . delay t)

concat :: (Ord time, Num time) =>
   [T time body] -> T time body
concat xs =
   let ts0 = List.scanl (+) 0 (List.map duration xs)
       (ts,dur) =
          fromMaybe
             (error "list of accumulated times is always non-empty")
             (ListHT.viewR ts0)
   in  snocTime
          (TimeBodyPriv.Cons $ Disp.concat $ List.map TimeBodyPriv.decons $
           List.zipWith TimeBodyList.delay ts (List.map (fst . viewTimeR) xs))
          dur

cycle :: (Ord time, Num time) =>
   T time body -> T time body
cycle = concat . List.repeat


decreaseStart :: (Ord time, Num time) =>
   time -> T time body -> T time body
decreaseStart dif =
   Cons .
   Mixed.switchSecondL
   (\ t xs ->
      Mixed.consSecond
         (if t>=dif
            then t-dif
            else error "decreaseStart: difference too big")
         (Disp.mapSecond (subtract dif) xs)) .
   decons

delay :: (Ord time, Num time) =>
   time -> T time body -> T time body
delay dif =
   if dif>=0
     then mapTime (dif+)
     else error "delay: negative delay"


discretize :: (RealFrac time, Integral i) =>
   T time body -> T i body
discretize = mapTime round

resample :: (RealFrac time, Integral i) =>
   time -> T time body -> T i body
resample rate =
   discretize . mapTime (rate*)

