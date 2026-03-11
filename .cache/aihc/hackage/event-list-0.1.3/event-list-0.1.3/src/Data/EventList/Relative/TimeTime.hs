{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Event lists starting with a time difference and ending with a time difference.
-}
module Data.EventList.Relative.TimeTime
   (T,
    mapBody, mapTime,
    zipWithBody, zipWithTime, unzip,
    concatMapMonoid,
    traverse, traverse_, traverseBody, traverseTime,
    mapM, mapM_, mapBodyM, mapTimeM,
    getTimes, getBodies, duration,
    merge, mergeBy, insert, {- insertBy, -} pad,
    moveForward, moveForwardRestricted, moveBackward, arrange, arrangeBy,
    moveForwardRestrictedBy,
    moveForwardRestrictedByQueue, moveForwardRestrictedByStrict,
    decreaseStart, delay,
    filter, partition, partitionMaybe, partitionMaybeR, slice,
    foldr, foldl,
    pause, isPause, cons, snoc, viewL, viewR, switchL, switchR,
    mapMaybe, catMaybes, catMaybesR,
    append, concat, concatNaive, cycle, cycleNaive, reverse,
    splitAtTime, takeTime, dropTime,
    forceTimeHead,
    discretize, resample,
    collectCoincident, flatten, mapCoincident,
    normalize, isNormalized,
    toAbsoluteEventList, fromAbsoluteEventList,
   ) where

import Data.EventList.Relative.TimeTimePrivate as TimeTimePriv

import qualified Data.EventList.Relative.BodyTimePrivate as BodyTimePriv
import qualified Data.EventList.Relative.TimeBody as TimeBodyList

import qualified Data.EventList.Absolute.TimeTimePrivate as AbsoluteEventPriv
import qualified Data.EventList.Absolute.TimeTime as AbsoluteEventList

-- import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Data.List as List
import qualified Data.EventList.Utility as Utility

import Data.Monoid (Monoid, mempty, mconcat, )

import qualified Numeric.NonNegative.Class as NonNeg
import Numeric.NonNegative.Class ((-|), zero, add, )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.List.HT (isAscending, )
import Data.EventList.Utility (floorDiff, )
import Control.Monad.Trans.State (evalState, modify, get, gets, put, )

import Control.Monad (Monad, return, liftM2, (>>), )
import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), )

import Data.Function ((.), ($), id, flip, )
import Data.Functor (fmap, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Tuple (fst, snd, )
import Data.Ord (Ord, (<), )
import Data.Eq (Eq, (==), )
import Data.Bool (Bool(False, True), not, (&&), )
import Prelude (Num, Integral, RealFrac, (*), (+), (-), seq, )



pause :: time -> T time body
pause = Cons . Uniform.singleton

isPause :: T time body -> Bool
isPause = Uniform.isSingleton . decons



getBodies :: T time body -> [body]
getBodies = Uniform.getFirsts . decons

getTimes :: T time body -> [time]
getTimes = Uniform.getSeconds . decons

duration :: NonNeg.C time => T time body -> time
duration = NonNeg.sum . getTimes



cons :: time -> body -> T time body -> T time body
cons time body = lift (Uniform.cons time body)

snoc :: T time body -> body -> time -> T time body
snoc xs body time =
   Cons $ (Uniform.snoc $~~ xs) body time


viewL :: T time body -> (time, Maybe (body, T time body))
viewL =
   mapSnd (fmap (mapSnd Cons)) .
   Mixed.viewL .
   decons

{-# INLINE switchL #-}
switchL :: (time -> a) -> ((time, body) -> T time body -> a) -> T time body -> a
switchL f g =
   Mixed.switchL f (\t b -> g (t,b) . Cons) .
   decons

viewR :: T time body -> (Maybe (T time body, body), time)
viewR =
   mapFst (fmap (mapFst Cons)) . Mixed.viewR . decons

{-# INLINE switchR #-}
switchR :: (time -> a) -> (T time body -> body -> time -> a) -> T time body -> a
switchR f g =
   Mixed.switchR f (g . Cons) .
   decons


mapBody :: (body0 -> body1) -> T time body0 -> T time body1
mapBody = lift . Uniform.mapFirst

mapTime :: (time0 -> time1) -> T time0 body -> T time1 body
mapTime = lift . Uniform.mapSecond


zipWithBody ::
   (body0 -> body1 -> body2) ->
   [body0] -> T time body1 -> T time body2
zipWithBody f = lift . Uniform.zipWithFirst f

zipWithTime ::
   (time0 -> time1 -> time2) ->
   (time0, [time0]) -> T time1 body -> T time2 body
zipWithTime f = lift . Uniform.zipWithSecond f

unzip :: T time (body0, body1) -> (T time body0, T time body1)
unzip =
   foldr
      (\time ->
         mapPair (consTime time, consTime time))
      (\(body0, body1) ->
         mapPair (consBody body0, consBody body1))
      (mempty, mempty)


concatMapMonoid :: Monoid m =>
   (time -> m) -> (body -> m) ->
   T time body -> m
concatMapMonoid f g = Uniform.concatMapMonoid g f . decons

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



{- |
Sort coincident elements.
-}
normalize :: (Ord body, NonNeg.C time) =>
   T time body -> T time body
normalize = mapCoincident List.sort

isNormalized :: (NonNeg.C time, Ord body) =>
   T time body -> Bool
isNormalized =
   List.all isAscending . getBodies . collectCoincident


{- |
The first important function is 'merge'
which merges the events of two lists into a new time order list.
-}

merge :: (NonNeg.C time, Ord body) =>
   T time body -> T time body -> T time body
merge = mergeBy (<)

{-
Could be implemented using 'splitAt' and 'insert'.
-}
mergeBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   T time body -> T time body -> T time body
mergeBy before =
   let recourse xs0 ys0 =
          let (xt,xs) = viewTimeL xs0
              (yt,ys) = viewTimeL ys0
              (mt,~(bef,dt)) = NonNeg.split xt yt
          in  delay mt $
              if dt == zero
                then
                   case (viewBodyL xs, viewBodyL ys) of
                      (Nothing, _) -> consTime zero ys
                      (_, Nothing) -> consTime zero xs
                      (Just (b0,xs1), Just (b1,ys1)) ->
                         {-
                         do not insert both b0 and b1 immediately,
                         because the later one of b0 and b1 may be pushed even further,
                         thus recourse with 'mergeBy' on xs or ys
                         -}
                         if before b0 b1
                           then cons zero b0 $
                                recourse xs1 (consTime zero ys)
                           else cons zero b1 $
                                recourse (consTime zero xs) ys1
                else
                  if bef
                    then
                       let ys1 = consTime dt ys
                       in  flip (switchBodyL ys1) xs $ \ b xs1 ->
                              cons zero b $ recourse xs1 ys1
                    else
                       let xs1 = consTime dt xs
                       in  flip (switchBodyL xs1) ys $ \ b ys1 ->
                              cons zero b $ recourse xs1 ys1
   in  recourse


{- |
Note that 'merge' compares entire events rather than just start
times.  This is to ensure that it is commutative, a desirable
condition for some of the proofs used in Haskore/section equivalence.
It is also necessary to assert a unique representation
of the event list independent of the structure of the event type.
The same function for inserting into a time ordered list with a trailing pause.
-}
insert :: (NonNeg.C time, Ord body) =>
   time -> body -> T time body -> T time body
insert = insertBy (<)

{-
Ordering of bodies at the same time
could be simplified using collectCoincident.
-}
insertBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   time -> body -> T time body -> T time body
insertBy before t0 me0 =
   let recurseTime t =
          switchTimeL $ \ t1 xs0 ->
             let (mt,~(b,dt)) = NonNeg.split t1 t
             in  delay mt $
                 if not b
                   then cons zero me0 $ consTime dt xs0
                   else
                     switchBodyL
                        (cons dt me0 $ pause zero)
                        (\ me1 xs -> consTime zero $
                           if dt==zero && before me0 me1
                             then consBody me0 (cons zero me1 xs)
                             else consBody me1 (recurseTime dt xs))
                        xs0
   in   recurseTime t0


{-
Ensure that the list has a minimum length
by extending the last pause accordingly.
-}
pad :: (NonNeg.C time) =>
   time -> T time body -> T time body
pad time = mergeBy (\ _ _ -> False) (pause time)


{- |
Move events towards the front of the event list.
You must make sure, that no event is moved before time zero.
This works only for finite lists.
-}
moveForward :: (Ord time, Num time) =>
   T time (time, body) -> T time body
moveForward =
   fromAbsoluteEventList .
   AbsoluteEventList.moveForward .
   toAbsoluteEventList 0

moveBackward :: (NonNeg.C time) =>
   T time (time, body) -> T time body
moveBackward =
   catMaybes .
   foldr
      (\t -> cons t Nothing)
      (\(t,b) -> insertBy (ltMaybe (\_ _ -> True)) t (Just b))
      (pause zero)

{- |
Like 'moveForward' but restricts the look-ahead time.
For @moveForwardRestricted maxTimeDiff xs@
all time differences (aka the moveForward offsets) in @xs@
must be at most @maxTimeDiff@.
With this restriction the function is lazy enough
for handling infinite event lists.
However the larger @maxTimeDiff@ the more memory and time is consumed.
-}
{-
Implementation notes:
We keep a (non-optimized) priority queue as the state of a state monad.
In a pause we emit all events that occur in this duration.
-}
moveForwardRestricted :: (Ord body, NonNeg.C time) =>
   time -> T time (time, body) -> T time body
moveForwardRestricted maxTime =
   decreaseStart maxTime .
   moveBackward .
   mapBody (mapFst (maxTime-|)) .
   pad maxTime
{-
   moveForwardRestrictedBy
      (\_ _ -> True)
      -- (<)
-}

ltMaybe :: (body -> body -> Bool) -> (Maybe body -> Maybe body -> Bool)
ltMaybe cmp mx my =
   case (mx,my) of
      (Nothing, _) -> True
      (_, Nothing) -> False
      (Just x, Just y) -> cmp x y


-- | currently only for testing
moveForwardRestrictedBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   time -> T time (time, body) -> T time body
moveForwardRestrictedBy cmp maxTime =
   decreaseStart maxTime .
   catMaybes .
   foldr
      (\t -> cons t Nothing)
      (\(t,b) -> insertBy (ltMaybe cmp) (maxTime-|t) (Just b))
      (pause maxTime)

-- | currently only for testing
moveForwardRestrictedByStrict :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   time -> T time (time, body) -> T time body
moveForwardRestrictedByStrict cmp maxTime =
   decreaseStart maxTime .
   foldr
      delay
      (\(t,b) -> insertBy cmp (maxTime-|t) b)
      (pause maxTime)

-- | currently only for testing
moveForwardRestrictedByQueue :: (NonNeg.C time, Num time) =>
   (body -> body -> Bool) ->
   time -> T time (time, body) -> T time body
moveForwardRestrictedByQueue cmp maxTime xs =
   let (prefix,suffix) = splitAtTime maxTime xs
       prefixDur = duration prefix {- maxTime would work in most cases, too -}
       getChunk t =
          do (toEmit,toKeep) <- gets (splitAtTime t)
             put toKeep
             return (pad t toEmit)
       insertEvent (t,b) =
          insertBy cmp (maxTime - t) b
   in  evalState
          (foldr
             (\t m -> liftM2 append (getChunk t) m)
             (\b m -> modify (insertEvent b) >> m)
             (gets (pad prefixDur)) suffix)
          (moveForward (seq prefixDur prefix))
             {- this way 'prefixDur' will be computed early
                and 'prefix' need not to be stored until the end of the list -}


{- |
Merge several event lists respecting the start time of the outer event list.
-}
arrange :: (Ord body, NonNeg.C time) =>
   T time (T time body) -> T time body
arrange = arrangeBy (\_ _ -> True)

arrangeBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   T time (T time body) -> T time body
arrangeBy cmp =
   catMaybes .
   foldr
      (\t -> cons t Nothing)
      (\xs -> mergeBy (ltMaybe cmp) (mapBody Just xs))
      (pause zero)


concat :: (NonNeg.C time) =>
   [T time body] -> T time body
concat = mconcat

{- |
'concat' and 'concatNaive' are essentially the same.
'concat' must use 'foldr' in order to work on infinite lists,
however if there are many empty lists,
summing of their durations will be done from right to left,
which is inefficient.
Thus we detect subsequent empty lists and merge them from left to right.
-}
concatNaive :: (NonNeg.C time) =>
   [T time body] -> T time body
concatNaive = List.foldr append (pause zero)


{- |
Uses sharing.
-}
cycle :: (NonNeg.C time) =>
   T time body -> T time body
cycle =
   switchTimeL
   (\t0 xs ->
       consTime t0 $
       BodyTimePriv.cycle $
       BodyTimePriv.mapTimeLast (add t0) xs)


cycleNaive :: (NonNeg.C time) =>
   T time body -> T time body
cycleNaive = concat . List.repeat



{- |
If there is an event at the cutting time,
this event is returned in the suffix part.
That is
@splitAtTime t0 (t0 ./ x /. t1 ./ empty) ==
    (pause t0, 0 ./ x /. t1 ./ empty)@
-}
{-
It could also be implemented by inserting a marker element
and then splitting at this element.
I hope that the current manual recursion routine is the most efficient solution.
-}
splitAtTime :: (NonNeg.C time) =>
   time -> T time body -> (T time body, T time body)
splitAtTime t0 =
   switchTimeL
   (\t1 xs ->
      let (mt,~(bef,dt)) = NonNeg.split t0 t1
      in  {-
          The handling of the second pair member looks a bit cumbersome,
          but it is necessary to prepend the time once
          in order to prevent a memory leak.
          -}
          mapPair (consTime mt, forceTimeHead) $
          if bef
            then (mempty, consTime dt xs)
            else switchBodyL
                    (mempty, pause zero)
                    (\ b -> mapFst (consBody b) . splitAtTime dt)
                    xs)

takeTime :: (NonNeg.C time) =>
   time -> T time body -> T time body
takeTime t = fst . splitAtTime t

dropTime :: (NonNeg.C time) =>
   time -> T time body -> T time body
-- dropTime t = snd . splitAtTime t
dropTime t0 =
   switchTimeL
   (\t1 xs ->
      let (bef,dt) = snd $ NonNeg.split t0 t1
      in  forceTimeHead $
          if bef
            then consTime dt xs
            else switchBodyL
                    (pause zero)
                    (\ _b -> dropTime dt)
                    xs)

{-
Surprisingly this has a space leak,
see test dropTimeLazyInfinite.

dropTime :: (NonNeg.C time) =>
   time -> T time body -> T time body
dropTime t0 =
   switchTimeL
   (\t1 xs ->
      let (bef,dt) = snd $ NonNeg.split t0 t1
      in  if bef
            then consTime dt xs
            else switchBodyL
                    (pause zero)
                    (\ _b -> dropTime dt)
                    xs)
-}


decreaseStart :: (NonNeg.C time) =>
   time -> T time body -> T time body
decreaseStart dif =
   mapTimeHead (-| dif)


collectCoincident :: (NonNeg.C time) => T time body -> T time [body]
collectCoincident =
   mapTimeInit TimeBodyList.collectCoincident



mapCoincident :: (NonNeg.C time) =>
   ([a] -> [b]) -> T time a -> T time b
mapCoincident f =
   flatten . mapBody f . collectCoincident


{- |
Analogously to the 'concat' \/ 'concatNaive' pair
we have to versions of 'filter',
where the clever implementation sums up pauses
from the beginning to the end.
-}

filter :: (NonNeg.C time) =>
   (body -> Bool) ->
   T time body -> T time body
filter p = mapMaybe (\b -> toMaybe (p b) b)

mapMaybe :: (NonNeg.C time) =>
   (body0 -> Maybe body1) ->
   T time body0 -> T time body1
mapMaybe f = catMaybes . mapBody f

{- |
Adds times in a left-associative fashion.
Use this if the time is a strict data type.
-}
catMaybes :: (NonNeg.C time) =>
   T time (Maybe body) -> T time body
catMaybes = mapTime NonNeg.sum . lift Uniform.catMaybesFirst

{- |
Adds times in a right-associative fashion.
Use this if the time is a data type like lazy Peano numbers
or "Numeric.NonNegative.Chunky".
-}
catMaybesR :: (NonNeg.C time) =>
   T time (Maybe body) -> T time body
catMaybesR =
   foldr
      (mapTimeHead . add)
      (maybe id (cons zero))
      (pause zero)

partition :: (NonNeg.C time) =>
   (body -> Bool) ->
   T time body -> (T time body, T time body)
partition p =
   mapPair (mapTime NonNeg.sum, mapTime NonNeg.sum) .
   mapPair (Cons, Cons) .
   Uniform.partitionFirst p .
   decons

partitionMaybe :: (NonNeg.C time) =>
   (body0 -> Maybe body1) -> T time body0 ->
   (T time body1, T time body0)
partitionMaybe f =
   mapPair (mapTime NonNeg.sum . Cons, mapTime NonNeg.sum . Cons) .
   Uniform.partitionMaybeFirst f .
   decons

{- |
Cf. 'catMaybesR'
-}
partitionMaybeR :: (NonNeg.C time) =>
   (body0 -> Maybe body1) -> T time body0 ->
   (T time body1, T time body0)
partitionMaybeR f =
   mapPair
      (mapTime (List.foldr add zero),
       mapTime (List.foldr add zero)) .
   mapPair (Cons, Cons) .
   Uniform.partitionMaybeFirst f .
   decons

{- |
Since we need it later for MIDI generation,
we will also define a slicing into equivalence classes of events.
-}
slice :: (Eq a, NonNeg.C time) =>
   (body -> a) -> T time body -> [(a, T time body)]
slice = Utility.slice (fmap fst . viewBodyL . snd . viewTimeL) partition


foldl :: (a -> time -> b) -> (b -> body -> a) -> a -> T time body -> b
foldl f g x = Uniform.foldl g f x . decons

reverse :: T time body -> T time body
reverse = lift Uniform.reverse


discretize :: (NonNeg.C time, RealFrac time, NonNeg.C i, Integral i) =>
   T time body -> T i body
discretize =
   flip evalState 0.5 . mapTimeM floorDiff

resample :: (NonNeg.C time, RealFrac time, NonNeg.C i, Integral i) =>
   time -> T time body -> T i body
resample rate =
   discretize . mapTime (rate*)


toAbsoluteEventList :: (Num time) =>
   time -> T time body -> AbsoluteEventList.T time body
toAbsoluteEventList start =
   AbsoluteEventPriv.Cons . decons .
   flip evalState start .
   mapTimeM (\dur -> modify (dur+) >> get)

fromAbsoluteEventList :: (Num time) =>
   AbsoluteEventList.T time body -> T time body
fromAbsoluteEventList =
   flip evalState 0 .
   mapTimeM
      (\time -> do lastTime <- get; put time; return (time-lastTime)) .
   Cons . AbsoluteEventPriv.decons
