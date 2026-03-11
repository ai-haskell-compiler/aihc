{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a time difference and ending with a body.


The time is stored in differences between the events.
Thus there is no increase of time information for long,
or even infinite, streams of events.
Further on, the time difference is stored
in the latter of two neighbouring events.
This is necessary for real-time computing
where it is not known whether and when the next event happens.

-}
module Data.EventList.Relative.TimeBody
   (T,
    empty, singleton, null,
    viewL, viewR, switchL, switchR, cons, snoc,
    fromPairList, toPairList,
    getTimes, getBodies, duration,
    mapBody, mapTime,
    zipWithBody, zipWithTime, unzip,
    concatMapMonoid,
    traverse, traverse_, traverseBody, traverseTime,
    mapM, mapM_, mapBodyM, mapTimeM,
    foldr, foldrPair,
    merge, mergeBy, insert, insertBy,
    moveForward,
    decreaseStart, delay, filter, partition, partitionMaybe, slice, span,
    mapMaybe, catMaybes,
    normalize, isNormalized,
    collectCoincident, flatten, mapCoincident,
    append, concat, cycle,
    discretize, resample,
    toAbsoluteEventList, fromAbsoluteEventList,
    toAbsoluteEventListGen, fromAbsoluteEventListGen,
   ) where

import Data.EventList.Relative.TimeBodyPrivate
import qualified Data.EventList.Relative.BodyBodyPrivate as BodyBodyPriv

import qualified Data.EventList.Absolute.TimeBodyPrivate as AbsoluteEventPriv
import qualified Data.EventList.Absolute.TimeBody as AbsoluteEventList

import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Data.List as List
import qualified Data.EventList.Utility as Utility

import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), )
import Data.Monoid (Monoid, )

import qualified Numeric.NonNegative.Class as NonNeg
import Numeric.NonNegative.Class ((-|), zero, add, )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.List.HT (isAscending, )
import Control.Monad.Trans.State (evalState, modify, get, put, )
import Control.Monad (Monad, return, (>>), )

import Data.Function (flip, const, (.), ($), )
import Data.Functor (fmap, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Bool (Bool, not, (||), (&&), )
import Data.Tuple (uncurry, fst, snd, )
import Data.Ord (Ord, (<), )
import Data.Eq (Eq, (/=), )
import Prelude (Num, Integral, RealFrac, (*), (+), (-), error, )



empty :: T time body
empty = Cons Disp.empty

null :: T time body -> Bool
null = Disp.null . decons

singleton :: time -> body -> T time body
singleton time body = Cons $ Disp.singleton time body


cons :: time -> body -> T time body -> T time body
cons time body = lift (Disp.cons time body)

snoc :: T time body -> time -> body -> T time body
snoc xs time body = Cons $ (Disp.snoc $~* xs) time body



viewL :: T time body -> Maybe ((time, body), T time body)
viewL = fmap (mapSnd Cons) . Disp.viewL . decons

viewR :: T time body -> Maybe (T time body, (time, body))
viewR = fmap (mapFst Cons) . Disp.viewR . decons


{-# INLINE switchL #-}
switchL :: c -> ((time, body) -> T time body -> c) -> T time body -> c
switchL f g = Disp.switchL f (\ t b  -> g (t,b) . Cons) . decons

{-# INLINE switchR #-}
switchR :: c -> (T time body -> (time, body) -> c) -> T time body -> c
switchR f g = Disp.switchR f (\xs t b -> g (Cons xs) (t,b)) . decons



fromPairList :: [(a,b)] -> T a b
fromPairList = Cons . Disp.fromPairList

toPairList :: T a b -> [(a,b)]
toPairList = Disp.toPairList . decons

getBodies :: T time body -> [body]
getBodies = Disp.getSeconds . decons

getTimes :: T time body -> [time]
getTimes = Disp.getFirsts . decons

duration :: NonNeg.C time => T time body -> time
duration = NonNeg.sum . getTimes



mapBody :: (body0 -> body1) -> T time body0 -> T time body1
mapBody f = lift (Disp.mapSecond f)

mapTime :: (time0 -> time1) -> T time0 body -> T time1 body
mapTime f = lift (Disp.mapFirst f)


zipWithBody ::
   (body0 -> body1 -> body2) ->
   [body0] -> T time body1 -> T time body2
zipWithBody f = lift . Disp.zipWithSecond f

zipWithTime ::
   (time0 -> time1 -> time2) ->
   [time0] -> T time1 body -> T time2 body
zipWithTime f = lift . Disp.zipWithFirst f


unzip :: T time (body0, body1) -> (T time body0, T time body1)
unzip =
   foldrPair
      (\time (body0, body1) ->
         mapPair (cons time body0, cons time body1))
      (empty, empty)



concatMapMonoid :: Monoid m =>
   (time -> m) -> (body -> m) ->
   T time body -> m
concatMapMonoid f g =
   Disp.concatMapMonoid f g . decons


traverse :: Applicative m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
traverse f g = liftA (Disp.traverse f g)

traverse_ :: Applicative m =>
   (time -> m ()) -> (body -> m ()) ->
   T time body -> m ()
traverse_ f g = Disp.traverse_ f g . decons


traverseBody :: Applicative m =>
   (body0 -> m body1) -> T time body0 -> m (T time body1)
traverseBody f = liftA (Disp.traverseSecond f)

traverseTime :: Applicative m =>
   (time0 -> m time1) -> T time0 body -> m (T time1 body)
traverseTime f = liftA (Disp.traverseFirst f)


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




foldr :: (time -> a -> b) -> (body -> b -> a) -> b -> T time body -> b
foldr f g x = Disp.foldr f g x . decons

foldrPair :: (time -> body -> a -> a) -> a -> T time body -> a
foldrPair f x = Disp.foldrPair f x . decons


{- |
Keep only events that match a predicate while preserving absolute times.
-}
filter :: (NonNeg.C time) =>
   (body -> Bool) -> T time body -> T time body
filter p = mapMaybe (\b -> toMaybe (p b) b)
-- filter p = fst . partition p

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
catMaybes =
   Cons .
   fst . Mixed.viewSecondR .
   Uniform.mapSecond NonNeg.sum .
   Uniform.catMaybesFirst .
   flip Mixed.snocSecond (error "catMaybes: no trailing time") .
   decons

{-
The function 'partition' is somehow the inverse to 'merge'.
It is similar to 'List.partition'.
We could use the List function if the event times would be absolute,
because then the events need not to be altered on splits.
But absolute time points can't be used for infinite music
thus we take the burden of adapting the time differences
when an event is removed from the performance list and
put to the list of events of a particular instrument.
@t0@ is the time gone since the last event in the first partition,
@t1@ is the time gone since the last event in the second partition.

Note, that using 'Data.EventList.Utility.mapPair' we circumvent the following problem:
Since the recursive call to 'partition'
may end up with Bottom,
pattern matching with, say \expression{(es0,es1)},
will halt the bounding of the variables
until the most inner call to 'partition' is finished.
This never happens.
If the pair constructor is made strict,
that is we write \expression{~(es0,es1)},
then everything works.
Also avoiding pattern matching and
using 'fst' and 'snd' would help.

-}

{-
Could be implemented more easily in terms of Uniform.partition
-}
partition :: (NonNeg.C time) =>
   (body -> Bool) -> T time body -> (T time body, T time body)
partition p = partitionRec p zero zero

partitionRec :: (NonNeg.C time) =>
   (body -> Bool) -> time -> time ->
       T time body -> (T time body, T time body)
partitionRec p =
   let recourse t0 t1 =
          switchL
             (empty, empty)
             (\ (t, b) es ->
                let t0' = add t0 t
                    t1' = add t1 t
                in  if p b
                      then mapFst (cons t0' b) (recourse zero t1' es)
                      else mapSnd (cons t1' b) (recourse t0' zero es))
   in  recourse

partitionMaybe :: (NonNeg.C time) =>
   (body0 -> Maybe body1) -> T time body0 ->
   (T time body1, T time body0)
partitionMaybe f =
   mapPair (catMaybes, catMaybes) .
   foldrPair (\t a ->
      let mb = f a
          a1 = maybe (Just a) (const Nothing) mb
      in  mapPair (cons t mb, cons t a1))
      (empty, empty)

{- |
Using a classification function
we splice the event list into lists, each containing the same class.
Absolute time stamps are preserved.
-}
slice :: (Eq a, NonNeg.C time) =>
   (body -> a) -> T time body -> [(a, T time body)]
slice = Utility.slice (fmap (snd . fst) . viewL) partition


span :: (body -> Bool) -> T time body -> (T time body, T time body)
span p = mapPair (Cons, Cons) . Disp.spanSecond p . decons


{- |
Group events that have equal start times
(that is zero time differences).
-}
collectCoincident :: (NonNeg.C time) => T time body -> T time [body]
collectCoincident =
   mapTimeTail $ BodyBodyPriv.lift $ Uniform.filterFirst (zero <)

{- |
Reverse to collectCoincident:
Turn each @body@ into a separate event.

>   xs  ==  flatten (collectCoincident xs)
-}
flatten :: (NonNeg.C time) => T time [body] -> T time body
flatten =
   Cons .
   Mixed.switchFirstL
      Disp.empty
      (\time ->
         unlift (delay time) .
         fst . Mixed.viewSecondR .
         Uniform.foldr
            (Mixed.appendUniformUniform . Uniform.fromSecondList zero)
            Mixed.consSecond Disp.empty .
         Uniform.mapSecond NonNeg.sum .
         Uniform.filterSecond (not . List.null)) .
   decons


{- |
Apply a function to the lists of coincident events.
-}
mapCoincident :: (NonNeg.C time) =>
   ([a] -> [b]) -> T time a -> T time b
mapCoincident f = flatten . mapBody f . collectCoincident

{- |
'List.sort' sorts a list of coinciding events,
that is all events but the first one have time difference 0.
'normalize' sorts all coinciding events in a list
thus yielding a canonical representation of a time ordered list.
-}
normalize :: (NonNeg.C time, Ord body) => T time body -> T time body
normalize = mapCoincident List.sort

isNormalized :: (NonNeg.C time, Ord body) =>
   T time body -> Bool
isNormalized =
   List.all isAscending . getBodies . collectCoincident



{- |
This function merges the events of two lists into a new event list.
Note that 'merge' compares entire events rather than just start times.
This is to ensure that it is commutative,
one of the properties we test for.
-}
merge :: (NonNeg.C time, Ord body) =>
   T time body -> T time body -> T time body
merge = mergeBy (<)

{- |
'mergeBy' is like 'merge' but does not simply use the methods of the 'Ord' class
but allows a custom comparison function.
If in event lists @xs@ and @ys@ there are coinciding elements @x@ and @y@,
and @cmp x y@ is 'True',
then @x@ comes before @y@ in @mergeBy cmp xs ys@.

> EventList> EventList.mergeBy (\_ _ -> True) (0 /. 'a' ./ empty) (0 /. 'b' ./ empty)
> 0 /. 'a' ./ 0 /. 'b' ./ empty
>
> EventList> EventList.mergeBy (\_ _ -> False) (0 /. 'a' ./ empty) (0 /. 'b' ./ empty)
> 0 /. 'b' ./ 0 /. 'a' ./ empty
-}

{-
Could be implemented using 'splitAt' and 'insert'.
-}
mergeBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   T time body -> T time body -> T time body
mergeBy before =
   let recourse xs0 ys0 =
          case (viewL xs0, viewL ys0) of
             (Nothing, _) -> ys0
             (_, Nothing) -> xs0
             (Just ((xt,xb),xs), Just ((yt,yb),ys)) ->
                let (mt,~(b,dt)) = NonNeg.split xt yt
                in  uncurry (cons mt) $
                    if b && (dt/=zero || before xb yb)
                      then (xb, recourse xs $ cons dt yb ys)
                      else (yb, recourse ys $ cons dt xb xs)
   in  recourse


{- |
'insert' inserts an event into an event list at the given time.
-}
insert :: (NonNeg.C time, Ord body) =>
   time -> body -> T time body -> T time body
insert = insertBy (<)


insertBy :: (NonNeg.C time) =>
   (body -> body -> Bool) ->
   time -> body -> T time body -> T time body
insertBy before =
   let recourse t0 me0 =
          (\ ~((t,me), rest) -> cons t me rest) .
          switchL
             ((t0,me0), empty)
             (\(t1, me1) mevs ->
                let (mt,~(b,dt)) = NonNeg.split t0 t1
                in  mapFst ((,) mt) $
                    if b && (dt/=zero || before me0 me1)
                      then (me0, cons     dt me1 mevs)
                      else (me1, recourse dt me0 mevs))
   in  recourse


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


{-
Like 'moveForward' but restricts the look-ahead time.
For @moveForwardRestricted maxTimeDiff xs@
all time differences (aka the moveForward offsets) in @xs@
must be at most @maxTimeDiff@.
With this restriction the function is lazy enough
for handling infinite event lists.
However the larger @maxTimeDiff@ the more memory and time is consumed.
-}
{- for implementation notes see TimeTime

This implementation requires TimeTime.duration, TimeMixed.appendBodyEnd, TimeMixed.splitAtTime
and thus we would need a lot of movement of functions between modules

moveForwardRestricted :: (NonNeg.C time) =>
   time -> T time (time, body) -> T time body
moveForwardRestricted maxTime xs =
   let (prefix,suffix) = splitAtTime maxTime xs
       prefixDur = duration prefix
       getChunk t =
          do (toEmit,toKeep) <- gets (splitAtTime t)
             put toKeep
             return (pad t toEmit)
       insertEvent (t,b) =
          insertBy (\ _ _ -> False) (maxTime - t) b
   in  evalState
          (foldr
             (\t m -> liftM2 append (getChunk t) m)
             (\b m -> modify (insertEvent b) >> m)
             (gets (pad prefixDur)) suffix)
          (moveForward (seq prefixDur prefix))
-}



append :: T time body -> T time body -> T time body
append xs = lift (Disp.append $~* xs)

concat :: [T time body] -> T time body
concat = Cons . Disp.concat . List.map decons

cycle :: T time body -> T time body
cycle = lift Disp.cycle



decreaseStart :: (NonNeg.C time) =>
   time -> T time body -> T time body
decreaseStart dif =
   mapTimeHead (-| dif)

delay :: (NonNeg.C time) =>
   time -> T time body -> T time body
delay dif =
   mapTimeHead (add dif)



{- |
We provide 'discretize' and 'resample' for discretizing the time information.
When converting the precise relative event times
to the integer relative event times
we have to prevent accumulation of rounding errors.
We avoid this problem with a stateful conversion
which remembers each rounding error we make.
This rounding error is used to correct the next rounding.
Given the relative time and duration of an event
the function 'floorDiff' creates a 'Control.Monad.State.State'
which computes the rounded relative time.
It is corrected by previous rounding errors.

The resulting event list may have differing time differences
which were equal before discretization,
but the overall timing is uniformly close to the original.

We use 'floorDiff' rather than 'Utility.roundDiff'
in order to compute exclusively with non-negative numbers.
-}

discretize :: (NonNeg.C time, RealFrac time, NonNeg.C i, Integral i) =>
   T time body -> T i body
discretize =
   flip evalState 0.5 . mapTimeM Utility.floorDiff

resample :: (NonNeg.C time, RealFrac time, NonNeg.C i, Integral i) =>
   time -> T time body -> T i body
resample rate =
   discretize . mapTime (rate*)


{- |
We tried hard to compute everything with respect to relative times.
However sometimes we need absolute time values.
-}
toAbsoluteEventList :: (Num time) =>
   time -> T time body -> AbsoluteEventList.T time body
toAbsoluteEventList = toAbsoluteEventListGen (+)

fromAbsoluteEventList :: (Num time) =>
   AbsoluteEventList.T time body -> T time body
fromAbsoluteEventList = fromAbsoluteEventListGen (-) 0

{- |
Convert from relative time stamps to absolute time stamps
using a custom accumulator function (like @(+)@).
-}
toAbsoluteEventListGen ::
   (absTime -> relTime -> absTime) ->
   absTime -> T relTime body -> AbsoluteEventList.T absTime body
toAbsoluteEventListGen accum start =
   AbsoluteEventPriv.Cons . decons .
   flip evalState start .
   mapTimeM (\dur -> modify (flip accum dur) >> get)

{- |
Convert from absolute time stamps to relative time stamps
using custom subtraction (like @(-)@) and zero.
-}
fromAbsoluteEventListGen ::
   (absTime -> absTime -> relTime) ->
   absTime ->
   AbsoluteEventList.T absTime body -> T relTime body
fromAbsoluteEventListGen diff start =
   flip evalState start .
   mapTimeM
      (\time -> do lastTime <- get; put time; return (diff time lastTime)) .
   Cons . AbsoluteEventPriv.decons
