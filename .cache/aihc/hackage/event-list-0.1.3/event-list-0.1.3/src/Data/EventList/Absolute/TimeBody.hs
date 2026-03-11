{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Absolute.TimeBody
   (T,
    empty, singleton, null,
    viewL, viewR, switchL, switchR, cons, snoc,
    fromPairList, toPairList,
    getTimes, getBodies, duration,
    mapBody, mapTime,
    concatMapMonoid,
    traverse, traverse_, traverseBody, traverseTime, traverseWithTime,
    mapM, mapM_, mapBodyM, mapTimeM,
    merge, mergeBy, insert, insertBy,
    moveForward,
    decreaseStart, delay, filter, partition, partitionMaybe,
    slice, foldr, foldrPair,
    mapMaybe, catMaybes,
    normalize, isNormalized,
    collectCoincident, flatten, mapCoincident,
    append, concat, cycle,
--    splitAtTime, takeTime, dropTime,
    discretize, resample,
    checkTimes,

    collectCoincidentFoldr, collectCoincidentNonLazy, -- for testing
   ) where

import Data.EventList.Absolute.TimeBodyPrivate

import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Data.EventList.Utility as Utility
import qualified Data.Traversable as Trav
import qualified Data.List as List

import Data.Monoid (Monoid, )

import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Maybe.HT (toMaybe, )
import Data.List.HT (isAscending, isAscendingLazy, )
import Data.Function.HT (compose2, )
import Data.EventList.Utility (beforeBy, )

import qualified Control.Monad as Monad
import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), )

import Control.Monad.Trans.State (state, evalState)
import Control.Monad (Monad, (>>), )

import Data.Function (id, flip, (.), ($), )
import Data.Functor (fmap, )
import Data.Maybe (Maybe(Just, Nothing), maybe, )
import Data.Tuple (uncurry, fst, snd, )
import Data.Ord (Ord, compare, (<), (>=), )
import Data.Eq (Eq, (==), (/=), )
import Prelude
   (Num, Integral, RealFrac, round, subtract, (*), (-),
    Bool, error, )


empty :: T time body
empty = Cons $ Disp.empty

null :: T time body -> Bool
null = Disp.null . decons

singleton :: time -> body -> T time body
singleton time body = Cons $ Disp.singleton time body


cons :: time -> body -> T time body -> T time body
cons time body = lift (Disp.cons time body)

snoc :: T time body -> time -> body -> T time body
snoc xs time body =
   Cons $ (Disp.snoc $~ xs) time body
--   lift (\ys -> Disp.snoc ys time body) xs


viewL :: T time body -> Maybe ((time, body), T time body)
viewL = fmap (mapSnd Cons) . Disp.viewL . decons

viewR :: T time body -> Maybe (T time body, (time, body))
viewR = fmap (mapFst Cons) . Disp.viewR . decons


fromPairList :: [(a,b)] -> T a b
fromPairList = Cons . Disp.fromPairList

toPairList :: T a b -> [(a,b)]
toPairList = Disp.toPairList . decons

getBodies :: T time body -> [body]
getBodies = Disp.getSeconds . decons

getTimes :: T time body -> [time]
getTimes = Disp.getFirsts . decons




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


traverseWithTime :: Applicative m =>
   (time -> body0 -> m body1) -> T time body0 -> m (T time body1)
traverseWithTime f =
   fmap fromPairList .
   Trav.traverse (\(t,b) -> fmap ((,) t) (f t b)) .
   toPairList


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
Check whether time values are in ascending order.
The list is processed lazily and
times that are smaller than there predecessors are replaced by 'undefined'.
If you would remove the 'undefined' times from the resulting list
the times may still not be ordered.
E.g. consider the time list @[0,3,1,2]@
-}
checkTimes :: (Ord time) => T time body -> T time body
checkTimes xs =
   lift
      (Disp.zipWithFirst
         (\b t -> if b then t else error "times out of order")
         (isAscendingLazy (getTimes xs)))
      xs


foldr :: (time -> a -> b) -> (body -> b -> a) -> b -> T time body -> b
foldr f g x = Disp.foldr f g x . decons

foldrPair :: (time -> body -> a -> a) -> a -> T time body -> a
foldrPair f x = Disp.foldrPair f x . decons


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
   foldrPair (maybe id . cons) empty

{-
Could be implemented more easily in terms of Uniform.partition
-}
partition ::
   (body -> Bool) -> T time body -> (T time body, T time body)
partition p =
   foldrPair
      (\ t b ->
          (if p b then mapFst else mapSnd) (cons t b))
      (empty, empty)

partitionMaybe ::
   (body0 -> Maybe body1) -> T time body0 -> (T time body1, T time body0)
partitionMaybe p =
   foldrPair
      (\ t b ->
          maybe (mapSnd (cons t b)) (mapFst . cons t) (p b))
      (empty, empty)

{- |
Since we need it later for MIDI generation,
we will also define a slicing into equivalence classes of events.
-}
slice :: (Eq a) =>
   (body -> a) -> T time body -> [(a, T time body)]
slice = Utility.slice (fmap (snd . fst) . viewL) partition


{- |
We will also sometimes need a function which groups events by equal start times.
This implementation is not so obvious since we work with time differences.
The criterion is: Two neighbouring events start at the same time
if the second one has zero time difference.
-}
collectCoincident :: Eq time => T time body -> T time [body]
collectCoincident =
   Cons .
   Mixed.switchFirstL
      Disp.empty
      (\ t0 ->
         Mixed.consFirst t0 .
         Uniform.catMaybesFirst .
         flip evalState (Just t0) .
         Uniform.traverseFirst (\time -> state $ \ oldTime ->
            (Monad.guard (time /= oldTime) >> time, time)) .
         Uniform.mapFirst Just) .
   decons

collectCoincidentFoldr :: Eq time => T time body -> T time [body]
collectCoincidentFoldr =
   Cons .
   foldrPair
      (\t0 b0 xs ->
          Mixed.consFirst t0 $
          Disp.switchL
             (Uniform.singleton [b0])
             (\t1 bs ys ->
                 if t0 == t1
                   then Mixed.consSecond (b0:bs) ys
                   else Mixed.consSecond [b0] xs)
             xs)
      Disp.empty

{- |
Will fail on infinite lists.
-}
collectCoincidentNonLazy :: Eq time => T time body -> T time [body]
collectCoincidentNonLazy =
   Cons .
   foldrPair
      (\t0 b0 xs ->
          Disp.switchL
             (Disp.singleton t0 [b0])
             (\t1 bs ys ->
                 if t0 == t1
                   then Disp.cons t0 (b0:bs) ys
                   else Disp.cons t0 [b0] xs)
             xs)
      Disp.empty


flatten :: (Ord time) => T time [body] -> T time body
flatten =
   foldrPair
      (\t bs xs -> List.foldr (cons t) xs bs)
      empty


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


{- |
The first important function is 'merge'
which merges the events of two lists into a new time order list.
-}

merge :: (Ord time, Ord body) =>
   T time body -> T time body -> T time body
merge = mergeBy (<)

{- |
Note that 'merge' compares entire events rather than just start
times.  This is to ensure that it is commutative, a desirable
condition for some of the proofs used in \secref{equivalence}.
It is also necessary to assert a unique representation
of the performance independent of the structure of the 'Music.T note'.
The same function for inserting into a time ordered list with a trailing pause.
The strictness annotation is necessary for working with infinite lists.

Here are two other functions that are already known for non-padded time lists.
-}

{-
Could be implemented using as 'splitAt' and 'insert'.
-}
mergeBy :: (Ord time) =>
   (body -> body -> Bool) ->
   T time body -> T time body -> T time body
mergeBy before =
   let recourse xs0 ys0 =
          case (viewL xs0, viewL ys0) of
             (Nothing, _) -> ys0
             (_, Nothing) -> xs0
             (Just (x,xs), Just (y,ys)) ->
                if beforeBy before x y
                  then uncurry cons x $ recourse xs ys0
                  else uncurry cons y $ recourse ys xs0
   in  recourse

{- |
The final critical function is @insert@,
which inserts an event
into an already time-ordered sequence of events.
For instance it is used in MidiFiles to insert a @NoteOff@ event
into a list of @NoteOn@ and @NoteOff@ events.
-}

insert :: (Ord time, Ord body) =>
   time -> body -> T time body -> T time body
insert = insertBy (<)


insertBy :: (Ord time) =>
   (body -> body -> Bool) ->
   time -> body -> T time body -> T time body
insertBy before t0 me0 mevs1 =
   let mev0 = (t0, me0)
   in  switchL
          (uncurry singleton mev0)
          (\mev1 mevs ->
              if beforeBy before mev0 mev1
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
   fromPairList .
   List.sortBy (compose2 compare fst) .
   List.map (\ ~(time,(timeDiff,body)) -> (time - timeDiff, body)) .
   toPairList




{-
splitAtTime :: (Ord time, Num time) =>
   time -> T time body -> (Uniform.T body time, T time body)
splitAtTime t0 =
   maybe
      (Uniform.singleton 0, empty)
      (\(t1,xs) ->
          if t0<=t1
            then (Uniform.singleton t0, consTime (t1-t0) xs)
            else
               (\(b,ys) -> mapFst (Uniform.cons t1 b) (splitAtTime (t0-t1) ys))
               (viewBodyL xs)) .
   viewTimeL

takeTime :: (Ord time, Num time) =>
   time -> T time body -> Uniform.T body time
takeTime t = fst . splitAtTime t

dropTime :: (Ord time, Num time) =>
   time -> T time body -> T time body
dropTime t = snd . splitAtTime t
-}


decreaseStart :: (Ord time, Num time) =>
   time -> T time body -> T time body
decreaseStart dif =
   switchL
      empty
      (\(t, b) xs ->
         cons
            (if t>=dif
               then t-dif
               else error "decreaseStart: difference too big") b
            (mapTime (subtract dif) xs))


{- |

Here are some functions for discretizing the time information.
When converting the precise relative event times
to the integer relative event times
we have to prevent accumulation of rounding errors.
We avoid this problem with a stateful conversion
which remembers each rounding error we make.
This rounding error is used to correct the next rounding.
Given the relative time and duration of a note
the function @discretizeEventM@ creates a @State@
which computes the rounded relative time.
It is corrected by previous rounding errors.

The resulting event list may have differing time differences
which were equal before discretization,
but the overall timing is uniformly close to the original.

-}

discretize :: (RealFrac time, Integral i) =>
   T time body -> T i body
discretize = mapTime round

resample :: (RealFrac time, Integral i) =>
   time -> T time body -> T i body
resample rate = discretize . mapTime (rate*)
