-- basic reactive functions that could as well be in reactive-banana
module Reactive.Banana.MIDI.Utility where

import qualified Reactive.Banana.Bunch.Combinators as RB

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM, liftM2, )

import Prelude hiding (sequence, )


partition ::
   (a -> Bool) -> RB.Event a -> (RB.Event a, RB.Event a)
partition p =
   (\x ->
      (fmap snd $ RB.filterE fst x,
       fmap snd $ RB.filterE (not . fst) x)) .
   fmap (\a -> (p a, a))

mapMaybe ::
   (a -> Maybe b) -> RB.Event a -> RB.Event b
mapMaybe f = RB.filterJust . fmap f

partitionMaybe ::
   (a -> Maybe b) -> RB.Event a -> (RB.Event b, RB.Event a)
partitionMaybe f =
   (\x ->
      (mapMaybe fst x,
       mapMaybe (\(mb,a) -> maybe (Just a) (const Nothing) mb) x)) .
   fmap (\a -> (f a, a))

bypass ::
   (a -> Maybe b) ->
   (RB.Event a -> RB.Event c) ->
   (RB.Event b -> RB.Event c) ->
   RB.Event a -> RB.Event c
bypass p fa fb evs =
   let (eb,ea) = partitionMaybe p evs
   in  RB.union (fb eb) (fa ea)

bypassM ::
   (Monad m) =>
   (a -> Maybe b) ->
   (RB.Event a -> m (RB.Event c)) ->
   (RB.Event b -> m (RB.Event c)) ->
   RB.Event a -> m (RB.Event c)
bypassM p fa fb evs =
   let (eb,ea) = partitionMaybe p evs
   in  liftM2 RB.union (fb eb) (fa ea)

traverse ::
   (RB.MonadMoment m) =>
   s -> (a -> MS.State s b) -> RB.Event a ->
   m (RB.Event b, RB.Behavior s)
traverse s f = sequence s . fmap f

sequence ::
   (RB.MonadMoment m) =>
   s -> RB.Event (MS.State s a) ->
   m (RB.Event a, RB.Behavior s)
sequence s =
   RB.mapAccum s . fmap MS.runState


mapAdjacent ::
   (RB.MonadMoment m) => (a -> a -> b) -> a -> RB.Event a -> m (RB.Event b)
mapAdjacent f a0 =
   liftM fst . RB.mapAccum a0 . fmap (\new old -> (f old new, new))
