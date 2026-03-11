module Reactive.Banana.Bunch.Combinators (
   Event,
   Behavior,
   MonadMoment(liftMoment),
   apply,
   (<@>),
   union,
   filterE,
   filterJust,
   accumB,
   accumE,
   mapAccum,
   stepper,
   RB.valueBLater,

   collect,
   spill,
   ) where

import qualified Reactive.Banana.Combinators as RB
import Reactive.Banana.Bunch.Private (Event(Event))
import Reactive.Banana.Combinators (MonadMoment, Behavior)

import Control.Monad (liftM, join)
import Control.Applicative ((<$>))

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Traversable as Trav
import Data.Maybe (catMaybes)
import Data.Tuple.HT (swap, mapFst)


infixl 4 <@>


(<@>), apply :: Behavior (a -> b) -> Event a -> Event b
(<@>) = apply

apply fs (Event ass) = Event $ fmap <$> fs RB.<@> ass

union :: Event a -> Event a -> Event a
union (Event xs) (Event ys) = Event $ RB.unionWith NonEmptyC.append xs ys

filterE :: (a -> Bool) -> Event a -> Event a
filterE = filterGen . filter

filterJust :: Event (Maybe a) -> Event a
filterJust = filterGen catMaybes

filterGen :: ([a] -> [b]) -> Event a -> Event b
filterGen f (Event xs) =
   Event $ RB.filterJust $ fmap (NonEmpty.fetch . f . NonEmpty.flatten) xs

stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper a (Event as) = RB.stepper a $ NonEmpty.last <$> as

accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
accumE acc (Event fss) = liftM (Event . fst) $ mapAccumGen double acc fss

accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a)
accumB acc (Event fss) = liftM snd $ mapAccumGen double acc fss

double :: a -> (a,a)
double a = (a,a)

mapAccum ::
   MonadMoment m => acc -> Event (acc -> (x, acc)) -> m (Event x, Behavior acc)
mapAccum acc (Event fss) = liftM (mapFst Event) $ mapAccumGen swap acc fss

mapAccumGen ::
   (MonadMoment m, Trav.Traversable t) =>
   (s -> (acc, a)) -> acc ->
   RB.Event (t (acc -> s)) -> m (RB.Event (t a), Behavior acc)
mapAccumGen g acc =
   RB.mapAccum acc .
   fmap (\fs a -> swap $ Trav.mapAccumL (\a0 f -> g $ f a0) a fs)


collect :: Event a -> Event (NonEmpty.T [] a)
collect (Event as) = Event (NonEmpty.singleton <$> as)

spill :: Event (NonEmpty.T [] a) -> Event a
spill (Event as) = Event (join <$> as)
