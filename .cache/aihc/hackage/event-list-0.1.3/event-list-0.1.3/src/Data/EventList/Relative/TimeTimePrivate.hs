{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Relative.TimeTimePrivate where

import qualified Data.EventList.Relative.TimeBodyPrivate as TimeBodyList
import qualified Data.EventList.Relative.TimeBodyPrivate as TimeBodyPriv

import qualified Data.EventList.Relative.BodyTimePrivate as BodyTimeList
import qualified Data.EventList.Relative.BodyTimePrivate as BodyTimePriv

import Data.EventList.Relative.TimeBodyPrivate (($~*))

import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Numeric.NonNegative.Class as NonNeg
import Numeric.NonNegative.Class (zero, add, )

import Data.Tuple.HT (mapFst, mapSnd, )

import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import qualified Control.Applicative as App
import Control.Applicative (Applicative, )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>), )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import Prelude hiding (foldr, )


newtype T time body = Cons {decons :: Uniform.T body time}
   deriving (Eq, Ord)


instance (Show time, Show body) => Show (T time body) where
   showsPrec p = Uniform.format " ./ " " /. " p . decons

instance (Arbitrary time, Arbitrary body) =>
             Arbitrary (T time body) where
   arbitrary = Monad.liftM Cons arbitrary
   shrink = liftM shrink


instance (NonNeg.C time) => Semigroup (T time body) where
   (<>) = append

instance (NonNeg.C time) => Monoid (T time body) where
   mempty = Cons (Uniform.singleton zero)
   mappend = (<>)
   mconcat =
      flatten . consTime zero .
      mconcat .
      map (consBody [] . fmap (:[]))

append, appendAlt, appendSwitch ::
   (NonNeg.C time) =>
   T time body -> T time body -> T time body
append xs ys =
   forceTimeHead $
   foldr
      delay
      (\b ->
         consTime NonNeg.zero .
         consBody b)
      ys xs

appendAlt xs ys =
   foldr
      (\t ->
         delay t .
         either id (consTime NonNeg.zero))
      (\b -> Right . consBody b)
      (Left ys) xs

{-
not lazy enough for @append (2 /. 'a' ./ 4 /. 'b' ./ 2 /. undefined) undefined@
-}
appendSwitch =
   switchTimeR
      (\ xs t ->
         lift (Mixed.appendDisparateUniform $~* xs) .
         delay t)


instance Functor (T time) where
   fmap f (Cons x) = Cons (Uniform.mapFirst f x)

instance Fold.Foldable (T time) where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable (T time) where
   traverse f =
      App.liftA Cons . Uniform.traverse f App.pure . decons


infixl 5 $~~

($~~) :: (Uniform.T body time -> a) -> (T time body -> a)
($~~) f = f . decons


lift ::
   (Uniform.T body0 time0 -> Uniform.T body1 time1) ->
   (T time0 body0 -> T time1 body1)
lift f = Cons . f . decons

liftA :: Applicative m =>
   (Uniform.T body0 time0 -> m (Uniform.T body1 time1)) ->
   (T time0 body0 -> m (T time1 body1))
liftA f = App.liftA Cons . f . decons

liftM :: Monad m =>
   (Uniform.T body0 time0 -> m (Uniform.T body1 time1)) ->
   (T time0 body0 -> m (T time1 body1))
liftM f = Monad.liftM Cons . f . decons

unlift ::
   (T time0 body0 -> T time1 body1) ->
   (Uniform.T body0 time0 -> Uniform.T body1 time1)
unlift f = decons . f . Cons




consBody :: body -> T time body -> BodyTimeList.T time body
consBody b = BodyTimePriv.Cons . Mixed.consFirst b . decons

consTime :: time -> BodyTimeList.T time body -> T time body
consTime t = Cons . Mixed.consSecond t . BodyTimePriv.decons


viewTimeL :: T time body -> (time, BodyTimeList.T time body)
viewTimeL = mapSnd BodyTimePriv.Cons . Mixed.viewSecondL . decons

viewBodyL :: BodyTimeList.T time body -> Maybe (body, T time body)
viewBodyL = fmap (mapSnd Cons) . Mixed.viewFirstL . BodyTimePriv.decons


viewTimeR :: T time body -> (TimeBodyList.T time body, time)
viewTimeR = mapFst TimeBodyPriv.Cons . Mixed.viewSecondR . decons

viewBodyR :: TimeBodyList.T time body -> Maybe (T time body, body)
viewBodyR = fmap (mapFst Cons) . Mixed.viewFirstR . TimeBodyPriv.decons


{-# INLINE switchTimeL #-}
switchTimeL :: (time -> BodyTimeList.T time body -> a) -> T time body -> a
switchTimeL f =
   Mixed.switchSecondL (\b -> f b . BodyTimePriv.Cons) . decons

{-# INLINE switchBodyL #-}
switchBodyL :: a -> (body -> T time body -> a) -> BodyTimeList.T time body -> a
switchBodyL f g =
   Mixed.switchFirstL f (\t -> g t . Cons) . BodyTimePriv.decons


{-# INLINE switchTimeR #-}
switchTimeR :: (TimeBodyList.T time body -> time -> a) -> T time body -> a
switchTimeR f = Mixed.switchSecondR (f . TimeBodyPriv.Cons) . decons

{-# INLINE switchBodyR #-}
switchBodyR :: a -> (T time body -> body -> a) -> TimeBodyList.T time body -> a
switchBodyR f g = Mixed.switchFirstR f (g . Cons) . TimeBodyPriv.decons


mapTimeL ::
   (time -> time, BodyTimeList.T time body0 -> BodyTimeList.T time body1) ->
   T time body0 -> T time body1
mapTimeL = lift . Mixed.mapSecondL . mapSnd BodyTimePriv.unlift

mapTimeHead ::
   (time -> time) ->
   T time body -> T time body
mapTimeHead = lift . Mixed.mapSecondHead

mapTimeTail ::
   (BodyTimeList.T time body0 -> BodyTimeList.T time body1) ->
   T time body0 -> T time body1
mapTimeTail f =
   switchTimeL (\time -> consTime time . f)
{-
This causes a memory leak when used with chunky time values.
I have found this problem in synthesizer-alsa:EventList.MIDI.matchNote,
but I could not reliably reproduce that in smaller setups.

mapTimeTail =
   lift . Mixed.mapSecondTail . BodyTimePriv.unlift
-}


mapTimeR ::
   (TimeBodyList.T time body0 -> TimeBodyList.T time body1, time -> time) ->
   T time body0 -> T time body1
mapTimeR = lift . Mixed.mapSecondR . mapFst TimeBodyPriv.unlift

mapTimeLast ::
   (time -> time) ->
   T time body -> T time body
mapTimeLast = lift . Mixed.mapSecondLast

mapTimeInit ::
   (TimeBodyList.T time body0 -> TimeBodyList.T time body1) ->
   T time body0 -> T time body1
mapTimeInit = lift . Mixed.mapSecondInit . TimeBodyPriv.unlift


foldr :: (time -> a -> b) -> (body -> b -> a) -> a -> T time body -> b
foldr f g x = Uniform.foldr g f x . decons

forceTimeHead :: (NonNeg.C time) =>
   T time body -> T time body
forceTimeHead =
   mapTimeHead id

delay :: (NonNeg.C time) =>
   time -> T time body -> T time body
delay dif =
   mapTimeHead (add dif)

flatten :: (NonNeg.C time) => T time [body] -> T time body
flatten =
   Cons .
   Uniform.foldr
      (Mixed.appendUniformUniform . Uniform.fromSecondList zero)
      Mixed.consSecond    -- consTime
      Disp.empty .
--      (\(b:bs) xs -> consBody b (List.foldr (cons 0) xs bs)) empty .
   Uniform.mapSecond NonNeg.sum .
   Uniform.filterFirst (not . null) .
   decons
