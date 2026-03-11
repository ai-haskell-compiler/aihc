{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Relative.TimeBodyPrivate where

import qualified Data.EventList.Relative.BodyBodyPrivate as BodyBodyList
import qualified Data.EventList.Relative.BodyBodyPrivate as BodyBodyPriv

import qualified Data.AlternatingList.List.Disparate as Disp
-- import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import qualified Control.Applicative as App
import Control.Applicative (Applicative, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import Data.Tuple.HT (mapSnd, )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))



newtype T time body = Cons {decons :: Disp.T time body}
   deriving (Eq, Ord)


instance (Show time, Show body) => Show (T time body) where
   showsPrec p = Disp.format " /. " " ./ " p . decons


instance (Arbitrary time, Arbitrary body) =>
             Arbitrary (T time body) where
   arbitrary = Monad.liftM Cons arbitrary
   shrink = liftM shrink

instance Semigroup (T time body) where
   Cons x <> Cons y = Cons (Disp.append x y)

instance Monoid (T time body) where
   mempty = Cons Disp.empty
   mappend = (<>)

instance Functor (T time) where
   fmap f (Cons x) = Cons (Disp.mapSecond f x)

instance Fold.Foldable (T time) where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable (T time) where
   traverse f =
      App.liftA Cons . Disp.traverse App.pure f . decons


infixl 5 $~*

($~*) :: (Disp.T time body -> a) -> (T time body -> a)
($~*) f = f . decons


lift ::
   (Disp.T time0 body0 -> Disp.T time1 body1) ->
   (T time0 body0 -> T time1 body1)
lift f = Cons . f . decons

liftA :: Applicative m =>
   (Disp.T time0 body0 -> m (Disp.T time1 body1)) ->
   (T time0 body0 -> m (T time1 body1))
liftA f = App.liftA Cons . f . decons

liftM :: Monad m =>
   (Disp.T time0 body0 -> m (Disp.T time1 body1)) ->
   (T time0 body0 -> m (T time1 body1))
liftM f = Monad.liftM Cons . f . decons

unlift ::
   (T time0 body0 -> T time1 body1) ->
   (Disp.T time0 body0 -> Disp.T time1 body1)
unlift f = decons . f . Cons



mapTimeL ::
   (time -> time, BodyBodyList.T time body0 -> BodyBodyList.T time body1) ->
   T time body0 -> T time body1
mapTimeL = lift . Mixed.mapFirstL . mapSnd BodyBodyPriv.unlift

mapTimeHead ::
   (time -> time) ->
   T time body -> T time body
mapTimeHead = lift . Mixed.mapFirstHead

mapTimeTail ::
   (BodyBodyList.T time body0 -> BodyBodyList.T time body1) ->
   T time body0 -> T time body1
mapTimeTail = lift . Mixed.mapFirstTail . BodyBodyPriv.unlift
