{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Relative.BodyBodyPrivate where

-- import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
-- import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import qualified Control.Applicative as App

import Test.QuickCheck (Arbitrary(arbitrary, shrink))


newtype T time body = Cons {decons :: Uniform.T time body}
   deriving (Eq, Ord)

instance (Show time, Show body) => Show (T time body) where
   showsPrec p = Uniform.format " /. " " ./ " p . decons


instance (Arbitrary time, Arbitrary body) =>
             Arbitrary (T time body) where
   arbitrary = Monad.liftM Cons arbitrary
   shrink = liftM shrink


{-
instance Monoid (T time body) where

mempty cannot be defined

mappend could be defined by inserting a time difference of zero
-}

instance Functor (T time) where
   fmap f (Cons x) = Cons (Uniform.mapSecond f x)

instance Fold.Foldable (T time) where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable (T time) where
   traverse f =
      App.liftA Cons . Uniform.traverse App.pure f . decons


infixl 5 $**

($**) :: (Uniform.T time body -> a) -> (T time body -> a)
($**) f = f . decons


lift ::
   (Uniform.T time0 body0 -> Uniform.T time1 body1) ->
   (T time0 body0 -> T time1 body1)
lift f = Cons . f . decons

liftM :: Monad m =>
   (Uniform.T time0 body0 -> m (Uniform.T time1 body1)) ->
   (T time0 body0 -> m (T time1 body1))
liftM f = Monad.liftM Cons . f . decons

unlift ::
   (T time0 body0 -> T time1 body1) ->
   (Uniform.T time0 body0 -> Uniform.T time1 body1)
unlift f = decons . f . Cons
