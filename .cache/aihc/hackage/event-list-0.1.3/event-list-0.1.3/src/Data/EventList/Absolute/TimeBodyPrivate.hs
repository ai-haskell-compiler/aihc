{- |
Copyright   :  (c) Henning Thielemann 2007-2009

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Absolute.TimeBodyPrivate where

import qualified Data.AlternatingList.List.Disparate as Disp
-- import qualified Data.AlternatingList.List.Uniform as Uniform
-- import qualified Data.AlternatingList.List.Mixed as Mixed

import qualified Control.Monad as Monad
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import qualified Control.Applicative as App
import Control.Applicative (Applicative, )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>), )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import Prelude hiding (concat, cycle)


newtype T time body = Cons {decons :: Disp.T time body}
   deriving (Eq, Ord, Show)


instance (Arbitrary time, Arbitrary body) =>
             Arbitrary (T time body) where
   arbitrary = Monad.liftM Cons arbitrary
   shrink = liftM shrink

instance (Num time, Ord time) => Semigroup (T time body) where
   (<>) = append

instance (Num time, Ord time) => Monoid (T time body) where
   mempty = Cons Disp.empty
   mappend = (<>)
   mconcat = concat

instance Functor (T time) where
   fmap f (Cons x) = Cons (Disp.mapSecond f x)

instance Fold.Foldable (T time) where
   foldMap = Trav.foldMapDefault

instance Trav.Traversable (T time) where
   traverse f =
      App.liftA Cons . Disp.traverse App.pure f . decons


infixl 5 $~

($~) :: (Disp.T time body -> a) -> (T time body -> a)
($~) f = f . decons


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


{-# INLINE switchL #-}
switchL :: c -> ((time, body) -> T time body -> c) -> T time body -> c
switchL f g = Disp.switchL f (\ t b  -> g (t,b) . Cons) . decons

{-# INLINE switchR #-}
switchR :: c -> (T time body -> (time, body) -> c) -> T time body -> c
switchR f g = Disp.switchR f (\xs t b -> g (Cons xs) (t,b)) . decons


mapBody :: (body0 -> body1) -> T time body0 -> T time body1
mapBody f = lift (Disp.mapSecond f)

mapTime :: (time0 -> time1) -> T time0 body -> T time1 body
mapTime f = lift (Disp.mapFirst f)


{- |
Duration of an empty event list is considered zero.
However, I'm not sure if this is sound.
-}
duration :: Num time => T time body -> time
duration = switchR 0 (const fst)

{-
Is it necessary to exclude negative delays?
Even negative time stamps should not hurt absolutely timestamped lists.
-}
delay :: (Ord time, Num time) =>
   time -> T time body -> T time body
delay dif =
   if dif>=0
     then mapTime (dif+)
     else error "delay: negative delay"


append :: (Ord time, Num time) =>
   T time body -> T time body -> T time body
append xs = lift (Disp.append $~ xs) . delay (duration xs)

concat :: (Ord time, Num time) =>
   [T time body] -> T time body
concat xs =
   let ts = scanl (+) 0 (map duration xs)
   in  Cons $ Disp.concat $ map decons $ zipWith delay ts xs

{-
Unfortunately in absolute lists we cannot use sharing as in List.cycle
since the start times of the later lists are greater.
-}
cycle :: (Ord time, Num time) =>
   T time body -> T time body
cycle = concat . repeat
