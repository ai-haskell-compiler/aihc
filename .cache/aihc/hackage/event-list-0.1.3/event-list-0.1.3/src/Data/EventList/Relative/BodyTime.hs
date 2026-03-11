{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a body and ending with a time difference.

-}
module Data.EventList.Relative.BodyTime
   (T,
    empty, singleton, null,
    fromPairList, toPairList,
    getTimes, getBodies, duration, durationR,
    mapBody, mapTime,
    concatMapMonoid,
    traverse, traverse_, traverseBody, traverseTime,
    mapM, mapM_, mapBodyM, mapTimeM,
    foldr, foldrPair,
    cons, snoc, viewL, viewR, switchL, switchR,
    span,
   ) where

import Data.EventList.Relative.BodyTimePrivate

import qualified Data.AlternatingList.List.Disparate as Disp
-- import qualified Data.AlternatingList.List.Uniform as Uniform

import Control.Monad (Monad, )
import Control.Applicative (Applicative, WrappedMonad(WrapMonad, unwrapMonad), )
import Data.Monoid (Monoid, mempty, )

import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import qualified Data.List as List

import Data.Function ((.), ($), )
import Data.Functor (fmap, )
import Data.Maybe (Maybe, )
import Prelude (Num, (+), Bool, )


fromPairList :: [(body, time)] -> T time body
fromPairList = Cons . Disp.fromPairList

toPairList :: T time body -> [(body, time)]
toPairList = Disp.toPairList . decons

getBodies :: T time body -> [body]
getBodies = Disp.getFirsts . decons

getTimes :: T time body -> [time]
getTimes = Disp.getSeconds . decons

duration :: Num time => T time body -> time
duration = List.sum . getTimes

durationR :: Num time => T time body -> time
durationR = List.foldr (+) 0 . getTimes


mapBody :: (body0 -> body1) -> T time body0 -> T time body1
mapBody f = lift (Disp.mapFirst f)

mapTime :: (time0 -> time1) -> T time0 body -> T time1 body
mapTime f = lift (Disp.mapSecond f)


concatMapMonoid :: Monoid m =>
   (time -> m) -> (body -> m) ->
   T time body -> m
concatMapMonoid f g =
   Disp.concatMapMonoid g f . decons

traverse :: Applicative m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
traverse timeAction bodyAction =
   liftA (Disp.traverse bodyAction timeAction)

traverse_ :: Applicative m =>
   (time -> m ()) -> (body -> m ()) ->
   T time body -> m ()
traverse_ f g = Disp.traverse_ g f . decons

traverseBody :: Applicative m =>
   (body0 -> m body1) -> T time body0 -> m (T time body1)
traverseBody f = liftA (Disp.traverseFirst f)

traverseTime :: Applicative m =>
   (time0 -> m time1) -> T time0 body -> m (T time1 body)
traverseTime f = liftA (Disp.traverseSecond f)


mapM :: Monad m =>
   (time0 -> m time1) -> (body0 -> m body1) ->
   T time0 body0 -> m (T time1 body1)
mapM timeAction bodyAction =
   unwrapMonad . traverse (WrapMonad . timeAction) (WrapMonad . bodyAction)

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


foldr :: (body -> a -> b) -> (time -> b -> a) -> b -> T time body -> b
foldr f g x = Disp.foldr f g x . decons

foldrPair :: (body -> time -> a -> a) -> a -> T time body -> a
foldrPair f x = Disp.foldrPair f x . decons


empty :: T time body
empty = mempty

null :: T time body -> Bool
null = Disp.null . decons

singleton :: body -> time -> T time body
singleton body time = Cons $ Disp.singleton body time


cons :: body -> time -> T time body -> T time body
cons body time = lift (Disp.cons body time)

snoc :: T time body -> body -> time -> T time body
snoc xs body time =
   Cons $ (Disp.snoc $*~ xs) body time


viewL :: T time body -> Maybe ((body, time), T time body)
viewL = fmap (mapSnd Cons) . Disp.viewL . decons

viewR :: T time body -> Maybe (T time body, (body, time))
viewR = fmap (mapFst Cons) . Disp.viewR . decons


{-# INLINE switchL #-}
switchL :: c -> (body -> time -> T time body -> c) -> T time body -> c
switchL f g = Disp.switchL f (\ b t  -> g b t . Cons) . decons

{-# INLINE switchR #-}
switchR :: c -> (T time body -> body -> time -> c) -> T time body -> c
switchR f g = Disp.switchR f (\xs b t -> g (Cons xs) b t) . decons


span :: (body -> Bool) -> T time body -> (T time body, T time body)
span p = mapPair (Cons, Cons) . Disp.spanFirst p . decons
