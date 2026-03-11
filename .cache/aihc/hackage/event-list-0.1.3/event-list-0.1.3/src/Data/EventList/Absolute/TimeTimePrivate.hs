{- |
Copyright   :  (c) Henning Thielemann 2007-2009

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Absolute.TimeTimePrivate where

import qualified Data.EventList.Absolute.TimeBodyPrivate as TimeBodyList

import Data.EventList.Absolute.TimeBodyPrivate (($~))

-- import qualified Data.AlternatingList.List.Disparate as Disp
import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

import Data.Tuple.HT (mapFst, )

import qualified Control.Monad as Monad
import qualified Control.Applicative as Applicative

import Control.Applicative (Applicative, )


newtype T time body = Cons {decons :: Uniform.T body time}
   deriving (Eq, Ord, Show)

infixl 5 $*

($*) :: (Uniform.T body time -> a) -> (T time body -> a)
($*) f = f . decons


lift ::
   (Uniform.T body0 time0 -> Uniform.T body1 time1) ->
   (T time0 body0 -> T time1 body1)
lift f = Cons . f . decons

liftA :: Applicative m =>
   (Uniform.T body0 time0 -> m (Uniform.T body1 time1)) ->
   (T time0 body0 -> m (T time1 body1))
liftA f = Applicative.liftA Cons . f . decons

liftM :: Monad m =>
   (Uniform.T body0 time0 -> m (Uniform.T body1 time1)) ->
   (T time0 body0 -> m (T time1 body1))
liftM f = Monad.liftM Cons . f . decons


snocBody :: T time body -> body -> TimeBodyList.T time body
snocBody xs =
   TimeBodyList.Cons . (Mixed.snocFirst $* xs)

snocTime :: TimeBodyList.T time body -> time -> T time body
snocTime xs =
   Cons . (Mixed.snocSecond $~ xs)


viewTimeR :: T time body -> (TimeBodyList.T time body, time)
viewTimeR =
   mapFst TimeBodyList.Cons . Mixed.viewSecondR . decons

viewBodyR :: TimeBodyList.T time body -> Maybe (T time body, body)
viewBodyR =
   fmap (mapFst Cons) . Mixed.viewFirstR . TimeBodyList.decons


{-# INLINE switchTimeR #-}
switchTimeR :: (TimeBodyList.T time body -> time -> a) -> T time body -> a
switchTimeR f =
   Mixed.switchSecondR (f . TimeBodyList.Cons) . decons

{-# INLINE switchBodyR #-}
switchBodyR :: a -> (T time body -> body -> a) -> TimeBodyList.T time body -> a
switchBodyR f g =
   Mixed.switchFirstR f (g . Cons) . TimeBodyList.decons


mapTimeInit ::
   (TimeBodyList.T time body0 -> TimeBodyList.T time body1) ->
   T time body0 -> T time body1
mapTimeInit f = uncurry snocTime . mapFst f . viewTimeR

