{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a body and ending with a time difference.

-}
module Data.EventList.Relative.MixedBody
   (consBody, consTime, (/.), (./), empty,
    viewTimeL,   viewBodyL,
    switchTimeL, switchBodyL,
    mapTimeL, mapTimeHead, mapTimeTail,
   ) where

import Data.EventList.Relative.TimeBody (empty)

import qualified Data.EventList.Relative.TimeBody as TimeBodyList
import qualified Data.EventList.Relative.BodyBody as BodyBodyList

import qualified Data.EventList.Relative.TimeBodyPrivate as TimeBodyPriv
import qualified Data.EventList.Relative.BodyBodyPrivate as BodyBodyPriv

import Data.EventList.Relative.TimeBodyPrivate (mapTimeL, mapTimeHead, mapTimeTail,)

-- import qualified Data.AlternatingList.List.Disparate as Disp
-- import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

-- import Data.AlternatingList.List.Mixed ((/.), (./))

import Data.Tuple.HT (mapSnd, )


consBody, (./) :: body -> TimeBodyList.T time body -> BodyBodyList.T time body
consBody b = BodyBodyPriv.Cons . Mixed.consSecond b . TimeBodyPriv.decons

consTime, (/.) :: time -> BodyBodyList.T time body -> TimeBodyList.T time body
consTime t = TimeBodyPriv.Cons . Mixed.consFirst t . BodyBodyPriv.decons

infixr 5 /. , ./

(./) = consBody

(/.) = consTime

viewTimeL :: TimeBodyList.T time body -> Maybe (time, BodyBodyList.T time body)
viewTimeL =
   fmap (mapSnd BodyBodyPriv.Cons) . Mixed.viewFirstL . TimeBodyPriv.decons

viewBodyL :: BodyBodyList.T time body -> (body, TimeBodyList.T time body)
viewBodyL = mapSnd TimeBodyPriv.Cons . Mixed.viewSecondL . BodyBodyPriv.decons


{-# INLINE switchTimeL #-}
switchTimeL :: a -> (time -> BodyBodyList.T time body -> a) -> TimeBodyList.T time body -> a
switchTimeL f g =
   Mixed.switchFirstL f (\t -> g t . BodyBodyPriv.Cons) . TimeBodyPriv.decons

{-# INLINE switchBodyL #-}
switchBodyL :: (body -> TimeBodyList.T time body -> a) -> BodyBodyList.T time body -> a
switchBodyL f =
   Mixed.switchSecondL (\b -> f b . TimeBodyPriv.Cons) . BodyBodyPriv.decons
