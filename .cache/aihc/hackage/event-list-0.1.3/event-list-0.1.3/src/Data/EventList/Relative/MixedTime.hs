{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98


Event lists starting with a body and ending with a time difference.

-}
module Data.EventList.Relative.MixedTime
   (consBody, consTime, (/.), (./), empty,
    viewTimeL,   viewBodyL,
    switchTimeL, switchBodyL,
    mapTimeL, mapTimeHead, mapTimeTail,
    mapBodyL, mapBodyHead, mapBodyTail,
   ) where

import qualified Data.EventList.Relative.BodyTimePrivate as BodyTimePriv
import           Data.EventList.Relative.TimeTimePrivate as TimeTimePriv

import qualified Data.EventList.Relative.BodyTime as BodyTimeList
import qualified Data.EventList.Relative.TimeTime as TimeTimeList

import Data.EventList.Relative.BodyTime (empty)

-- import qualified Data.AlternatingList.List.Disparate as Disp
-- import qualified Data.AlternatingList.List.Uniform as Uniform
import qualified Data.AlternatingList.List.Mixed as Mixed

-- import Data.AlternatingList.List.Mixed ((/.), (./))

import Data.Tuple.HT (mapSnd, )

import Prelude hiding
   (null, foldr, map, filter, concat, cycle, sequence, sequence_, mapM, mapM_)


infixr 5 /. , ./

(./) :: body -> TimeTimeList.T time body -> BodyTimeList.T time body
(./) = consBody

(/.) :: time -> BodyTimeList.T time body -> TimeTimeList.T time body
(/.) = consTime

mapBodyL ::
   (body -> body, TimeTimeList.T time0 body -> TimeTimeList.T time1 body) ->
   BodyTimeList.T time0 body -> BodyTimeList.T time1 body
mapBodyL = BodyTimePriv.lift . Mixed.mapFirstL . mapSnd TimeTimePriv.unlift

mapBodyHead ::
   (body -> body) ->
   BodyTimeList.T time body -> BodyTimeList.T time body
mapBodyHead = BodyTimePriv.lift . Mixed.mapFirstHead

mapBodyTail ::
   (TimeTimeList.T time0 body -> TimeTimeList.T time1 body) ->
   BodyTimeList.T time0 body -> BodyTimeList.T time1 body
mapBodyTail = BodyTimePriv.lift . Mixed.mapFirstTail . TimeTimePriv.unlift

