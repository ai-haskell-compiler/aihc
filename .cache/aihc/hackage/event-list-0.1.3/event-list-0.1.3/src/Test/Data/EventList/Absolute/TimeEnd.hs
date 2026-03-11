{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Test.Data.EventList.Absolute.TimeEnd (tests) where

import Test.Utility
import Test.QuickCheck (quickCheck)

import qualified Data.EventList.Absolute.TimeTime as AbsTime
import qualified Data.EventList.Absolute.TimeTimePrivate as AbsTimePriv
import qualified Data.EventList.Relative.TimeTime as RelTime
import qualified Data.AlternatingList.List.Mixed as Mixed

-- for testing in GHCi
-- import Data.AlternatingList.List.Disparate (empty)
-- import Data.AlternatingList.List.Uniform ((/.), (./))

import System.Random (Random, randomR, mkStdGen)
import Control.Monad (liftM)

import qualified Numeric.NonNegative.Class as NonNeg
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import Control.Monad.Trans.State (state, evalState)

import Prelude hiding (filter, concat)


infixl 5 $~

($~) :: Num time =>
   (AbsTime.T time body -> a) -> (RelTime.T time body -> a)
($~) f = f . RelTime.toAbsoluteEventList 0

infixl 4 ==~

(==~) :: (Eq body, Eq time, Num time) =>
   AbsTime.T time body -> RelTime.T time body -> Bool
(==~) xs ys =
   xs == RelTime.toAbsoluteEventList 0 ys



duration ::
   (NonNeg.C time, Num time) =>
   RelTime.T time body -> Bool
duration xs =
   AbsTime.duration $~ xs == RelTime.duration xs


mapBody :: (Eq body1, NonNeg.C time, Num time) =>
   (body0 -> body1) -> RelTime.T time body0 -> Bool
mapBody f xs =
   AbsTime.mapBody f $~ xs ==~ RelTime.mapBody f xs



mapBodyM ::
   (Monad m, Eq body1, NonNeg.C time, Num time) =>
   (m (AbsTime.T time body1) -> AbsTime.T time body1) ->
   (body0 -> m body1) -> RelTime.T time body0 -> Bool
mapBodyM run f xs =
   run (AbsTime.mapBodyM f $~ xs) ==
   run (liftM (RelTime.toAbsoluteEventList 0) (RelTime.mapBodyM f xs))

mapBodyMRandom ::
   (NonNeg.C time, Num time, Random body, Eq body) =>
   Int -> RelTime.T time (body, body) -> Bool
mapBodyMRandom seed =
   mapBodyM 
      (flip evalState (mkStdGen seed))
      (state . randomR)


filter :: (Eq body, NonNeg.C time, Num time) =>
   (body -> Bool) -> RelTime.T time body -> Bool
filter p xs =
   AbsTime.filter p $~ xs ==~ RelTime.filter p xs

{-
mapMaybe :: (NonNeg.C time, Num time) =>
   (body0 -> Maybe body1) ->
   RelTime.T time body0 -> RelTime.T time body1
mapMaybe f = catMaybes . mapBody f
-}

catMaybes :: (Eq body, NonNeg.C time, Num time) =>
   RelTime.T time (Maybe body) -> Bool
catMaybes xs =
   AbsTime.catMaybes $~ xs ==~ RelTime.catMaybes xs

{-
Could be implemented more easily in terms of Uniform.partition
-}
partition :: (Eq body, NonNeg.C time, Num time) =>
   (body -> Bool) -> RelTime.T time body -> Bool
partition p xs =
   AbsTime.partition p $~ xs ==
--      mapPair (RelTime.toAbsoluteEventList 0, RelTime.toAbsoluteEventList 0)
      (uncurry $ \ys zs -> (,) $~ ys $~ zs)
      (RelTime.partition p xs)

{- |
Since we need it later for MIDI generation,
we will also define a slicing into equivalence classes of events.
-}
slice :: (Eq a, Eq body, NonNeg.C time, Num time) =>
   (body -> a) -> RelTime.T time body -> Bool
slice f xs =
   AbsTime.slice f $~ xs ==
   map (mapSnd (RelTime.toAbsoluteEventList 0)) (RelTime.slice f xs)


collectCoincident :: (NonNeg.C time, Num time, Eq body) =>
   RelTime.T time body -> Bool
collectCoincident xs =
   AbsTime.collectCoincident $~ xs ==~
   RelTime.collectCoincident xs

collectCoincidentInfinite :: (NonNeg.C time, Num time, Eq body) =>
   NonEmptyList time body -> Bool
collectCoincidentInfinite =
   checkInfinite .
   AbsTime.collectCoincident .
   makeUncollapsedInfiniteEventList


flatten :: (NonNeg.C time, Num time, Eq body) =>
   RelTime.T time [body] -> Bool
flatten xs =
   AbsTime.flatten $~ xs  ==~  RelTime.flatten xs


normalize :: (NonNeg.C time, Num time, Ord body) =>
   RelTime.T time body -> Bool
normalize xs =
   AbsTime.normalize $~ xs  ==~  RelTime.normalize xs


{-
test fails

1 /. '\DEL' ./ 0 /. '"' ./ 1 /. '}' ./ 0 /. empty
0 /. '\DEL' ./ 1 /. '}' ./ 0 /. '\DEL' ./ 1 /. empty


4 /. '|' ./ 0 /. '!' ./ 3 /. '"' ./ 1 /. '!' ./ 3 /. empty
1 /. '$' ./ 2 /. '~' ./ 1 /. '|' ./ 1 /. '|' ./ 1 /. empty
-}
merge :: (NonNeg.C time, Num time, Ord body) =>
   RelTime.T time body -> RelTime.T time body -> Bool
merge xs ys =
   AbsTime.merge $~ xs $~ ys  ==~  RelTime.merge xs ys


insert :: (NonNeg.C time, Num time, Ord body) =>
   time -> body -> RelTime.T time body -> Bool
insert t b xs =
   AbsTime.insert t b $~ xs  ==~  RelTime.insert t b xs



append :: (NonNeg.C time, Num time, Eq body) =>
   RelTime.T time body -> RelTime.T time body -> Bool
append xs ys =
   AbsTime.append $~ xs $~ ys  ==~
   RelTime.append xs ys

concat :: (NonNeg.C time, Num time, Eq body) =>
   [RelTime.T time body] -> Bool
concat xs =
   AbsTime.concat (map (RelTime.toAbsoluteEventList 0) xs)  ==~
   RelTime.concat xs


{-
cycle :: (NonNeg.C time) =>
   RelTime.T time body -> RelTime.T time body
cycle = concat . List.repeat
-}


decreaseStart :: (NonNeg.C time, Num time, Eq body) =>
   time -> time -> RelTime.T time body -> Bool
decreaseStart dif0 dif1 xs0 =
   let difA = min dif0 dif1
       difB = max dif0 dif1
       xs   = RelTime.delay difB xs0
   in  AbsTime.decreaseStart difA $~ xs ==~
       RelTime.decreaseStart difA xs


delay :: (NonNeg.C time, Num time, Eq body) =>
   time -> RelTime.T time body -> Bool
delay dif xs =
   AbsTime.delay dif $~ xs  ==~
   RelTime.delay dif xs



resample :: (Eq body) =>
   TimeDiff -> RelTime.T (TimeDiff, TimeDiff) body -> Bool
resample rateInt xs0 =
   let {-
       I add a small amount to the numerator in order
       to prevent the case of a fraction like 10.5,
       which can be easily rounded to 10 or 11
       depending on previous rounding errors.
       -}
       xs = RelTime.mapTime ((1e-6 +) . makeFracTime) xs0
       rate = timeToDouble rateInt + 1
   in  AbsTime.resample rate $~ xs ==~
       intTimeList (RelTime.resample rate xs)

resampleInfinite :: (Eq body) =>
   TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) body -> Bool
resampleInfinite rateInt =
   let rate = timeToDouble rateInt + 1
   in  checkInfinite . intTimeList .
       AbsTime.resample rate .
       makeInfiniteEventList .
       mapPair (mapFst makeFracTime, RelTime.mapTime makeFracTime)



type NonEmptyList time body = ((time, body), RelTime.T time body)

makeUncollapsedInfiniteEventList :: (NonNeg.C time, Num time) =>
   NonEmptyList time body -> AbsTime.T time body
makeUncollapsedInfiniteEventList =
   makeInfiniteEventList .
   mapFst (mapFst (1+))

makeInfiniteEventList :: (NonNeg.C time, Num time) =>
   NonEmptyList time body -> AbsTime.T time body
makeInfiniteEventList =
   RelTime.toAbsoluteEventList 0 . RelTime.cycle . makeNonEmptyEventList

makeNonEmptyEventList :: (NonNeg.C time, Num time) =>
   NonEmptyList time body -> RelTime.T time body
makeNonEmptyEventList (p, evs) =
   uncurry RelTime.cons p evs

{- |
Pick an arbitrary element from an infinite list
and check if it can be evaluated.
-}
checkInfinite :: (Eq time, Eq body) =>
   AbsTime.T time body -> Bool
checkInfinite xs0 =
   let (x,xs) = AbsTime.viewL (AbsTimePriv.lift (Mixed.dropUniform 100) xs0)
       y = maybe
              (error "checkInfinite: finite list")
              fst
              xs
   in  x == x && y == y




tests :: [(String, IO ())]
tests =
   ("duration",
     quickCheck (duration :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("mapBody",
     quickCheck (mapBody toUpper :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("mapBodyM",
     quickCheck (mapBodyMRandom :: Int -> RelTime.T TimeDiff (ArbChar, ArbChar) -> Bool)) :
   ("filter",
     quickCheck (\c -> filter (c<) :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("catMaybes",
     quickCheck (catMaybes :: RelTime.T TimeDiff (Maybe ArbChar) -> Bool)) :
   ("partition",
     quickCheck (\c -> partition (c<) :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("slice",
     quickCheck (slice fst :: RelTime.T TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("collectCoincident",
     quickCheck (collectCoincident :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentInfinite",
     quickCheck (collectCoincidentInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("flatten",
     quickCheck (flatten :: RelTime.T TimeDiff [ArbChar] -> Bool)) :
   ("normalize",
     quickCheck (normalize :: RelTime.T TimeDiff ArbChar -> Bool)) :
   ("merge",
     quickCheck (merge :: RelTime.T TimeDiff ArbChar -> RelTime.T TimeDiff ArbChar -> Bool)) :
   ("insert",
     quickCheck (insert :: TimeDiff -> ArbChar -> RelTime.T TimeDiff ArbChar -> Bool)) :
   ("append",
     quickCheck (append :: RelTime.T TimeDiff ArbChar -> RelTime.T TimeDiff ArbChar -> Bool)) :
   ("concat",
     quickCheck (concat :: [RelTime.T TimeDiff ArbChar] -> Bool)) :
   ("decreaseStart",
     quickCheck (decreaseStart :: TimeDiff -> TimeDiff -> RelTime.T TimeDiff ArbChar -> Bool)) :
   ("delay",
     quickCheck (delay :: TimeDiff -> RelTime.T TimeDiff ArbChar -> Bool)) :
   ("resample",
     quickCheck (resample :: TimeDiff -> RelTime.T (TimeDiff, TimeDiff) ArbChar -> Bool)) :
   ("resampleInfinite",
     quickCheck (resampleInfinite :: TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) ArbChar -> Bool)) :
   []
