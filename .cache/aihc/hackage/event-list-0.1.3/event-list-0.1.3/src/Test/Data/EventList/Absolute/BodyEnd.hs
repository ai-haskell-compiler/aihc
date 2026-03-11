{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Test.Data.EventList.Absolute.BodyEnd (tests) where

import Test.Utility
import Test.QuickCheck (quickCheck)

import qualified Data.EventList.Absolute.TimeBody as AbsBody
import qualified Data.EventList.Absolute.TimeBodyPrivate as AbsBodyPriv
import qualified Data.EventList.Relative.TimeBody as RelBody
import qualified Data.AlternatingList.List.Disparate as Disp

-- for testing in GHCi
-- import Data.AlternatingList.List.Disparate (empty)
-- import Data.AlternatingList.List.Uniform ((/.), (./))

import System.Random (Random, randomR, mkStdGen)
import Control.Monad (liftM)

import qualified Numeric.NonNegative.Class as NonNeg
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )
import Control.Monad.Trans.State (state, evalState, )

import Prelude hiding (filter, concat)


infixl 5 $~

($~) :: Num time =>
   (AbsBody.T time body -> a) -> (RelBody.T time body -> a)
($~) f = f . RelBody.toAbsoluteEventList 0

infixl 4 ==~

(==~) :: (Eq body, NonNeg.C time, Num time) =>
   AbsBody.T time body -> RelBody.T time body -> Bool
(==~) xs ys =
   xs == RelBody.toAbsoluteEventList 0 ys



duration ::
   (NonNeg.C time, Num time) =>
   RelBody.T time body -> Bool
duration xs =
   AbsBody.duration $~ xs == RelBody.duration xs


mapBody ::
   (Eq body1, NonNeg.C time, Num time) =>
   (body0 -> body1) -> RelBody.T time body0 -> Bool
mapBody f xs =
   AbsBody.mapBody f $~ xs ==~ RelBody.mapBody f xs



mapBodyM ::
   (Monad m, Eq body1, NonNeg.C time, Num time) =>
   (m (AbsBody.T time body1) -> AbsBody.T time body1) ->
   (body0 -> m body1) -> RelBody.T time body0 -> Bool
mapBodyM run f xs =
   run (AbsBody.mapBodyM f $~ xs) ==
   run (liftM (RelBody.toAbsoluteEventList 0) (RelBody.mapBodyM f xs))

mapBodyMRandom ::
   (NonNeg.C time, Num time, Random body, Eq body) =>
   Int -> RelBody.T time (body, body) -> Bool
mapBodyMRandom seed =
   mapBodyM 
      (flip evalState (mkStdGen seed))
      (state . randomR)


filter :: (Eq body, NonNeg.C time, Num time) =>
   (body -> Bool) -> RelBody.T time body -> Bool
filter p xs =
   AbsBody.filter p $~ xs ==~ RelBody.filter p xs

{-
mapMaybe :: (Num time) =>
   (body0 -> Maybe body1) ->
   RelBody.T time body0 -> RelBody.T time body1
mapMaybe f = catMaybes . mapBody f
-}

catMaybes :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time (Maybe body) -> Bool
catMaybes xs =
   AbsBody.catMaybes $~ xs ==~ RelBody.catMaybes xs

{-
Could be implemented more easily in terms of Uniform.partition
-}
partition :: (Eq body, NonNeg.C time, Num time) =>
   (body -> Bool) -> RelBody.T time body -> Bool
partition p xs =
   AbsBody.partition p $~ xs ==
--      mapPair (RelBody.toAbsoluteEventList 0, RelBody.toAbsoluteEventList 0)
      (uncurry $ \ys zs -> (,) $~ ys $~ zs)
      (RelBody.partition p xs)

{- |
Since we need it later for MIDI generation,
we will also define a slicing into equivalence classes of events.
-}
slice :: (Eq a, Eq body, NonNeg.C time, Num time) =>
   (body -> a) -> RelBody.T time body -> Bool
slice f xs =
   AbsBody.slice f $~ xs ==
   map (mapSnd (RelBody.toAbsoluteEventList 0)) (RelBody.slice f xs)


collectCoincident :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time body -> Bool
collectCoincident xs =
   AbsBody.collectCoincident $~ xs ==~
   RelBody.collectCoincident xs

collectCoincidentFoldr :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time body -> Bool
collectCoincidentFoldr xs =
   AbsBody.collectCoincident $~ xs ==
   AbsBody.collectCoincidentFoldr $~ xs

collectCoincidentNonLazy :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time body -> Bool
collectCoincidentNonLazy xs =
   AbsBody.collectCoincident $~ xs ==
   AbsBody.collectCoincidentNonLazy $~ xs

collectCoincidentInfinite ::
   (Eq body, NonNeg.C time, Num time) =>
   NonEmptyList time body -> Bool
collectCoincidentInfinite =
   checkInfinite .
   AbsBody.collectCoincident .
   makeUncollapsedInfiniteEventList


flatten :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time [body] -> Bool
flatten xs =
   AbsBody.flatten $~ xs  ==~  RelBody.flatten xs


normalize :: (Ord body, NonNeg.C time, Num time) =>
   RelBody.T time body -> Bool
normalize xs =
   AbsBody.normalize $~ xs  ==~  RelBody.normalize xs


merge :: (Ord body, NonNeg.C time, Num time) =>
   RelBody.T time body -> RelBody.T time body -> Bool
merge xs ys =
   AbsBody.merge $~ xs $~ ys  ==~  RelBody.merge xs ys


insert :: (Ord body, NonNeg.C time, Num time) =>
   time -> body -> RelBody.T time body -> Bool
insert t b xs =
   AbsBody.insert t b $~ xs  ==~  RelBody.insert t b xs



append :: (Eq body, NonNeg.C time, Num time) =>
   RelBody.T time body -> RelBody.T time body -> Bool
append xs ys =
   AbsBody.append $~ xs $~ ys  ==~
   RelBody.append xs ys

concat :: (Eq body, NonNeg.C time, Num time) =>
   [RelBody.T time body] -> Bool
concat xs =
   AbsBody.concat (map (RelBody.toAbsoluteEventList 0) xs)  ==~
   RelBody.concat xs


{-
cycle :: (NonNeg.C time) =>
   RelBody.T time body -> RelBody.T time body
cycle = concat . List.repeat
-}


decreaseStart :: (Eq body, NonNeg.C time, Num time) =>
   time -> time -> RelBody.T time body -> Bool
decreaseStart dif0 dif1 xs0 =
   let difA = min dif0 dif1
       difB = max dif0 dif1
       xs   = RelBody.delay difB xs0
   in  AbsBody.decreaseStart difA $~ xs ==~
       RelBody.decreaseStart difA xs


delay :: (Eq body, NonNeg.C time, Num time) =>
   time -> RelBody.T time body -> Bool
delay dif xs =
   AbsBody.delay dif $~ xs  ==~
   RelBody.delay dif xs



{-
resample :: (Integral time, Eq body) =>
   time -> RelBody.T (time, time) body -> Bool
resample rateInt xs0 =
   let xs = RelBody.mapTime (\(n,d) -> n % (d+1)) xs0
       rate = rateInt % 1
   in  AbsBody.resample rate $~ xs ==~
       (RelBody.resample rate xs `asTypeOf`
           AbsBody.singleton (undefined::Int) undefined)
-}

resample :: (Eq body) =>
   TimeDiff -> RelBody.T (TimeDiff, TimeDiff) body -> Bool
resample rateInt xs0 =
   let {-
       I add a small amount to the numerator in order
       to prevent the case of a fraction like 10.5,
       which can be easily rounded to 10 or 11
       depending to previous rounding errors.
       -}
       xs = RelBody.mapTime ((1e-6 +) . makeFracTime) xs0
       rate = timeToDouble rateInt + 1
   in  AbsBody.resample rate $~ xs ==~
       (RelBody.resample rate xs `asTypeOf`
           RelBody.singleton (undefined::TimeDiff) undefined)

resampleInfinite :: (Eq body) =>
   TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) body -> Bool
resampleInfinite rateInt =
   let rate = timeToDouble rateInt + 1
   in  checkInfinite .
       (`asTypeOf` AbsBody.singleton (undefined::TimeDiff) undefined) .
       AbsBody.resample rate .
       makeInfiniteEventList .
       mapPair (mapFst makeFracTime, RelBody.mapTime makeFracTime)




type NonEmptyList time body = ((time, body), RelBody.T time body)

makeUncollapsedInfiniteEventList ::
   (NonNeg.C time, Num time) =>
   NonEmptyList time body -> AbsBody.T time body
makeUncollapsedInfiniteEventList =
   makeInfiniteEventList .
   mapFst (mapFst (1+))

makeInfiniteEventList ::
   (NonNeg.C time, Num time) =>
   NonEmptyList time body -> AbsBody.T time body
makeInfiniteEventList =
   RelBody.toAbsoluteEventList 0 . RelBody.cycle . makeNonEmptyEventList

makeNonEmptyEventList :: (NonNeg.C time) =>
   NonEmptyList time body -> RelBody.T time body
makeNonEmptyEventList (p, evs) =
   uncurry RelBody.cons p evs

{- |
Pick an arbitrary element from an infinite list
and check if it can be evaluated.
-}
checkInfinite :: (Eq time, Eq body) =>
   AbsBody.T time body -> Bool
checkInfinite xs0 =
   let x = AbsBody.switchL
              (error "BodyEnd.checkInfinite: empty list") const $
              AbsBodyPriv.lift (Disp.drop 100) xs0
   in  x == x





tests :: [(String, IO ())]
tests =
   ("duration",
     quickCheck (duration :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("mapBody",
     quickCheck (mapBody toUpper :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("mapBodyM",
     quickCheck (mapBodyMRandom :: Int -> RelBody.T TimeDiff (ArbChar, ArbChar) -> Bool)) :
   ("filter",
     quickCheck (\c -> filter (c<) :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("catMaybes",
     quickCheck (catMaybes :: RelBody.T TimeDiff (Maybe ArbChar) -> Bool)) :
   ("partition",
     quickCheck (\c -> partition (c<) :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("slice",
     quickCheck (slice fst :: RelBody.T TimeDiff (ArbChar,ArbChar) -> Bool)) :
   ("collectCoincident",
     quickCheck (collectCoincident :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentFoldr",
     quickCheck (collectCoincidentFoldr :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentNonLazy",
     quickCheck (collectCoincidentNonLazy :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("collectCoincidentInfinite",
     quickCheck (collectCoincidentInfinite :: NonEmptyList TimeDiff ArbChar -> Bool)) :
   ("flatten",
     quickCheck (flatten :: RelBody.T TimeDiff [ArbChar] -> Bool)) :
   ("normalize",
     quickCheck (normalize :: RelBody.T TimeDiff ArbChar -> Bool)) :
   ("merge",
     quickCheck (merge :: RelBody.T TimeDiff ArbChar -> RelBody.T TimeDiff ArbChar -> Bool)) :
   ("insert",
     quickCheck (insert :: TimeDiff -> ArbChar -> RelBody.T TimeDiff ArbChar -> Bool)) :
   ("append",
     quickCheck (append :: RelBody.T TimeDiff ArbChar -> RelBody.T TimeDiff ArbChar -> Bool)) :
   ("concat",
     quickCheck (concat :: [RelBody.T TimeDiff ArbChar] -> Bool)) :
   ("decreaseStart",
     quickCheck (decreaseStart :: TimeDiff -> TimeDiff -> RelBody.T TimeDiff ArbChar -> Bool)) :
   ("delay",
     quickCheck (delay :: TimeDiff -> RelBody.T TimeDiff ArbChar -> Bool)) :
   ("resample",
     quickCheck (resample :: TimeDiff -> RelBody.T (TimeDiff, TimeDiff) ArbChar -> Bool)) :
   ("resampleInfinite",
     quickCheck (resampleInfinite :: TimeDiff -> NonEmptyList (TimeDiff, TimeDiff) ArbChar -> Bool)) :
   []
