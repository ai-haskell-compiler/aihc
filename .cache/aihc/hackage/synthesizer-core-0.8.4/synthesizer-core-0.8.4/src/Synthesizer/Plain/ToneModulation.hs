{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2006
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


Avoid importing this module.
Better use functions from
"Synthesizer.Plain.Oscillator" and
"Synthesizer.Basic.Wave"

Input data is interpreted as samples of data on a cylinder
in the following form:

> |*          |
> |   *       |
> |      *    |
> |         * |
> | *         |
> |    *      |
> |       *   |
> |          *|
> |  *        |
> |     *     |
> |        *  |


> -----------
> *
>     *
>         *
>  *
>      *
>          *
>   *
>       *
>           *
>    *
>        *
> -----------

We have to interpolate in the parallelograms.

-}
module Synthesizer.Plain.ToneModulation (
   Cell,
   interpolateCell,

   Prototype,
   makePrototype,
   sampledToneCell,

   oscillatorCells,
   seekCell,
   oscillatorSuffixes,

   -- this function fits better in the Oscillator module
   freqsToPhases,

   -- for testing
   dropFrac,
   dropRem,
   propDropFrac,
   propDropRem,
   oscillatorCoords,
   integrateFractional,
   limitRelativeShapes,
   limitMinRelativeValues,
   limitMaxRelativeValues,
   limitMaxRelativeValuesNonNeg,
   ) where

import qualified Synthesizer.Basic.ToneModulation as ToneMod
import qualified Synthesizer.Basic.Phase as Phase

import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Interpolation as Interpolation
import Synthesizer.Interpolation (Margin, )

import Control.Monad (guard, )

import qualified Data.List       as List
import qualified Data.List.HT    as ListHT
import qualified Data.List.Match as ListMatch
import Data.Array (Array, (!), listArray, )
import Data.Tuple.HT (mapPair, mapSnd, forcePair, )
import Data.Ord.HT (limit, )

import qualified Algebra.RealField             as RealField
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import qualified Number.NonNegative       as NonNeg
import qualified Number.NonNegativeChunky as Chunky

import NumericPrelude.Numeric
import NumericPrelude.Base



-- * general helpers

type Cell y = Sig.T (Sig.T y)

interpolateCell ::
   Interpolation.T a y ->
   Interpolation.T b y ->
   (a, b) ->
   Cell y -> y
interpolateCell ipLeap ipStep (qLeap,qStep) =
   Interpolation.func ipStep qStep .
   map (Interpolation.func ipLeap qLeap)


-- * array based shape variable wave

data Prototype t y =
   Prototype {
      protoMarginLeap,
      protoMarginStep  :: Margin,
      protoIpOffset    :: Int,
      protoPeriod      :: t,
      protoPeriodInt   :: Int,
      protoShapeLimits :: (t,t),
      protoArray       :: Array Int y
   }


makePrototype :: (RealField.C t) =>
   Margin ->
   Margin ->
   Int -> t -> Sig.T y -> Prototype t y
makePrototype marginLeap marginStep periodInt period tone =
   let ipOffset =
          ToneMod.interpolationOffset marginLeap marginStep periodInt
       len = length tone
       (lower,upper) =
          ToneMod.shapeLimits marginLeap marginStep periodInt len
       limits =
          if lower > upper
            then error "min>max"
            else (fromIntegral lower, fromIntegral upper)

   in  Prototype {
          protoMarginLeap  = marginLeap,
          protoMarginStep  = marginStep,
          protoIpOffset    = ipOffset,
          protoPeriod      = period,
          protoPeriodInt   = periodInt,
          protoShapeLimits = limits,
          protoArray       = listArray (0, pred len) tone
       }

sampledToneCell :: (RealField.C t) =>
   Prototype t y -> t -> Phase.T t -> ((t,t), Cell y)
sampledToneCell p shape phase =
   let (n, q) =
          ToneMod.flattenShapePhase (protoPeriodInt p) (protoPeriod p)
             (limit (protoShapeLimits p) shape, phase)
   in  (q,
        map (map (protoArray p ! ) . iterate (protoPeriodInt p +)) $
        enumFrom (n - protoIpOffset p))


{-
  M = ((1,1)^T, (periodRound, period-periodRound)^T)

  equation for the line
   0 = (nStep - offset ipStep) +
       (nLeap - offset ipLeap) * periodInt

   <(1,periodInt), (offset ipStep, offset ipLeap)>
        = <(1,periodInt), (nStep,nLeap)>
   d = <a,x>
     = <a,M^-1*M*x>
     = <(M^-T)*a,M*x>
     = <(M^-T)*a,y>
   b = (M^-T)*a
   required:
      y0 such that y1=0
      y0 such that y1=period

   The line {x : d = <a,x>} converted to (shape,phase) coordinates
   has constant shape and meets all phases.
-}



-- * lazy oscillator


oscillatorCells :: (RealField.C t) =>
    Margin ->
    Margin ->
    Int -> t ->
    Sig.T y -> (t, Sig.T t) -> (Phase.T t, Sig.T t) -> Sig.T ((t,t), Cell y)
oscillatorCells
       marginLeap marginStep periodInt period sampledTone shapes freqs =
    map (seekCell periodInt period) $
    oscillatorSuffixes
        marginLeap marginStep periodInt period sampledTone shapes freqs

seekCell :: (RealField.C t) =>
    Int -> t ->
    ((t, Phase.T t), Cell y) -> ((t,t), Cell y)
seekCell periodInt period =
    {- n will be zero within the data.
       We would need it only for extrapolation at the end.
       But this does not happen, since we limit the shape control parameter accordingly.
    -}
    (\(coords, ptr) ->
       let (k,q) = ToneMod.flattenShapePhase periodInt period coords
       in  if k>0
             then error "ToneModulation.oscillatorCells: k>0"
             else (q, drop (periodInt+k) ptr))

oscillatorSuffixes :: (RealField.C t) =>
    Margin ->
    Margin ->
    Int -> t -> Sig.T y ->
    (t, Sig.T t) -> (Phase.T t, Sig.T t) ->
    Sig.T ((t, Phase.T t), Cell y)
oscillatorSuffixes
       marginLeap marginStep periodInt period sampledTone shapes freqs =
    let ptrs =
           List.transpose $
           takeWhile (not . null) $
           iterate (drop periodInt) sampledTone
        ipOffset =
           periodInt +
           ToneMod.interpolationOffset marginLeap marginStep periodInt
{- I tried to switch integrateFractional and limitRelativeShapes
   in order to have a position where I can easily add phase distortion.
   However, limitting skip values after integrateFractional
   does not work this way, since once we start setting skip values to zero,
   we had to clear the fractional parts of the shape coordinate, too.
        (firstSkip:allSkips,coords) =
           unzip $
           integrateFractional period shapes freqs
        (skip,skips) =
           limitRelativeShapes marginLeap marginStep
              periodInt sampledTone (firstSkip,allSkips)
-}
        (skip:skips,coords) =
           unzip $
           integrateFractional period
              (limitRelativeShapes marginLeap marginStep periodInt sampledTone shapes)
              freqs
    in  zip coords $
        map (\(n,ptr) ->
               if n>0
                 then error $ "ToneModulation.oscillatorCells: " ++
                              "limit of shape parameter is buggy"
                 else ptr) $
        tail $
        scanl
           {- since we clip the coordinates before calling oscillatorCells
              we do not need 'dropRem', since 'drop' would never go beyond the list end -}
           (\ (n,ptr0) d0 -> dropRem (n+d0) ptr0)
           (0,ptrs)
           ((skip - ipOffset) : skips)

dropFrac :: RealField.C i => i -> Sig.T a -> (Int, i, Sig.T a)
dropFrac =
   let recourse acc n xt =
          if n>=1
            then
               case xt of
                  _:xs -> recourse (succ acc) (n-1) xs
                  [] -> (acc, n, [])
            else (acc,n,xt)
   in  recourse 0

dropFrac' :: RealField.C i => i -> Sig.T a -> (Int, i, Sig.T a)
dropFrac' =
   let recourse acc n xt =
          maybe
             (acc,n,xt)
             (recourse (succ acc) (n-1) . snd)
             (guard (n>=1) >> ListHT.viewL xt)
   in  recourse 0

propDropFrac :: (RealField.C i, Eq a) => i -> Sig.T a -> Bool
propDropFrac n xs =
   dropFrac n xs == dropFrac' n xs



dropRem :: Int -> Sig.T a -> (Int, Sig.T a)
dropRem =
   let recourse n xt =
          if n>0
            then
               case xt of
                  _:xs -> recourse (pred n) xs
                  [] -> (n, [])
            else (n,xt)
   in  recourse

dropRem' :: Int -> Sig.T a -> (Int, Sig.T a)
dropRem' =
   let recourse n xt =
          maybe
             (n,xt)
             (recourse (pred n) . snd)
             (guard (n>0) >> ListHT.viewL xt)
   in  recourse

propDropRem :: (Eq a) => Int -> Sig.T a -> Bool
propDropRem n xs =
   dropRem n xs == dropRem' n xs

{-
*Synthesizer.Plain.ToneModulation> Test.QuickCheck.quickCheck (\n xs -> propDropRem n (xs::[Int]))
OK, passed 100 tests.
*Synthesizer.Plain.ToneModulation> Test.QuickCheck.quickCheck (\n xs -> propDropFrac (n::Rational) (xs::[Int]))
OK, passed 100 tests.
-}


oscillatorCoords :: (RealField.C t) =>
    Int -> t -> (t, Sig.T t) -> (Phase.T t, Sig.T t) -> Sig.T (ToneMod.Coords t)
oscillatorCoords periodInt period shapes freqs =
   map (mapSnd (ToneMod.flattenShapePhase periodInt period)) $
   integrateFractional period shapes freqs
{-
mapM print $ take 30 $ let period = 1/0.07::Double in oscillatorCoords (round period) period 0 0 (repeat 0.1) (repeat 0.01)

*Synthesizer.Plain.Oscillator> mapM print $ take 30 $ let period = 1/0.07::Rational in oscillatorCoords (round period) period 0 0 (repeat 1) (repeat 0.07)

*Synthesizer.Plain.Oscillator> mapM print $ take 30 $ let period = 1/0.07::Rational in oscillatorCoords (round period) period 0 0 (repeat 0.25) (repeat 0.0175)
-}


integrateFractional :: (RealField.C t) =>
    t -> (t, Sig.T t) -> (Phase.T t, Sig.T t) -> Sig.T (ToneMod.Skip t)
integrateFractional period (shape0, shapes) (phase, freqs) =
    let shapeOffsets =
           scanl
              (\(_,s) c -> splitFraction (s+c))
              (splitFraction shape0) shapes
        phases =
           let (s:ss) = map (\(n,_) -> fromIntegral n / period) shapeOffsets
           in  freqsToPhases
                  (Phase.decrement s phase)  -- phase - s
                  (zipWith (-) freqs ss)
    in  zipWith
           (\(d,s) p -> (d, (s,p)))
           shapeOffsets
           phases


-- this function fits better in the Oscillator module
{- |
Convert a list of phase steps into a list of momentum phases
phase is a number in the interval [0,1)
freq contains the phase steps
-}
freqsToPhases :: RealRing.C a => Phase.T a -> Sig.T a -> Sig.T (Phase.T a)
freqsToPhases phase freq = scanl (flip Phase.increment) phase freq



limitRelativeShapes :: (Ring.C t, Ord t) =>
    Margin ->
    Margin ->
    Int -> Sig.T y -> (t, Sig.T t) -> (t, Sig.T t)
limitRelativeShapes marginLeap marginStep periodInt sampledTone =
    let -- len = List.genericLength sampledTone
        len = Chunky.fromChunks (ListMatch.replicate sampledTone one)
        (minShape, maxShape) =
           ToneMod.shapeLimits marginLeap marginStep periodInt len
        fromChunky = NonNeg.toNumber   . Chunky.toNumber
        toChunky   = Chunky.fromNumber . NonNeg.fromNumber
    in  mapPair (fromChunky, map fromChunky) .
        uncurry (limitMaxRelativeValuesNonNeg maxShape) .
        mapPair (toChunky, map toChunky) .
        uncurry (limitMinRelativeValues (fromChunky minShape))
{-
*Synthesizer.Plain.Oscillator> let ip = Interpolation.linear in limitRelativeShapes ip ip 13 (take 100 $ iterate (1+) (0::Double)) (0::Double, cycle [0.5,1.5])
(13.0,[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5*** Exception: Numeric.NonNegative.Chunky.-: negative number
-}


limitMinRelativeValues :: (Additive.C a, Ord a) =>
   a -> a -> Sig.T a -> (a, Sig.T a)
limitMinRelativeValues xMin x0 xs =
   let (ys,zs) =
          span ((<zero).fst) (zip (scanl (+) (x0-xMin) xs) (x0:xs))
   in  case ys of
          [] -> (x0,xs)
          (_:yr) -> (xMin, ListMatch.replicate yr zero ++
              case zs of
                 [] -> []
                 (z:zr) -> fst z : map snd zr)

limitMaxRelativeValues :: (Additive.C a, Ord a) =>
   a -> a -> Sig.T a -> (a, Sig.T a)
limitMaxRelativeValues xMax x0 xs =
   let (ys,zs) =
          span (>zero) (scanl (-) (xMax-x0) xs)
   in  forcePair $
       ListHT.switchR
          (xMax, ListMatch.replicate xs zero)
          (\ yl yr -> (x0, ListMatch.take yl xs ++ ListMatch.take zs (yr : repeat zero)))
          ys

{- |
Avoids negative numbers and thus can be used with Chunky numbers.
-}
limitMaxRelativeValuesNonNeg :: (Additive.C a, Ord a) =>
   a -> a -> Sig.T a -> (a, Sig.T a)
limitMaxRelativeValuesNonNeg xMax x0 xs =
   let (ys,zs) =
          span fst (scanl (\(_,acc) d -> safeSub acc d) (safeSub xMax x0) xs)
   in  forcePair $
       ListHT.switchR
          (xMax, ListMatch.replicate xs zero)
          (\ yl ~(_,yr) -> (x0, ListMatch.take yl xs ++ ListMatch.take zs (yr : repeat zero)))
          ys
{-
*Synthesizer.Plain.Oscillator> limitMaxRelativeValuesNonNeg (let inf = 1+inf in inf) (0::Chunky.T NonNeg.Rational) (repeat 2.5)
-}

safeSub :: (Additive.C a, Ord a) => a -> a -> (Bool, a)
safeSub a b = (a>=b, a-b)
