module Synthesizer.Causal.ToneModulation (
   ToneModS.interpolateCell,
   seekCell,
   oscillatorCells,
   oscillatorSuffixes,
   integrateFractional,
   integrateFractionalClip,
   -- for testing
   limitRelativeShapes,
   limitMinRelativeValues,
   ) where

import qualified Synthesizer.Basic.ToneModulation as ToneMod
import qualified Synthesizer.Basic.Phase as Phase
import qualified Synthesizer.State.ToneModulation as ToneModS
import qualified Synthesizer.Interpolation as Interpolation
import qualified Synthesizer.Causal.Oscillator.Core as Osci
import qualified Synthesizer.Causal.Process as Causal
import qualified Synthesizer.Generic.Signal as SigG

import Control.Arrow (first, (<<<), (<<^), (^<<), (&&&), (***), )
import Control.Monad.Trans.State (state, )

{- for testing in GHCi
import qualified Synthesizer.Plain.ToneModulation as ToneModL
import qualified Synthesizer.State.Signal as SigS
import Data.Tuple.HT (mapFst, mapSnd, swap, )
-}
import Data.Tuple.HT (mapFst, )

import qualified Algebra.RealField             as RealField
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



oscillatorCells :: (RealField.C t, SigG.Transform sig y) =>
    Interpolation.Margin ->
    Interpolation.Margin ->
    Int -> t -> sig y -> (t, Phase.T t) ->
    Causal.T (t,t) ((t,t), ToneModS.Cell sig y)
oscillatorCells
       marginLeap marginStep periodInt period sampledTone (shape0, phase) =
    seekCell periodInt period
     ^<< oscillatorSuffixes marginLeap marginStep
            periodInt period sampledTone (shape0, phase)
{-
*Synthesizer.Causal.ToneModulation> let shapes = [0.3,2.4,0.2,2.1,1.2,1.5::Double]; phases = [0.43,0.72,0.91,0.37,0.42,0.22::Double]
*Synthesizer.Causal.ToneModulation> let marginLeap = Interpolation.Margin 3 1; marginStep = Interpolation.Margin 2 0
*Synthesizer.Causal.ToneModulation> mapM_ (print . mapSnd List.transpose) $ ToneModL.oscillatorCells marginLeap marginStep 5 5.3 ['a'..'z'] (2.3,shapes) (Phase.fromRepresentative 0.6, phases)
*Synthesizer.Causal.ToneModulation> mapM_ print $ SigS.toList $ oscillatorCells marginLeap marginStep 5 5.3 ['a'..'z'] (2.3, Phase.fromRepresentative 0.6) `Causal.apply` (SigS.fromList $ List.zip shapes phases)
-}


seekCell :: (RealField.C t, SigG.Transform sig y) =>
    Int -> t ->
    ((t, Phase.T t), sig y) ->
    ((t,t), ToneModS.Cell sig y)
seekCell periodInt period =
    (\(sp,ptr) ->
       let (k,q) = ToneMod.flattenShapePhase periodInt period sp
       in  (q, ToneModS.makeCell periodInt $
               SigG.drop (ToneModS.checkNonNeg $ periodInt+k) ptr))


{- |
In contrast to the counterpart of this function for plain lists,
it does not use sophisticated list transposition tricks,
but seeks through the prototype signal using 'drop'.
Since 'drop' is used in an inner loop, it must be fast.
This is true for StorableVectors.
-}
oscillatorSuffixes :: (RealField.C t, SigG.Transform sig y) =>
    Interpolation.Margin ->
    Interpolation.Margin ->
    Int -> t ->
    sig y -> (t, Phase.T t) ->
    Causal.T (t,t) ((t, Phase.T t), sig y)
oscillatorSuffixes
       marginLeap marginStep periodInt period sampledTone (shape0, phase) =
    let margin =
           ToneMod.interpolationNumber marginLeap marginStep periodInt
        ipOffset =
           periodInt +
           ToneMod.interpolationOffset marginLeap marginStep periodInt
        (shape0min, shapeLimiter) =
           limitMinRelativeValues (fromIntegral ipOffset) shape0
        ((skip0,coord0), coordinator) =
           integrateFractional period (shape0min, phase)
    in  (\(((b,n),ptr), sp@(_,p)) ->
           (if b
              then (zero, Phase.increment (fromIntegral n / period) p)
              else sp,
            ptr))
        ^<<
        (Causal.scanL
           (\ ((_,n),ptr) d -> dropMargin margin (n+d) ptr)
           (dropMargin margin (skip0 - ipOffset) sampledTone)
         ***
         Causal.consInit coord0)
        <<<
        coordinator
        <<<
        Causal.first shapeLimiter
{-
*Synthesizer.Causal.ToneModulation> let shapes = replicate 10 (2.6::Double); phases = cycle [0.43,0.72,0.91,0.37,0.42,0.22::Double]
*Synthesizer.Causal.ToneModulation> let marginLeap = Interpolation.Margin 3 1; marginStep = Interpolation.Margin 2 0
*Synthesizer.Causal.ToneModulation> mapM_ (print . swap . mapSnd (mapSnd (map head))) $ ToneModL.oscillatorSuffixes marginLeap marginStep 5 5.3 ['a'..'z'] (2.3,shapes) (Phase.fromRepresentative 0.6, phases)
*Synthesizer.Causal.ToneModulation> mapM_ print $ SigS.toList $ oscillatorSuffixes marginLeap marginStep 5 5.3 ['a'..'z'] (2.3, Phase.fromRepresentative 0.6) `Causal.apply` (SigS.fromList $ List.zip shapes phases)
-}

{- ToDo:
Both lengthAtMost and dropMarginRem seek through the list.
Maybe an improved version of dropMargin could avoid this.
E.g. dropMarginRem :: Int -> Int -> sig y -> (Maybe Int, sig y),
where return value (Just 0) means,
that drop could actually drop the requested number of elements,
but that we reached the end of the list.
-}
dropMargin :: (SigG.Transform sig y) =>
   Int -> Int -> sig y -> ((Bool, Int), sig y)
dropMargin margin n xs =
   mapFst ((,) (SigG.lengthAtMost (margin+n) xs)) $
   SigG.dropMarginRem margin (ToneModS.checkNonNeg n) xs

regroup :: (Int,t) -> Phase.T t -> ToneMod.Skip t
regroup (d,s) p = (d, (s,p))

integrateFractional :: (RealField.C t) =>
    t ->
    (t, Phase.T t) ->
    (ToneMod.Skip t, Causal.T (t,t) (ToneMod.Skip t))
integrateFractional period (shape0, phase) =
    let sf0 = splitFraction shape0
        -- shapeOffsets :: RealField.C t => Causal.T t (Int,t)
        shapeOffsets =
           Causal.fromState
              (\c -> state $ \s0 ->
                 let s1 = splitFraction (s0+c)
                 in  (s1, snd s1))
              (snd sf0)
        scale (n,_) = fromIntegral n / period
        -- phases :: RealField.C t => Causal.T ((Int,t), t) (Phase.T t)
        phase0 = Phase.decrement (scale sf0) phase
        phases =
           Osci.freqModSync phase0
              <<^ (\(s,f) -> f - scale s)
    in  (regroup sf0 phase0,
         uncurry regroup
         ^<<
         (Causal.map fst &&& phases)
         <<<
         first shapeOffsets)

{- |
Delays output by one element and shorten it by one element at the end.
-}
integrateFractionalClip :: (RealField.C t) =>
    t ->
    (t, Phase.T t) ->
    Causal.T (t,t) (ToneMod.Skip t)
integrateFractionalClip period (shape0, phase) =
    let sf0 = splitFraction shape0
        -- shapeOffsets :: RealField.C t => Causal.T t (Int,t)
        shapeOffsets =
           Causal.fromState
              (\c -> state $ \s0 ->
                 let s1 = splitFraction (s0+c)
                 in  (s1, snd s1))
              (snd sf0)
        scale (n,_) = fromIntegral n / period
        -- phases :: RealField.C t => Causal.T ((Int,t), t) (Phase.T t)
        phases =
           Osci.freqMod
              (Phase.decrement (scale sf0) phase)
              <<^ (\(s,f) -> f - scale s)
    in  uncurry regroup
        ^<<
        ((Causal.consInit sf0 <<^ fst) &&& phases)
        <<<
        first shapeOffsets
{-
test to automate:
*Synthesizer.Generic.ToneModulation> let shapes = [0.3,0.4,0.2::Double]; phases = [0.43,0.72,0.91::Double]
*Synthesizer.Generic.ToneModulation> ToneMod.oscillatorCoords 9 10 (2.3,shapes) (Phase.fromRepresentative 0.6, phases)
[(2,(-6,(0.63,0.6299999999999999))),(0,(-2,(0.22999999999999998,0.53))),(0,(-4,(0.5500000000000002,4.9999999999998934e-2))),(1,(-6,(0.6600000000000001,0.2599999999999989)))]

*Synthesizer.Generic.ToneModulation> ToneModS.oscillatorCoords 9 10 (2.3, SigS.fromList shapes) (Phase.fromRepresentative 0.6, SigS.fromList phases)
StateSignal.fromList [(2,(-6,(0.63,0.6299999999999999))),(0,(-2,(0.22999999999999998,0.53))),(0,(-4,(0.5500000000000002,4.9999999999998934e-2)))]

*Synthesizer.Generic.ToneModulation> Data.Tuple.HT.mapSnd (flip Causal.apply $ SigS.fromList (zip shapes phases)) $ oscillatorCoords 9 10 (2.3, Phase.fromRepresentative 0.6)
((2,(-6,(0.63,0.6299999999999999))),StateSignal.fromList [(0,(-2,(0.22999999999999998,0.53))),(0,(-4,(0.5500000000000002,4.9999999999998934e-2))),(1,(-6,(0.6600000000000001,0.2599999999999989)))])

*Synthesizer.Generic.ToneModulation> oscillatorCoords' 9 10 (2.3, Phase.fromRepresentative 0.6) `Causal.apply` SigS.fromList (zip shapes phases)
StateSignal.fromList [(2,(-6,(0.63,0.6299999999999999))),(0,(-2,(0.22999999999999998,0.53))),(0,(-4,(0.5500000000000002,4.9999999999998934e-2)))]
-}

limitRelativeShapes :: (Ring.C t, Ord t) =>
    Interpolation.Margin ->
    Interpolation.Margin ->
    Int -> t -> (t, Causal.T t t)
limitRelativeShapes marginLeap marginStep periodInt =
    limitMinRelativeValues $ fromIntegral $
    ToneMod.interpolationOffset marginLeap marginStep periodInt + periodInt

limitMinRelativeValues :: (Additive.C t, Ord t) =>
   t -> t -> (t, Causal.T t t)
limitMinRelativeValues xMin x0 =
   let x1 = xMin-x0
   in  if x1<=zero
         then (x0, Causal.id)
         else (xMin,
               Causal.crochetL
                  (\x lim -> Just $
                     let d = x-lim
                     in  if d>=zero
                           then (d,zero) else (zero, negate d)) x1)
