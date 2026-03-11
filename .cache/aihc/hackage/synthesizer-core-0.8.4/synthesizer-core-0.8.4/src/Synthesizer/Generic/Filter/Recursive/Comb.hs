{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Comb filters, useful for emphasis of tones with harmonics
and for repeated echos.
-}
module Synthesizer.Generic.Filter.Recursive.Comb (
   karplusStrong,
   run,
   runMulti,
   runProc,
   ) where

import qualified Synthesizer.Generic.Filter.NonRecursive as Filt
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG

import qualified Algebra.Module                as Module
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
The most simple version of the Karplus-Strong algorithm
which is suitable to simulate a plucked string.
It is similar to the 'runProc' function.
-}
{-# INLINE karplusStrong #-}
karplusStrong ::
   (Ring.C t, Module.C t y, SigG.Write sig y) =>
   Filt1.Parameter t -> sig y -> sig y
karplusStrong c wave =
   SigG.delayLoop (SigG.modifyStatic Filt1.lowpassModifier c) wave


{- |
Infinitely many equi-delayed exponentially decaying echos.
The echos are clipped to the input length.
We think it is easier (and simpler to do efficiently)
to pad the input with zeros or whatever
instead of cutting the result according to the input length.
-}
{-# INLINE run #-}
run :: (Module.C t y, SigG.Write sig y) =>
   Int -> t -> sig y -> sig y
run time gain =
   runProc time (Filt.amplifyVector gain)

{- |
Echos of different delays.
Chunk size must be smaller than all of the delay times.
-}
{-# INLINE runMulti #-}
runMulti :: (Module.C t y, SigG.Write sig y) =>
   [Int] -> t -> sig y -> sig y
runMulti times gain x =
    let y = foldl
               (SigG.zipWith (+)) x
               (map (flip Filt.delay (Filt.amplifyVector gain y)) times)
--               (map (flip Delay.staticPos (gain *> y)) times)
    in  y

{- | Echos can be piped through an arbitrary signal processor. -}
{-# INLINE runProc #-}
runProc :: (Additive.C y, SigG.Write sig y) =>
   Int -> (sig y -> sig y) -> sig y -> sig y
runProc = SigG.delayLoopOverlap


{- |
Alternative to 'run' that uses 'CutG.splitAt' at the beginning
instead of adding a zero signal.
-}
_run :: (Module.C t y, SigG.Transform sig y) => t -> Int -> sig y -> sig y
_run gain delay xs =
   let (xs0,xs1) = CutG.splitAt delay $ Filt.amplifyVector (1-gain) xs
       ys = CutG.append xs0 $ SigG.zipWith (+) xs1 $ Filt.amplifyVector gain ys
   in  ys

_runInf :: (Module.C t y, SigG.Write sig y) => t -> Int -> sig y -> sig y
_runInf gain delay xs =
   let (xs0,xs1) =
          CutG.splitAt delay $
          Filt.amplifyVector (1-gain) xs `CutG.append`
             SigG.repeat (SigG.LazySize delay) zero
       ys = CutG.append xs0 $ SigG.zipWith (+) xs1 $ Filt.amplifyVector gain ys
   in  ys
