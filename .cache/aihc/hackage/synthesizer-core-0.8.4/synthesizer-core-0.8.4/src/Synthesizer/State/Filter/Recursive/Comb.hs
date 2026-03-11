{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Comb filters, useful for emphasis of tones with harmonics
and for repeated echos.
-}
module Synthesizer.State.Filter.Recursive.Comb where

import qualified Synthesizer.State.Signal  as Sig
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1

import qualified Synthesizer.State.Filter.Delay as Delay

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
karplusStrong :: (Ring.C a, Module.C a v) =>
   Filt1.Parameter a -> Sig.T v -> Sig.T v
karplusStrong c wave =
   Sig.delayLoop (Sig.modifyStatic Filt1.lowpassModifier c) wave


{- |
Infinitely many equi-delayed exponentially decaying echos.
The echos are clipped to the input length.
We think it is easier (and simpler to do efficiently)
to pad the input with zeros or whatever
instead of cutting the result according to the input length.
-}
{-# INLINE run #-}
run :: (Module.C a v) => Int -> a -> Sig.T v -> Sig.T v
run time gain = Sig.delayLoopOverlap time (gain *>)

{- | Echos of different delays. -}
{-# INLINE runMulti #-}
runMulti :: (Ring.C a, Module.C a v) => [Int] -> a -> Sig.T v -> Sig.T v
runMulti times gain x =
    let y = Sig.fromList $ Sig.toList $
            foldl
               (Sig.zipWith (+)) x
               (map (flip Delay.staticPos (gain *> y)) times)
    in  y

{- | Echos can be piped through an arbitrary signal processor. -}
{-# INLINE runProc #-}
runProc :: Additive.C v => Int -> (Sig.T v -> Sig.T v) -> Sig.T v -> Sig.T v
runProc = Sig.delayLoopOverlap
