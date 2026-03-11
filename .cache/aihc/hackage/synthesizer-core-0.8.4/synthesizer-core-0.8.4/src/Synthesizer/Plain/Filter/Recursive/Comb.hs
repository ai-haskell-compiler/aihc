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
module Synthesizer.Plain.Filter.Recursive.Comb where

import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import Synthesizer.Plain.Filter.NonRecursive (delay, )

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
karplusStrong :: (Ring.C a, Module.C a v) =>
   Filt1.Parameter a -> Sig.T v -> Sig.T v
karplusStrong c wave =
    let y = wave ++ Filt1.lowpass (Ctrl.constant c) y
    in  y


{- |
Infinitely many equi-delayed exponentially decaying echos.
The echos are clipped to the input length.
We think it is easier (and simpler to do efficiently)
to pad the input with zeros or whatever
instead of cutting the result according to the input length.
-}
run :: (Module.C a v) => Int -> a -> Sig.T v -> Sig.T v
run time gain x =
    let y = zipWith (+) x (delay time (gain *> y))
    in  y

{- | Echos of different delays. -}
runMulti :: (Ring.C a, Module.C a v) => [Int] -> a -> Sig.T v -> Sig.T v
runMulti time gain x =
    let y = foldl (zipWith (+)) x (map (flip delay (gain *> y)) time)
    in  y

{- | Echos can be piped through an arbitrary signal processor. -}
runProc :: Additive.C v => Int -> (Sig.T v -> Sig.T v) -> Sig.T v -> Sig.T v
runProc time feedback x =
    let y = zipWith (+) x (delay time (feedback y))
    in  y
