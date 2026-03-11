{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Storable.Play where

import qualified Synthesizer.Basic.Binary as BinSmp

import qualified Sound.Sox.Option.Format as SoxOpt
import qualified Sound.Sox.Play as Play

import Foreign.Storable (Storable, )

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Frame.Stereo as Stereo

import System.Exit (ExitCode, )

import qualified Algebra.RealRing as RealRing

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
Latency is high using Sox -
We can achieve better results using ALSA's sound output!
See synthesizer-alsa package.
-}
monoToInt16 ::
   (Storable a, RealRing.C a) =>
   a -> SigSt.T a -> IO ExitCode
monoToInt16 rate =
   Play.simple SigSt.hPut SoxOpt.none (round rate) .
   SigSt.map BinSmp.int16FromCanonical

stereoToInt16 ::
   (Storable a, RealRing.C a) =>
   a -> SigSt.T (Stereo.T a) -> IO ExitCode
stereoToInt16 rate =
   Play.simple SigSt.hPut SoxOpt.none (round rate) .
   SigSt.map (\y -> Stereo.cons
                       (BinSmp.int16FromCanonical $ Stereo.left y)
                       (BinSmp.int16FromCanonical $ Stereo.right y))
