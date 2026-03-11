{- |
Copyright   :  (c) Henning Thielemann 2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Causal.Analysis (
   deltaSigmaModulationPositive,
   ) where

import qualified Synthesizer.Causal.Analysis as Ana
import qualified Synthesizer.Causal.Filter.NonRecursive as FiltNR

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Sample as Sample
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Causal.Process as CausalD

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
import Number.DimensionTerm ((&*&), )

import qualified Algebra.Field          as Field
import qualified Algebra.RealRing       as RealRing

import Control.Arrow (second, (<<<), )

import NumericPrelude.Base
-- import NumericPrelude.Numeric
import Prelude ()


type DNS v y yv = Sample.Dimensional v y yv



deltaSigmaModulationPositive ::
   (RealRing.C a, Field.C a, Dim.C u, Dim.C v) =>
   Proc.T s u a (CausalD.T s (DNS (Dim.Mul u v) a a, DNS v a a) (DNS v a a))
deltaSigmaModulationPositive =
   flip fmap Proc.getSampleRate $ \rate ->
      CausalD.consFlip $ \ (Amp.Numeric thresholdAmp, Amp.Numeric inputAmp) ->
         let targetAmp =
                DN.rewriteDimension
                   (Dim.identityLeft .
                    Dim.applyLeftMul Dim.cancelLeft .
                    Dim.associateLeft) $
                rate &*& thresholdAmp
             ampRatio = DN.divToScalar inputAmp targetAmp
         in  (Amp.Numeric targetAmp,
              Ana.deltaSigmaModulationPositive
              <<<
              second (FiltNR.amplify ampRatio))
