{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Basic.DistortionControlled (
   clip, logit,
   zigZag, sine,
   quantize,
   ) where

import qualified Synthesizer.Basic.Distortion  as Dist

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing

import Data.Ord.HT (limit, )

import NumericPrelude.Numeric


{- * Clipping -}

{- |
limit, fuzz booster
-}
clip :: (RealRing.C a) => a -> a -> a
clip c = limit (negate c, c)

{- |
logit, tanh
-}
logit :: (Trans.C a) => a -> a -> a
logit k = rescale k Dist.logit

{-
probit, error function
-}



{- * Wrapping -}

{- |
zig-zag
-}
zigZag :: (RealField.C a) => a -> a -> a
zigZag k = rescale k Dist.zigZag

{- |
sine
-}
sine :: (Trans.C a) => a -> a -> a
sine k = rescale k Dist.sine




{- * Quantization -}

quantize :: (RealField.C a) => a -> a -> a
quantize k = rescale k Dist.quantize



{- Auxilary function -}

rescale :: (Field.C a) => a -> (a -> a) -> a -> a
rescale k f x = k * f (x/k)

{-
*Synthesizer.Basic.Distortion> GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (-3,3::Double)) (map logit [0,0.1..1])
-}
