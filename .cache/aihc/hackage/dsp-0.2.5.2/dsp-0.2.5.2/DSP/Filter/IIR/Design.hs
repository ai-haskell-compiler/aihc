-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Design
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Lowpass, Highpass, Bandpass IIR design functions
--
-- Method:
--
-- (1) Design analog prototype
--
-- 2.  Perform analog-to-analog frequency transformation
--
-- 3.  Perform bilinear transform
--
-----------------------------------------------------------------------------

module DSP.Filter.IIR.Design (
   poly2iir,
   butterworthLowpass,  butterworthHighpass, butterworthBandpass,
   chebyshev1Lowpass, chebyshev2Lowpass,
   mkButterworth, mkChebyshev1, mkChebyshev2,
   ) where

import qualified DSP.Filter.Analog.Prototype as Analog
import DSP.Filter.Analog.Transform (a_lp2lp, a_lp2hp, a_lp2bp)
import DSP.Filter.IIR.Bilinear (bilinear, prewarp)

import DSP.Basic ((^!))

import Data.Array (Array, listArray)


poly2iir :: ([a], [b]) -> (Array Int a, Array Int b)
poly2iir (a,b) =
   let toArray x = listArray (0, length x - 1) $ reverse x
   in  (toArray a, toArray b)


-- | Generates lowpass Butterworth IIR filters

butterworthLowpass, mkButterworth ::
      (Double, Double) -- ^ (wp,dp)
   -> (Double, Double) -- ^ (ws,ds)
   -> (Array Int Double, Array Int Double) -- ^ (b,a)
butterworthLowpass p s =
   let (n, s') = butterworthParams p s
   in  butterworth (a_lp2lp $ wc n s') n

butterworthHighpass, butterworthBandpass ::
   (Double, Double) ->
   (Double, Double) ->
   (Array Int Double, Array Int Double)
butterworthHighpass p s =
   let (n, s') = butterworthParams p s
   in  butterworth (a_lp2hp $ wc n s') n

butterworthBandpass p@(wp, _dp) s@(ws, _ds) =
   let (n, _s') = butterworthParams p s
   in  butterworth (a_lp2bp wp ws) n

butterworth ::
   (([Double], [Double]) -> ([Double], [Double])) ->
   Int -> (Array Int Double, Array Int Double)
butterworth analogToAnalog n =
   poly2iir $ bilinear 1 $ analogToAnalog $ Analog.butterworth n

butterworthParams ::
   (Double, Double) ->
   (Double, Double) ->
   (Int, (Double, Double))
butterworthParams (wp, dp) (ws, ds) =
   let n = ceiling $ log (((1/ds)^!2-1) / ((1/(1-dp))^!2-1)) / 2 / log (ws' / wp')
       wp' = prewarp wp 1
       ws' = prewarp ws 1
   in  (n, (ws', ds))

wc :: Floating a => Int -> (a, a) -> a
wc n (ws', ds) = ws' / ((1/ds)^!2 - 1) ** (1/2/fromIntegral n)


{-# DEPRECATED mkButterworth "Use butterworthLowpass instead" #-}
mkButterworth = butterworthLowpass


-- | Generates lowpass Chebyshev IIR filters

chebyshev1Lowpass, mkChebyshev1 ::
      (Double, Double) -- ^ (wp,dp)
   -> (Double, Double) -- ^ (ws,ds)
   -> (Array Int Double, Array Int Double) -- ^ (b,a)

chebyshev1Lowpass (wp,dp) (ws,ds) = poly2iir    $
			       bilinear 1  $
			       a_lp2lp wp' $
			       Analog.chebyshev1 eps n
    where wp' = prewarp wp 1
          ws' = prewarp ws 1
	  eps = sqrt ((2 - dp)*dp) / (1 - dp)
	  a   = 1 / ds
	  k1  = eps / sqrt (a^!2 - 1)
	  k   = wp' / ws'
	  n   = ceiling $ acosh (1/k1) / log ((1 + sqrt (1 - k^!2)) / k)

{-# DEPRECATED mkChebyshev1 "Use chebyshev1Lowpass instead" #-}
mkChebyshev1 = chebyshev1Lowpass


-- | Generates lowpass Inverse Chebyshev IIR filters

chebyshev2Lowpass, mkChebyshev2 ::
      (Double, Double) -- ^ (wp,dp)
   -> (Double, Double) -- ^ (ws,ds)
   -> (Array Int Double, Array Int Double) -- ^ (b,a)

chebyshev2Lowpass (wp,dp) (ws,ds) = poly2iir    $
			       bilinear 1  $
			       a_lp2lp ws' $
			       Analog.chebyshev2 eps n
    where wp' = prewarp wp 1
          ws' = prewarp ws 1
	  eps = ds / sqrt (1 - ds^!2)
	  g = 1 - dp
	  n   = ceiling $ acosh (g / eps / sqrt (1 - g^!2)) / acosh (ws' / wp')

{-# DEPRECATED mkChebyshev2 "Use chebyshev2Lowpass instead" #-}
mkChebyshev2 = chebyshev2Lowpass
