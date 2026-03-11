-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Transform
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Digital IIR filter transforms
--
-- Reference: R&G, pg 260; O&S, pg 434; P&M, pg 699
--
-- Notation follows O&S
--
-----------------------------------------------------------------------------

-- TODO: These need more testing.  I checked the lp2hp case against O&S
-- which verifies substitute and lp2hp,nd I triple checked the parameters
-- for the others.  I need to find test vectors for the other cases for
-- proper testing, though.

module DSP.Filter.IIR.Transform (d_lp2lp, d_lp2hp, d_lp2bp, d_lp2bs) where

import DSP.Filter.Analog.Transform (substitute)
import Numeric.Special.Trigonometric (cot)


-- | Lowpass to lowpass: @z^-1 --> (z^-1 - a)\/(1 - a*z^-1)@

d_lp2lp :: Double -- ^ theta_p
        -> Double -- ^ omega_p
        -> ([Double], [Double]) -- ^ (b,a)
        -> ([Double], [Double]) -- ^ (b',a')

d_lp2lp tp wp (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [-a, 1]
          dsub = [1, -a]
          a = sin ((tp-wp)/2) / sin ((tp+wp)/2)

-- | Lowpass to Highpass: @z^-1 --> -(z^-1 + a)\/(1 + a*z^-1)@

d_lp2hp :: Double -- ^ theta_p
        -> Double -- ^ omega_p
        -> ([Double], [Double]) -- ^ (b,a)
        -> ([Double], [Double]) -- ^ (b',a')

d_lp2hp tp wp (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [a, 1]
          dsub = [-1, -a]
          a = -cos ((tp+wp)/2) / cos ((tp-wp)/2)

-- | Lowpass to Bandpass: z^-1 -->

d_lp2bp :: Double -- ^ theta_p
        -> Double -- ^ omega_p1
        -> Double -- ^ omega_p2
        -> ([Double], [Double]) -- ^ (b,a)
        -> ([Double], [Double]) -- ^ (b',a')

d_lp2bp tp wp1 wp2 (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [ (k-1)/(k+1), -2*a*k/(k+1), 1 ]
          dsub = [ 1, -2*a*k/(k+1), (k-1)/(k+1) ]
          a = cos ((wp2+wp1)/2) / cos ((wp2-wp1)/2)
          k = cot ((wp2-wp1)/2) * tan (tp/2)

-- | Lowpass to Bandstop: z^-1 -->

d_lp2bs :: Double -- ^ theta_p
        -> Double -- ^ omega_p1
        -> Double -- ^ omega_p2
        -> ([Double], [Double]) -- ^ (b,a)
        -> ([Double], [Double]) -- ^ (b',a')

d_lp2bs tp wp1 wp2 (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [ (1-k)/(1+k), -2*a/(1+k), 1 ]
          dsub = [ 1, -2*a/(1+k), (1-k)/(1+k) ]
          a = cos ((wp2+wp1)/2) / cos ((wp2-wp1)/2)
          k = cot ((wp2-wp1)/2) * tan (tp/2)

{-

Test vectors

O&S, pg 435

 num = polypow  [ 0.001836, 0.001836 ] 4
 den = polymult [ 0.6493, -1.5548, 1 ] [ 0.8482, -1.4996, 1 ]

-}
