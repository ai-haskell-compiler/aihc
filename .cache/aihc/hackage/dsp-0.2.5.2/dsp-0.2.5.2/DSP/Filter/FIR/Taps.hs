-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.Taps
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for creating rectangular windowed FIR filters
--
-----------------------------------------------------------------------------

{-
Reference:

@Book{dsp,
  author = 	 "Alan V. Oppenheim and Ronald W. Schafer",
  title = 	 "Discrete-Time Signal Processing",
  publisher = 	 "Prentice-Hall",
  year = 	 1989,
  address =	 "Englewood Cliffs",
  series =       {Prentice-Hall Signal Processing Series}
}
-}

module DSP.Filter.FIR.Taps (lpf, hpf, bpf, bsf, mbf, rc) where

import Data.Array

-- indexes generates the list of indexes that we will map the prototype
-- functions onto

indexes :: (Integral a, Num b, Enum b) => a -> [b]
indexes m = [ 0 .. fromIntegral m ]

-- the _tap functions generate one tap for the given function

-- wc = cutoff frequency in normalized radians
-- m = the order of the filter (length - 1)
-- n = the tap number

-- Lowpass tap function

lpf_tap :: (Integral a, Floating b, Eq b) => b -> a -> b -> b
lpf_tap wc m n | n-a == 0  = wc / pi
               | otherwise = sin (wc * (n-a)) / (pi * (n-a))
    where a = (fromIntegral m) / 2

-- Highpass tap function

hpf_tap :: (Integral a, Floating b, Eq b) => b -> a -> b -> b
hpf_tap wc m n | n-a == 0  = 1 - wc / pi
               | otherwise = sin (pi * (n-a)) / (pi * (n-a)) - lpf_tap wc m n
    where a = (fromIntegral m) / 2

-- Multiband tap function

mbf_tap :: (Integral a, Floating b, Eq b) => [b] -> [b] -> a -> b -> b
mbf_tap (g:[])     (w:[]) m n = g * lpf_tap w m n
mbf_tap (g1:g2:gs) (w:ws) m n = (g1-g2) * lpf_tap w m n + mbf_tap (g2:gs) ws m n
mbf_tap _          _      _ _ = error "mbf_tap: bands out of sync"

-- Raised-cosine tap function.  This does _not_ have 0 dB DC gain.

-- ws = symbol rate in normalized radians
-- b = filter beta

rc_tap :: (Integral a, Floating b, Eq b) => b -> b -> a -> b -> b
rc_tap ws b m n | n-a == 0  = 1
                | den == 0  = 0
                | otherwise = sin sarg / sarg * cos carg / den
    where sarg = ws * (n-a) / 2
          carg = b * ws * (n-a) / 2
          den = 1 - 4 * ((b*ws*(n-a)) / (2*pi)) ^ (2::Int)
          a = (fromIntegral m) / 2

-- The following functions generate a list of the taps for a given set of
-- parameter.

-- | Lowpass filter

lpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wc
       -> a -- ^ M
       -> Array a b -- ^ h[n]

lpf wc m = listArray (0,m) $ map (lpf_tap wc m) (indexes m)

-- | Highpass filter

hpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wc
       -> a -- ^ M
       -> Array a b -- ^ h[n]

hpf wc m = listArray (0,m) $ map (hpf_tap wc m) (indexes m)

-- | Bandpass filter

bpf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wl
       -> b -- ^ wu
       -> a -- ^ M
       -> Array a b -- ^ h[n]

bpf wl wu m = listArray (0,m) $ zipWith (+) (elems $ lpf wu m) (elems $ hpf wl m)

-- | Bandstop filter

bsf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ wl
       -> b -- ^ wu
       -> a -- ^ M
       -> Array a b -- ^ h[n]

bsf wl wu m = listArray (0,m) $ zipWith (+) (elems $ lpf wl m) (elems $ hpf wu m)

-- | Multiband filter

mbf :: (Ix a, Integral a, Enum b, Floating b, Eq b) => [b] -- ^ [mags]
       -> [b] -- ^ [w]
       -> a -- ^ M
       -> Array a b -- ^ h[n]

mbf g w m = listArray (0,m) $ map (mbf_tap g w m) (indexes m)

-- | Raised-cosine filter

rc :: (Ix a, Integral a, Enum b, Floating b, Eq b) => b -- ^ ws
       -> b -- ^ beta
       -> a -- ^ M
       -> Array a b -- ^ h[n]

rc ws b m = listArray (0,m) $ map (rc_tap ws b m) (indexes m)
