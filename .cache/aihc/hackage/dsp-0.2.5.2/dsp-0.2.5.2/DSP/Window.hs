-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Window
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used window functions.  Except for the Parzen window, the
-- results of all of these /look/ right, but I have to check them against
-- either Matlab or my C code.
--
-- More windowing functions exist, but I have to dig through my papers to
-- find the equations.
--
-----------------------------------------------------------------------------

-- TODO: These functions should probably be reworked to use list
-- comprehensions...

{-

Reference:

@Book{dsp,
  author =       "Alan V. Oppenheim and Ronald W. Schafer",
  title =        "Discrete-Time Signal Processing",
  publisher =    "Prentice-Hall",
  year =         1989,
  address =      "Englewood Cliffs",
  series =       {Prentice-Hall Signal Processing Series}
}

@Book{kay,
  author =       "Steven M. Kay",
  title =        "Modern Spectral Estimation: Theory \& Application",
  publisher =    "Prentice Hall",
  year =         1988,
  address =      "Englewood Cliffs",
  series =       {Prentice-Hall Signal Processing Series}
}

-}

module DSP.Window
    (window, rectangular, bartlett, hanning, hamming, blackman,
     kaiser, gen_hamming, parzen) where

import DSP.Basic ((^!))
import Data.Array


-- | Applys a window, @w@, to a sequence @x@

window :: Array Int Double -- ^ w[n]
       -> Array Int Double -- ^ x[n]
       -> Array Int Double -- ^ w[n] * x[n]

window w x = listArray (0,m) [ w!i * x!i | i <- [0..m] ]
    where m = snd $ bounds w

-- | rectangular window

rectangular :: Int -- ^ M
            -> Array Int Double -- ^ w[n]

rectangular m = listArray (0,m) $ repeat 1

-- | Bartlett  window

bartlett :: Int -- ^ M
         -> Array Int Double -- ^ w[n]

bartlett = makeArray bartlett'

bartlett' :: Double -> Double -> Double
bartlett' m n =
   if n <= m / 2
     then 2 * n / m
     else 2 - 2 * n / m

-- | Hanning window

hanning :: Int -- ^ M
        -> Array Int Double -- ^ w[n]

hanning = makeArray hanning'

hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)

-- | Hamming window

hamming :: Int -- ^ M
        -> Array Int Double -- ^ w[n]

hamming = makeArray hamming'

hamming' :: Double -> Double -> Double
hamming' m n = 0.54 - 0.46 * cos(2 * pi * n / m)


-- | Blackman window

blackman :: Int -- ^ M
         -> Array Int Double -- ^ w[n]

blackman = makeArray blackman'

blackman' :: Double -> Double -> Double
blackman' m n =
   0.42 - 0.5 * cos(2 * pi * n / m) +
   0.08 * cos (4 * pi * n / m)

-- | Generalized Hamming window

gen_hamming :: Double -- ^ alpha
            -> Int -- ^ M
            -> Array Int Double -- ^ w[n]

gen_hamming = makeArray . gen_hamming'

gen_hamming' :: Double -> Double -> Double -> Double
gen_hamming' a m n = a - (1 - a) * cos(2 * pi * n / m)

-- | rectangular window

kaiser :: Double -- ^ beta
       -> Int -- ^ M
       -> Array Int Double -- ^ w[n]

kaiser = makeArray . kaiser'

kaiser' :: Double -> Double -> Double -> Double
kaiser' b m n =
   let a = m / 2
   in  i0 (b * sqrt (1 -((n-a)/a)^!2)) / i0 b


-- Recursive computation of I0, the zeroth-order modified Bessel function
-- of the first kind.

i0  :: Double -> Double
i0 x = i0' x 2 1

i0' :: Double -> Double -> Double -> Double
i0' x d ds | ds < 1.0e-30 = 1
           | otherwise = ds * x^!2 / d^!2 + (i0' x (d+2) (ds * x^!2 / d^!2))

-- I don't think this one is correct.  Kay's book uses different variable
-- conventions and I haven't deciphered them yet...

-- | rectangular window

parzen :: Int -- ^ M
       -> Array Int Double -- ^ w[n]

parzen = makeArray parzen'

parzen' :: Double -> Double -> Double
parzen' m n =
   if n <= m / 2
     then 2 * (1-n/m) ^! 3 - (1-2*n/m) ^! 3
     else 2 * (1-n/m) ^! 3


makeArray :: (Double -> Double -> Double) -> Int -> Array Int Double
makeArray win m =
   let md = fromIntegral m
   in  listArray (0,m) $ map (win md . fromIntegral) [(0::Int) ..]
