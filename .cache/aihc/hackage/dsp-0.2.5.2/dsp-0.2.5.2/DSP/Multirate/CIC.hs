-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Multirate.CIC
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- CIC filters
--
-- R = rate change
--
-- M = differential delay in combs
--
-- N = number of stages
--
-----------------------------------------------------------------------------

{-

An implementation in Haskell of the description of CIC decimator and
interpolators as described in:

@Article{Hogenauer_AnEcon_ASSP81,
  journal =      "{IEEE} Trans. Acoustics, Speech and Signal
                 Processing",
  author =       "E. B. Hogenauer",
  title =        "An Economical Class of Digital Filters for Decimation
                 and Interpolation",
  year =         "1981",
  volume =       "{ASSP-29}",
  number =       "2",
  pages =        "155",
}

Note that this implementation does not account for the overflow
handling, bit growth, etc., described in the paper, but this does not
matter for real or complex data.

-}

module DSP.Multirate.CIC (cic_interpolate, cic_decimate) where

import DSP.Basic (delay1, delay, upsample, downsample)

-- apply returns a function of n applications of a function, eg,

--	apply f 3 = f . f . f

-- We will use this to create a cascade of integrators and combs

apply :: (a -> a) -> Int -> (a -> a)
apply f 1 = f
apply f n = f . apply f (n - 1)

-- integrate implements a discrte integrator, ie, the output is the sum
-- of all previous samples and the current one, eg

--	integrate [ 1, 1, 1, 1 ] = [ 1, 2, 3, 4 ]

integrate :: (Num a) => [a] -> [a]
integrate a = zipWith (+) a (delay1 (integrate a))

-- comb implements the comb function described in the paper above.  The m
-- parameter is the length of the delay in the feed-forward element.

comb :: (Num a) => Int -> [a] -> [a]
comb m a = zipWith (-) a (delay m a)

{-

It is now simple to create a CIC imterpolator or decimator.  In the
functions below

	r is the rate change
	m is the length of the delay in the feed-forward element of the combs
	n is the number of stages (the number of integrators and combs)

integrate_chain and comb_chain are the cascade of integrator and combs
(hence the name CIC filter).  We then just slap the functions together
with the application operator.  There is a non unity gain that I
should probably account for, but that cound be swallowed up in another
function.

-}

-- | CIC interpolator

cic_interpolate :: (Num a) => Int -- ^ R
		-> Int -- ^ M
		-> Int -- ^ N
		-> [a] -- ^ x[n]
		-> [a] -- ^ y[n]

cic_interpolate r m n = integrate_chain . (upsample r) . comb_chain
    where integrate_chain = apply integrate n
          comb_chain = apply (comb m) n

-- | CIC interpolator

cic_decimate :: (Num a) => Int -- ^ R
	     -> Int -- ^ M
	     -> Int -- ^ N
	     -> [a] -- ^ x[n]
	     -> [a] -- ^ y[n]

cic_decimate r m n = comb_chain . (downsample r) . integrate_chain
    where integrate_chain = apply integrate n
          comb_chain = apply (comb m) n
