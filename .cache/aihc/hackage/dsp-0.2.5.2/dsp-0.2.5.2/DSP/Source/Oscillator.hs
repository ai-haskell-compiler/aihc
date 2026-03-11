-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Source.Oscillator
-- Copyright   :  (c) Matthew Donadio 1998,2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- NCO and NCOM functions
--
-----------------------------------------------------------------------------

module DSP.Source.Oscillator (nco, ncom,
			      quadrature_nco, complex_ncom,
			      quadrature_ncom,
                              agc) where

import Data.Complex

-- | 'nco' creates a sine wave with normalized frequency wn (numerically
-- controlled oscillator, or NCO) using the recurrence relation y[n] =
-- 2cos(wn)*y[n-1] - y[n-2].  Eventually, cumulative errors will creep
-- into the data.  This is unavoidable since performing AGC on this type
-- of real data is hard.  The good news is that the error is small with
-- floating point data.

nco :: RealFloat a => a -- ^ w
    -> a -- ^ phi
    -> [a] -- ^ y

nco wn phi = y
    where a0 = 2 * cos wn
	  y1 = -(sin (wn + phi)) : y
          y2 = -(sin (2 * wn + phi)) : y1
          y  = zipWith (-) (map (a0 *) y1) y2

-- | 'ncom' mixes (multiplies) x by a real sine wave with normalized
-- frequency wn.  This is usually called an NCOM: Numerically Controlled
-- Oscillator and Modulator.

ncom :: RealFloat a => a -- ^ w
     -> a -- ^ phi
     -> [a] -- ^ x
     -> [a] -- ^ y

ncom wn phi x = zipWith (*) x (nco wn phi)

-- agc is used in quadrature_nco (below) to scale a complex phasor to
-- have length as close to 1 as possible, ie perform some automatic gain
-- control.  Since we aren't computing sin and cos for each sample, not
-- using AGC would results in cumulative errors (small one with floating
-- point data).  The Complex class includes the signum function which
-- will do what we want, but we will use the approximation 1/sqrt(x) ~=
-- (3-x)/2 for x ~= 1 to eliminate doing a sqrt for every point.

agc         :: RealFloat a => Complex a -> Complex a
agc (x:+y) = x * r :+ y * r
    where r = (3 - x * x - y * y) / 2

-- | 'quadrature_nco' returns an infinite list representing a complex phasor
-- with a phase step of wn radians, ie a quadrature nco with normalized
-- frequency wn radians\/sample.  Since Haskell uses lazy evaluation,
-- rotate will only be computed once, so this NCO uses only one sin and
-- one cos for the entire list, at the expense of 4 mults, 1 add, and 1
-- subtract per point.

quadrature_nco :: RealFloat a => a -- ^ w
	       -> a -- ^ phi
	       -> [ Complex a ] -- ^ y

quadrature_nco wn phi = (cis phi) : map ((*) (cis wn)) (quadrature_nco wn phi)

-- | 'complex_ncom' mixes the complex input x with a quardatue nco with
-- normalized frequency wn radians\/sample using complex multiplies
-- (perform a complex spectral shift)

complex_ncom      :: RealFloat a => a -- ^ w
		  -> a -- ^ phi
		  -> [ Complex a ] -- ^ x
		  -> [ Complex a ] -- ^ y

complex_ncom _  _   [] = []
complex_ncom wn phi x  = zipWith (*) (quadrature_nco wn phi) x

-- quadrature_mults returns the sum of the real parts and the imagimary
-- parts of two complex numbers (dot product)

quadrature_mult                 :: RealFloat a => Complex a -> Complex a -> a
quadrature_mult (x1:+y1) (x2:+y2) = x1 * x2 + y1 * y2

-- | 'quadrature_ncom' mixes the complex input x with a quadrature nco with
-- normalized frequency wn radians\/sample in quadrature (I\/Q modulation)

quadrature_ncom :: RealFloat a => a -- ^ w
		-> a -- ^ phi
		-> [Complex a] -- ^ x
		-> [a] -- ^ y

quadrature_ncom wn phi x = zipWith quadrature_mult x (quadrature_nco wn phi)
