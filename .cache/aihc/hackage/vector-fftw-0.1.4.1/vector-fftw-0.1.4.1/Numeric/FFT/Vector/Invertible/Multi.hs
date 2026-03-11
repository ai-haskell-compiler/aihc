{- |
This module provides  normalized multi-dimensional versions of the transforms in @fftw@.

The forwards transforms in this module are identical to those in "Numeric.FFT.Vector.Unnormalized".
The backwards transforms are normalized to be their inverse operations (approximately, due to floating point precision).

For more information on the underlying transforms, see
<http://www.fftw.org/fftw3_doc/What-FFTW-Really-Computes.html>.

@since 0.2
-}

module Numeric.FFT.Vector.Invertible.Multi
  (
        -- * Creating and executing 'Plan's
        run,
        plan,
        execute,
        -- * Complex-to-complex transforms
        U.dft,
        idft,
        -- * Real-to-complex transforms
        U.dftR2C,
        dftC2R,
  ) where

import Numeric.FFT.Vector.Base
import qualified Numeric.FFT.Vector.Unnormalized.Multi as U
import Data.Complex
import qualified Data.Vector.Storable as VS

-- | A backward discrete Fourier transform which is the inverse of 'U.dft'.  The output and input sizes are the same (@n@).
idft :: TransformND (Complex Double) (Complex Double)
idft = U.idft {normalizationND = \ns -> constMultOutput $ 1 / toEnum (VS.product ns)}

-- | A normalized backward discrete Fourier transform which is the left inverse of
-- 'U.dftR2C'.  (Specifically, @run dftC2R . run dftR2C == id@.)
--
-- This 'Transform' behaves differently than the others:
--
--  - Calling @planND dftC2R dims@ where @dims = [n0, ..., nk]@ creates a 'Plan' whose /output/ size is @dims@, and whose
--    /input/ size is @[n0, ..., nk \`div\` 2 + 1]@.
--
--  - If @length v == n0 * ... * nk@, then @length (run dftC2R v) == n0 * ... * 2*(nk-1)@.
--
dftC2R :: TransformND (Complex Double) Double
dftC2R = U.dftC2R {normalizationND = \ns -> constMultOutput $ 1 / toEnum (VS.product ns)}
