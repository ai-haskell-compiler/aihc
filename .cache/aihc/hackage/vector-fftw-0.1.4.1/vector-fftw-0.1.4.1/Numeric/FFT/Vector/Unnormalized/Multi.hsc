{- |
Raw, unnormalized multi-dimensional versions of the transforms in @fftw@.

Note that the forwards and backwards transforms of this module are not actually
inverses.  For example, @run idft (run dft v) /= v@ in general.

For more information on the individual transforms, see
<http://www.fftw.org/fftw3_doc/What-FFTW-Really-Computes.html>.

@since 0.2
-}

module Numeric.FFT.Vector.Unnormalized.Multi
  (
        -- * Creating and executing 'Plan's
        run,
        plan,
        execute,
        -- * Complex-to-complex transforms
        dft,
        idft,
        -- * Real-to-complex transforms
        dftR2C,
        dftC2R,
  ) where

import Numeric.FFT.Vector.Base
import Foreign
import Foreign.C
import Data.Complex

#include <fftw3.h>

-- | Whether the complex fft is forwards or backwards.
type CDirection = CInt

foreign import ccall unsafe fftw_plan_dft
    :: CInt -> Ptr CInt -> Ptr (Complex Double) -> Ptr (Complex Double)
        -> CDirection -> CFlags -> IO (Ptr CPlan)

foreign import ccall unsafe fftw_plan_dft_r2c
    :: CInt -> Ptr CInt -> Ptr Double -> Ptr (Complex Double) -> CFlags
        -> IO (Ptr CPlan)

foreign import ccall unsafe fftw_plan_dft_c2r
    :: CInt -> Ptr CInt -> Ptr (Complex Double) -> Ptr Double -> CFlags
        -> IO (Ptr CPlan)

dftND :: CDirection -> TransformND (Complex Double) (Complex Double)
dftND d = TransformND
  { inputSizeND = id
  , outputSizeND = id
  , creationSizeFromInputND = id
  , makePlanND = \rk dims a b -> withPlanner . fftw_plan_dft rk dims a b d
  , normalizationND = const id
  }

-- | A forward discrete Fourier transform.  The output and input sizes are the same (@n@).
dft :: TransformND (Complex Double) (Complex Double)
dft = dftND (#const FFTW_FORWARD)

-- | A backward discrete Fourier transform.  The output and input sizes are the same (@n@).
idft :: TransformND (Complex Double) (Complex Double)
idft = dftND (#const FFTW_BACKWARD)

-- | A forward discrete Fourier transform with real data.  If the input size is @n0 * ... * nk@,
-- the output size will be @n0 * ... * nk \`div\` 2 + 1@.
dftR2C :: TransformND Double (Complex Double)
dftR2C = TransformND {
              inputSizeND = id,
              outputSizeND = \n -> n `div` 2 + 1,
              creationSizeFromInputND = id,
              makePlanND = \rk dims a b -> withPlanner . fftw_plan_dft_r2c rk dims a b,
              normalizationND = const id
          }

-- | A backward discrete Fourier transform which produces real data.
--
-- This 'Transform' behaves differently than the others:
--
--  - Calling @plan dftC2R n@ creates a 'Plan' whose /output/ size is @n@, and whose
--    /input/ size is @n \`div\` 2 + 1@.
--
--  - If @length v == n@, then @length (run dftC2R v) == 2*(n-1)@.
dftC2R :: TransformND (Complex Double) Double
dftC2R = TransformND {
            inputSizeND = \n -> n `div` 2 + 1,
            outputSizeND = id,
            creationSizeFromInputND = \n -> 2 * (n-1),
            makePlanND = \rk dims a b -> withPlanner . fftw_plan_dft_c2r rk dims a b,
            normalizationND = const id
        }
