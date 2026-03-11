{- |
Raw, unnormalized versions of the transforms in @fftw@.

Note that the forwards and backwards transforms of this module are not actually
inverses.  For example, @run idft (run dft v) /= v@ in general.

For more information on the individual transforms, see
<http://www.fftw.org/fftw3_doc/What-FFTW-Really-Computes.html>.
-}
module Numeric.FFT.Vector.Unnormalized(
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
                    -- * Real-to-real transforms
                    -- $dct_size
                    -- ** Discrete cosine transforms
                    dct1,
                    dct2,
                    dct3,
                    dct4,
                    -- ** Discrete sine transforms
                    dst1,
                    dst2,
                    dst3,
                    dst4,
                    ) where

import Numeric.FFT.Vector.Base
import Foreign
import Foreign.C
import Data.Complex

#include <fftw3.h>

-- | Whether the complex fft is forwards or backwards.
type CDirection = CInt

-- | The type of the cosine or sine transform.
type CKind = (#type fftw_r2r_kind)

foreign import ccall unsafe fftw_plan_dft_1d
    :: CInt -> Ptr (Complex Double) -> Ptr (Complex Double) -> CDirection
        -> CFlags -> IO (Ptr CPlan)

foreign import ccall unsafe fftw_plan_dft_r2c_1d
    :: CInt -> Ptr Double -> Ptr (Complex Double) -> CFlags -> IO (Ptr CPlan)

foreign import ccall unsafe fftw_plan_dft_c2r_1d
    :: CInt -> Ptr (Complex Double) -> Ptr Double -> CFlags -> IO (Ptr CPlan)

foreign import ccall unsafe fftw_plan_r2r_1d
    :: CInt -> Ptr Double -> Ptr Double -> CKind -> CFlags -> IO (Ptr CPlan)

dft1D :: CDirection -> Transform (Complex Double) (Complex Double)
dft1D d = Transform {
            inputSize = id,
            outputSize = id,
            creationSizeFromInput = id,
            makePlan = \n a b -> withPlanner . fftw_plan_dft_1d n a b d,
            normalization = const id
            }

-- | A forward discrete Fourier transform.  The output and input sizes are the same (@n@).
--
-- @y_k = sum_(j=0)^(n-1) x_j e^(-2pi i j k/n)@
dft :: Transform (Complex Double) (Complex Double)
dft = dft1D (#const FFTW_FORWARD)

-- | A backward discrete Fourier transform.  The output and input sizes are the same (@n@).
--
-- @y_k = sum_(j=0)^(n-1) x_j e^(2pi i j k/n)@
idft :: Transform (Complex Double) (Complex Double)
idft = dft1D (#const FFTW_BACKWARD)

-- | A forward discrete Fourier transform with real data.  If the input size is @n@,
-- the output size will be @n \`div\` 2 + 1@.
dftR2C :: Transform Double (Complex Double)
dftR2C = Transform {
            inputSize = id,
            outputSize = \n -> n `div` 2 + 1,
            creationSizeFromInput = id,
            makePlan = \n a b -> withPlanner . fftw_plan_dft_r2c_1d n a b,
            normalization = const id
        }

-- | A backward discrete Fourier transform which produces real data.
--
-- This 'Transform' behaves differently than the others:
--
--  - Calling @plan dftC2R n@ creates a 'Plan' whose /output/ size is @n@, and whose
--    /input/ size is @n \`div\` 2 + 1@.
--
--  - If @length v == n@, then @length (run dftC2R v) == 2*(n-1)@.
dftC2R :: Transform (Complex Double) Double
dftC2R = Transform {
            inputSize = \n -> n `div` 2 + 1,
            outputSize = id,
            creationSizeFromInput = \n -> 2 * (n-1),
            makePlan = \n a b -> withPlanner . fftw_plan_dft_c2r_1d n a b,
            normalization = const id
        }


r2rTransform :: CKind -> Transform Double Double
r2rTransform kind = Transform {
                    inputSize = id,
                    outputSize = id,
                    creationSizeFromInput = id,
                    makePlan = \n a b -> withPlanner . fftw_plan_r2r_1d n a b kind,
                    normalization = const id
                }

-- $dct_size
-- The real-even (DCT) and real-odd (DST) transforms.  The input and output sizes
-- are the same (@n@).

-- | A type-1 discrete cosine transform.
--
-- @y_k = x_0 + (-1)^k x_(n-1) + 2 sum_(j=1)^(n-2) x_j cos(pi j k\/(n-1))@
dct1 :: Transform Double Double
dct1 = r2rTransform (#const  FFTW_REDFT00)

-- | A type-2 discrete cosine transform.
--
-- @y_k = 2 sum_(j=0)^(n-1) x_j cos(pi(j+1\/2)k\/n)@
dct2 :: Transform Double Double
dct2 = r2rTransform (#const  FFTW_REDFT10)

-- | A type-3 discrete cosine transform.
--
-- @y_k = x_0 + 2 sum_(j=1)^(n-1) x_j cos(pi j(k+1\/2)\/n)@
dct3 :: Transform Double Double
dct3 = r2rTransform (#const  FFTW_REDFT01)

-- | A type-4 discrete cosine transform.
--
-- @y_k = 2 sum_(j=0)^(n-1) x_j cos(pi(j+1\/2)(k+1\/2)\/n)@
dct4 :: Transform Double Double
dct4 = r2rTransform (#const  FFTW_REDFT11)

-- | A type-1 discrete sine transform.
--
-- @y_k = 2 sum_(j=0)^(n-1) x_j sin(pi(j+1)(k+1)\/(n+1))@
dst1 :: Transform Double Double
dst1 = r2rTransform (#const  FFTW_RODFT00)

-- | A type-2 discrete sine transform.
--
-- @y_k = 2 sum_(j=0)^(n-1) x_j sin(pi(j+1\/2)(k+1)\/n)@
dst2 :: Transform Double Double
dst2 = r2rTransform (#const  FFTW_RODFT10)

-- | A type-3 discrete sine transform.
--
-- @y_k = (-1)^k x_(n-1) + 2 sum_(j=0)^(n-2) x_j sin(pi(j+1)(k+1\/2)/n)@
dst3 :: Transform Double Double
dst3 = r2rTransform (#const  FFTW_RODFT01)

-- | A type-4 discrete sine transform.
--
-- @y_k = sum_(j=0)^(n-1) x_j sin(pi(j+1\/2)(k+1\/2)\/n)@
dst4 :: Transform Double Double
dst4 = r2rTransform (#const FFTW_RODFT11)
