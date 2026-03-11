{- |
This module provides normalized versions of the transforms in @fftw@.

All of the transforms are normalized so that

 - Each transform is unitary, i.e., preserves the inner product and the sum-of-squares norm of its input.

 - Each backwards transform is the inverse of the corresponding forwards transform.

(Both conditions only hold approximately, due to floating point precision.)

For more information on the underlying transforms, see
<http://www.fftw.org/fftw3_doc/What-FFTW-Really-Computes.html>.
--
-- @since 0.2
-}

module Numeric.FFT.Vector.Unitary.Multi
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

import Control.Exception (assert)
import Control.Monad (forM_)
import Numeric.FFT.Vector.Base
import qualified Numeric.FFT.Vector.Unnormalized.Multi as U
import Data.Complex
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MS
import Control.Monad.Primitive(RealWorld)

-- | A discrete Fourier transform. The output and input sizes are the same (@n@).
--
-- @y_k = (1\/sqrt n) sum_(j=0)^(n-1) x_j e^(-2pi i j k\/n)@
dft :: TransformND (Complex Double) (Complex Double)
dft = U.dft {normalizationND = \ns -> constMultOutput $ 1 / sqrt (toEnum (VS.product ns))}

-- | An inverse discrete Fourier transform.  The output and input sizes are the same (@n@).
--
-- @y_k = (1\/sqrt n) sum_(j=0)^(n-1) x_j e^(2pi i j k\/n)@
idft :: TransformND (Complex Double) (Complex Double)
idft = U.idft {normalizationND = \ns -> constMultOutput $ 1 / sqrt (toEnum (VS.product ns))}

-- | A forward discrete Fourier transform with real data.  If the input size is @n@,
-- the output size will be @n \`div\` 2 + 1@.
dftR2C :: TransformND Double (Complex Double)
dftR2C = U.dftR2C {normalizationND = \ns -> modifyOutput $
                    complexR2CScaling (sqrt 2) ns (outputSizeND U.dftR2C $ VS.last ns)
        }

-- | A normalized backward discrete Fourier transform which is the left inverse of
-- 'U.dftR2C'.  (Specifically, @run dftC2R . run dftR2C == id@.)
--
-- This 'Transform' behaves differently than the others:
--
--  - Calling @plan dftC2R n@ creates a 'Plan' whose /output/ size is @n@, and whose
--    /input/ size is @n \`div\` 2 + 1@.
--
--  - If @length v == n@, then @length (run dftC2R v) == 2*(n-1)@.
--
dftC2R :: TransformND (Complex Double) Double
dftC2R = U.dftC2R {normalizationND = \ns -> modifyInput $
                    complexR2CScaling (sqrt 0.5) ns (inputSizeND U.dftC2R $ VS.last ns)
        }

complexR2CScaling :: Double -> VS.Vector Int -> Int -> MS.MVector RealWorld (Complex Double) -> IO ()
complexR2CScaling !t !ns !len !a = assert (MS.length a == VS.product (VS.init ns) * len) $ do
    let !s1 = sqrt (1/toEnum (VS.product ns))
    let !s2 = t * s1
    -- Justification for the use of unsafeModify:
    -- The output size is 2n+1; so if n>0 then the output size is >=1;
    -- and if n even then the output size is >=3.
    forM_ [0.. VS.product (VS.init ns) - 1] $ \idx -> do
      unsafeModify a (idx * len) $ scaleByD s1
      if odd (VS.last ns)
        then multC s2 (MS.unsafeSlice (idx * len + 1) (len-1) a)
        else do
            unsafeModify a (idx * len + len - 1) $ scaleByD s1
            multC s2 (MS.unsafeSlice (idx * len + 1) (len-2) a)

