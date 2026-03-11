-- $Id: FFTTest.hs,v 1.2 2003/04/11 21:57:04 donadio Exp donadio $

-- Ergun's method for testing FFT routines

-- borrowed from FFTW, orig reference is

-- Funda Ergun, "Testing multivariate linear functions: Overcoming the
-- generator bottleneck, Proc. 27th ACM Symposium on the Theory of
-- Computing, 407-416 (1995).

module Main (main) where

import Numeric.Random.Generator.MT19937
import Numeric.Random.Distribution.Uniform

import Numeric.Transform.Fourier.FFT (fft)

import DSP.Basic ((^!))

import System.Environment (getArgs)

import Data.Complex (Complex((:+)), cis)
import Data.Array (Array, Ix, listArray, elems, bounds, range, (!))


-- Generates random test vectors

gendata :: Int -> W -> Array Int (Complex Double)
gendata n s = listArray (0,n-1) $ zipWith (:+) (uniform53cc $ genrand s) (uniform53cc $ genrand (s+1))

-- A few functions over arrays

aadd, asub ::
   (Ix i, Num e) =>
   Array i e -> Array i e -> Array i e

aadd x y = listArray bnds [ x!i + y!i | i <- range bnds ]
    where bnds = bounds x

asub x y = listArray bnds [ x!i - y!i | i <- range bnds ]
    where bnds = bounds x

arot ::
   (Ix i, Num e) =>
   Array i e -> Array i e

arot xa =
   listArray (bounds xa) $
   case elems xa of
      [] -> []
      x:xs -> xs ++ [x]

ascale ::
   (Ix i, Num e) =>
   e -> Array i e -> Array i e
ascale a x = fmap (a*) x

-- linearity test: aFFT(x) + bFFT(y) == FFT(ax+by)

lin_test :: Int -> Double
lin_test n = acomp z1 z2
    where x = gendata n 42
	  y = gendata n 44
	  a = u !! 0 :+ u !! 1
	  b = u !! 2 :+ u !! 3
	  u = uniform53cc $ genrand 46
	  x' = ascale a $ fft x
	  y' = ascale b $ fft y
	  z1 = aadd x' y'
	  z2 = fft $ aadd (ascale a x) (ascale b y)

-- impulse response test: rect == FFT(x) + FFT(impulse - x)

imp_test :: Int -> Double
imp_test n = acomp a' (aadd b' c')
    where zeros = 0 : zeros
	  a = listArray (0,n-1) $ (1 :+ 0) : zeros
	  b = gendata n 42
	  c = asub a b
	  a' = listArray (0,n-1) $ replicate n (1 :+ 0)
	  b' = fft b
	  c' = fft c

-- shift test: x[n-m] <-> W_N^km X[k]

shift_test :: Int -> Double
shift_test n = acomp a' c'
    where a = gendata n 42
	  b = arot a
	  a' = fft a
	  b' = fft b
	  c' = listArray (0,n-1) $ [ b'!i * cis (-2 * pi * fromIntegral i / fromIntegral n) | i <- [0..n-1] ]

-- determines peak error (from FFTW)

acomp ::
   (Ix i, RealFloat a) =>
   Array i (Complex a) -> Array i (Complex a) -> a

acomp x y = (maximum $ zipWith (/) a mag)
    where a = zipWith calc_a (elems x) (elems y)
	  mag = zipWith calc_mag (elems x) (elems y)
	  calc_a (xr:+xi) (yr:+yi) = sqrt $ (xr - yr)^!2 + (xi - yi)^!2
	  calc_mag (xr:+xi) (yr:+yi) = 0.5 * (sqrt (xr^!2+xi^!2) + sqrt (yr^!2+yi^!2)) + tol
          tol = 1.0e-6


--glue it all together

test1fft :: Int -> IO ()
test1fft n = do putStr $ show n ++ ":\t"
		putStr $ if ok then "OK\n" else "ERROR\n"
    where ok = lin_test n < tol && imp_test n < tol && shift_test n < tol
          tol = 1.0e-6

testfft :: Int -> Int -> IO ()
testfft n1 n2 = mapM_ test1fft [n1..n2]

main :: IO ()
main = do args <- getArgs
	  testfft (read $ args !! 0) (read $ args !! 1)
